//! The plugin trampoline and the plugin-call entry point.
//!
//! Plugin functions are registered as ordinary natives whose `fun` pointer
//! is [`plugin_trampoline`] and whose `plugin_fn` field holds the real
//! `extern "C"` pointer. The trampoline recovers that pointer from the
//! callee itself, which the dispatch site keeps on the VM stack directly
//! below the copied arguments - so `NativeFunctionImpl`, the dispatch
//! sites, and the number of loadable plugin functions are all unconstrained.
//!
//! This is where the host calls *into* plugin code, which is `unsafe` for
//! the obvious reason - the callee is foreign, and the trust for it was
//! established at `dlopen`. Each of the three call sites below builds the
//! vtable, hands over a live argument buffer, and decodes the returned blob;
//! those obligations are what its `SAFETY` comment discharges.

use generic_lang_api::{
    FfiReturn, FfiStatus, GenericValue, PluginFn, PluginMethodFn, PluginValueFn,
};

use super::host_api::{build_host_api, from_ffi, to_ffi};
use crate::heap::StringId;
use crate::value::Value;
use crate::vm::ExceptionKind::Exception;
use crate::vm::VM;
use crate::vm::errors::{RuntimeErrorKind, VmResult};

/// The `fun` of every plugin-backed `NativeFunction`: look the external
/// pointer back up on the callee and call it.
///
/// Relies on the dispatch-site layout: `execute_native_function_call`
/// copies the arguments off the stack and leaves the callee at
/// `stack[len - argc - 1]` until its post-call truncate - every path that
/// reaches it (`OP_CALL`, both `invoke` arms, the plugin `call_value`
/// callback) places the callee in that slot first.
#[allow(unsafe_code)]
pub(super) fn plugin_trampoline(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let callee = vm.stack[vm.stack.len() - args.len() - 1];
    let Value::NativeFunction(id) = callee else {
        unreachable!("plugin trampoline dispatched without its native function on the stack")
    };
    let native = id.to_value(&vm.heap);
    let fun = native
        .plugin_fn
        .expect("plugin trampoline on a native without a plugin function");
    let name = native.name;
    // SAFETY: `fun` came off a heap `NativeFunction` the loader built from a
    // validated descriptor, so it is a real plugin function; `args` is the live
    // dispatch-frame slice.
    unsafe { call_plugin(vm, fun, args, name) }
}

/// Call a plugin function with zero-copy arguments.
///
/// The `&[Value]` buffer lives in the dispatch frame (see
/// `execute_native_function_call`) and outlives the call, so its pointer is
/// handed to the plugin directly, cast to [`GenericValue`] (same size;
/// `Value`'s alignment satisfies `GenericValue`'s).
///
/// # Safety
///
/// `fun` must be a real plugin function from a loaded library, honoring
/// [`PluginFn`]'s contract - calling anything else is undefined behavior.
#[allow(unsafe_code)]
pub(super) unsafe fn call_plugin(
    vm: &mut VM,
    fun: PluginFn,
    args: &[Value],
    name: StringId,
) -> VmResult<Value> {
    let host = build_host_api(vm);
    // SAFETY: `PluginFn`'s contract. `host` is the vtable just built, live for
    // the whole call, and `args` is a live `&[Value]` slice reinterpreted as
    // `nargs` contiguous `GenericValue`s - same size, and `Value`'s alignment
    // is the stricter one. Calling the plugin itself is the trust established
    // at dlopen.
    let ret: FfiReturn = unsafe {
        fun(
            &raw const host,
            args.as_ptr().cast::<GenericValue>(),
            args.len(),
        )
    };
    // SAFETY: `ret` is what the plugin just returned.
    unsafe { map_plugin_return(vm, ret, name) }
}

/// Call a plugin value creator: no arguments, only the host vtable. Used
/// once per exported module value at import time.
///
/// # Safety
///
/// As [`call_plugin`], for a [`PluginValueFn`].
#[allow(unsafe_code)]
pub(super) unsafe fn call_plugin_value(
    vm: &mut VM,
    fun: PluginValueFn,
    name: StringId,
) -> VmResult<Value> {
    let host = build_host_api(vm);
    // SAFETY: `PluginValueFn`'s contract - as in `call_plugin`, minus the
    // arguments.
    let ret: FfiReturn = unsafe { fun(&raw const host) };
    // SAFETY: `ret` is what the plugin just returned.
    unsafe { map_plugin_return(vm, ret, name) }
}

/// Call a plugin method: the receiver is passed as a separate value (the C ABI
/// method convention), and `args` are the remaining arguments only.
///
/// GC rooting is via the VM stack - `execute_native_method_call` keeps the
/// receiver and args there for the whole call and truncates only afterward.
///
/// # Safety
///
/// As [`call_plugin`], for a [`PluginMethodFn`]; `receiver` must be a live
/// value the host owns.
#[allow(unsafe_code)]
pub(crate) unsafe fn call_plugin_method(
    vm: &mut VM,
    fun: PluginMethodFn,
    receiver: Value,
    args: &[Value],
    name: StringId,
) -> VmResult<Value> {
    let host = build_host_api(vm);
    // SAFETY: `PluginMethodFn`'s contract - as in `call_plugin`, with the
    // receiver handed over as a `to_ffi` blob of a real `Value`.
    let ret: FfiReturn = unsafe {
        fun(
            &raw const host,
            to_ffi(receiver),
            args.as_ptr().cast::<GenericValue>(),
            args.len(),
        )
    };
    // SAFETY: `ret` is what the plugin just returned.
    unsafe { map_plugin_return(vm, ret, name) }
}

/// Map a plugin's [`FfiReturn`] to the native calling convention (shared by
/// [`call_plugin`] and [`call_plugin_method`]):
/// - [`FfiStatus::Ok`] → the value (rooted by the dispatch site's push).
/// - [`FfiStatus::Exception`] → `value` is the exception *instance* to raise -
///   either created by the plugin via `exception_new` or one it caught from
///   a re-entering callback and rethrows, which preserves its class,
///   fields, and original stack trace.
/// - [`FfiStatus::Fatal`] → a fatal runtime error (uncatchable, forwarded from
///   a re-entering host callback).
/// - anything else is a plugin bug and becomes a base `Exception`.
///
/// # Safety
///
/// `ret` must be a return value a plugin produced, so that on the two statuses
/// that carry a value it is a blob the host issued - what [`from_ffi`] needs.
/// The other two never touch it.
#[allow(unsafe_code)]
unsafe fn map_plugin_return(vm: &mut VM, ret: FfiReturn, name: StringId) -> VmResult<Value> {
    match FfiStatus::from_u32(ret.status) {
        // SAFETY: by this function's contract `ret` came from a plugin, so an
        // `Ok` value is one this host issued.
        Some(FfiStatus::Ok) => Ok(unsafe { from_ffi(ret.value) }),
        Some(FfiStatus::Exception) => {
            // SAFETY: as above. `raise_pending_from_stack` then rejects any
            // value that is not an exception instance, but that check is about
            // semantics, not soundness.
            vm.stack.push(unsafe { from_ffi(ret.value) });
            // Validates the value (anything but an instance of an Exception
            // subclass becomes a TypeError) and attaches a stack trace only
            // if it has none - a rethrown exception keeps its original one.
            Err(vm
                .raise_pending_from_stack()
                .expect_err("raising is never a success"))
        }
        Some(FfiStatus::Fatal) => {
            // Fatal errors print at the site that produced them; a fatal the
            // plugin returned on its own has no such site in the host, so
            // name the function here (for a forwarded host fatal this adds
            // placement context to the line the original site printed).
            eprintln!(
                "Fatal error reported by plugin function `{}`.",
                *name.to_value(&vm.heap)
            );
            Err(RuntimeErrorKind.into())
        }
        // A protocol violation: `value` must not be interpreted at all.
        None => {
            let message = format!("Plugin returned unknown status {}.", ret.status);
            Err(vm.throw(Exception, &message).unwrap_err())
        }
    }
}
