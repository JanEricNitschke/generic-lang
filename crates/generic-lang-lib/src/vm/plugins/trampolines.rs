//! The plugin trampoline and the plugin-call entry point.
//!
//! Plugin functions are registered as ordinary natives whose `fun` pointer
//! is [`plugin_trampoline`] and whose `plugin_fn` field holds the real
//! `extern "C"` pointer. The trampoline recovers that pointer from the
//! callee itself, which the dispatch site keeps on the VM stack directly
//! below the copied arguments - so `NativeFunctionImpl`, the dispatch
//! sites, and the number of loadable plugin functions are all unconstrained.

use generic_lang_api::{FfiReturn, FfiStatus, GenericValue, PluginFn};

use super::host_api::{build_host_api, from_ffi};
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
    call_plugin(vm, fun, args, name)
}

/// Call a plugin function with zero-copy arguments.
///
/// The `&[Value]` buffer lives in the dispatch frame (see
/// `execute_native_function_call`) and outlives the call, so its pointer is
/// handed to the plugin directly, cast to the layout-asserted
/// [`GenericValue`]. The returned [`FfiReturn`] maps to the native calling
/// convention:
/// - [`FfiStatus::Ok`] → the value (rooted by the dispatch site's push).
/// - [`FfiStatus::Exception`] → `value` is the exception *instance* to raise -
///   either created by the plugin via `exception_new` or one it caught from
///   a re-entering callback and rethrows, which preserves its class,
///   fields, and original stack trace.
/// - [`FfiStatus::Fatal`] → a fatal runtime error (uncatchable, forwarded from
///   a re-entering host callback).
/// - anything else is a plugin bug and becomes a base `Exception`.
pub(super) fn call_plugin(
    vm: &mut VM,
    fun: PluginFn,
    args: &[Value],
    name: StringId,
) -> VmResult<Value> {
    let host = build_host_api(vm);
    let ret: FfiReturn = fun(
        &raw const host,
        args.as_ptr().cast::<GenericValue>(),
        args.len(),
    );
    match FfiStatus::from_u32(ret.status) {
        Some(FfiStatus::Ok) => Ok(from_ffi(ret.value)),
        Some(FfiStatus::Exception) => {
            vm.stack.push(from_ffi(ret.value));
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
