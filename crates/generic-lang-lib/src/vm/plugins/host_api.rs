//! The host vtable handed to plugins: value bridging and the 32 callbacks.
//!
//! Callbacks are grouped exactly like the `HostApi` declaration in
//! `generic-lang-api`: inspect / attributes / construct / display never
//! re-enter the interpreter (and allocation never collects, so they can
//! never trigger a GC); the re-entering group runs bytecode through the
//! `invoke_and_run_function` contract and may collect. Re-entering
//! callbacks root every value argument on the VM stack first — plugin
//! arguments are not otherwise reachable by the GC.
//!
//! Error discipline: host-side failures (wrong receiver kind, missing key,
//! …) are returned as `FfiReturn` codes with an interned message — they do
//! NOT create pending exceptions, since the plugin may swallow them. A
//! pending exception only exists while generic bytecode is unwinding; if
//! one escapes to a callback it is popped, the VM restored, and the
//! exception handed to the plugin as an `FfiReturn`. Fatal runtime errors
//! become [`FfiStatus::Fatal`], which stays uncatchable end to end.
//!
//! This file is the FFI boundary itself — nearly every function reads
//! through the opaque context or raw plugin-provided pointers.
#![allow(unsafe_code)]

use core::ffi::c_void;

use generic_lang_api::{
    FfiReturn, FfiStatus, FfiStr, GENERIC_PLUGIN_ABI_VERSION, GenericValue, HostApi, ValueKind,
};

use crate::heap::Heap;
use crate::value::{
    Dict, GenericInt, Instance, List, NativeClass, Number, Set, Value, get_native_class_id,
    is_exception_subclass, value_isinstance,
};
use crate::vm::errors::{VmErrorKind, VmResult};
use crate::vm::{ExceptionKind, VM};

// The opaque-blob bridge is a bit-copy of the whole `Value`; these asserts
// fail the build (not the runtime) if `Value` ever stops matching the ABI.
// Size and alignment must agree exactly: buffers of one type are read
// through pointers to the other in both directions.
const _: () = assert!(
    size_of::<Value>() == size_of::<GenericValue>(),
    "Value size drifted from the plugin ABI"
);
const _: () = assert!(
    align_of::<Value>() == align_of::<GenericValue>(),
    "Value alignment drifted from the plugin ABI"
);
// Bit-copies are handed to foreign code: Value must stay trivially
// copyable (no owning payloads).
const _: () = {
    const fn assert_copy<T: Copy>() {}
    assert_copy::<Value>();
};

/// Bit-copy a `Value` into the opaque FFI blob.
pub(super) fn to_ffi(value: Value) -> GenericValue {
    // SAFETY: same size (asserted above). `GenericValue`'s limbs are
    // `MaybeUninit`, so this copy never asserts that `Value`'s
    // uninitialized bytes (small variants leave most of the 32 unwritten)
    // are initialized — it is sound, not merely harmless in practice. The
    // plugin never inspects the bytes and only hands them back to `from_ffi`.
    unsafe { std::mem::transmute::<Value, GenericValue>(value) }
}

/// Reinterpret an opaque FFI blob as the `Value` it was created from.
///
/// Soundness rests on the trust model: plugins receive blobs from `to_ffi`
/// and must pass them back unmodified — fabricating or modifying one is
/// undefined behavior.
pub(super) fn from_ffi(value: GenericValue) -> Value {
    // SAFETY: `value` is a bit-copy of a real `Value` (see above).
    unsafe { std::mem::transmute::<GenericValue, Value>(value) }
}

/// Build the vtable for one plugin call. Cheap: a stack struct of fn
/// pointers plus the VM as the opaque context.
pub(super) fn build_host_api(vm: &mut VM) -> HostApi {
    HostApi {
        abi_version: GENERIC_PLUGIN_ABI_VERSION,
        ctx: std::ptr::from_mut(vm).cast::<c_void>(),
        value_kind: cb_value_kind,
        bool_get: cb_bool_get,
        int_get: cb_int_get,
        float_get: cb_float_get,
        string_get: cb_string_get,
        list_len: cb_list_len,
        list_get: cb_list_get,
        tuple_len: cb_tuple_len,
        tuple_get: cb_tuple_get,
        dict_len: cb_dict_len,
        set_len: cb_set_len,
        builtin_get: cb_builtin_get,
        is_instance: cb_is_instance,
        attr_get: cb_attr_get,
        attr_set: cb_attr_set,
        attr_has: cb_attr_has,
        nil_new: cb_nil_new,
        bool_new: cb_bool_new,
        int_new: cb_int_new,
        float_new: cb_float_new,
        string_new: cb_string_new,
        list_new: cb_list_new,
        list_push: cb_list_push,
        list_set: cb_list_set,
        exception_new: cb_exception_new,
        value_display: cb_value_display,
        call_value: cb_call_value,
        invoke_method: cb_invoke_method,
        value_str: cb_value_str,
        dict_get: cb_dict_get,
        dict_set: cb_dict_set,
        dict_contains: cb_dict_contains,
        set_add: cb_set_add,
        set_contains: cb_set_contains,
        value_truthy: cb_value_truthy,
        value_equals: cb_value_equals,
        value_hash: cb_value_hash,
        root: cb_root,
        unroot: cb_unroot,
    }
}

/// Reborrow the VM from the opaque context pointer.
///
/// # Safety
///
/// `ctx` must be the pointer stored by [`build_host_api`], and the borrow
/// must be properly nested inside the plugin call (host callbacks are —
/// `call_plugin` does not touch the VM while the plugin runs).
unsafe fn vm_from_ctx<'a>(ctx: *mut c_void) -> &'a mut VM {
    // SAFETY: guaranteed by the caller, see above.
    unsafe { &mut *ctx.cast::<VM>() }
}

/// Decode a borrowed FFI string; `None` on null or invalid UTF-8.
fn str_from_ffi<'a>(s: FfiStr) -> Option<&'a str> {
    if s.ptr.is_null() {
        return None;
    }
    // SAFETY: a non-null `FfiStr` references `len` bytes valid for the
    // duration of the callback (ABI contract).
    let bytes = unsafe { std::slice::from_raw_parts(s.ptr, s.len) };
    std::str::from_utf8(bytes).ok()
}

fn ffi_ok(value: Value) -> FfiReturn {
    FfiReturn {
        status: FfiStatus::Ok as u32,
        value: to_ffi(value),
    }
}

/// An exception instance handed to the plugin with full identity: the
/// plugin can inspect it (`exception_kind`, `value_str`, `attr_get`), throw
/// it onward unchanged, or handle it. No pending exception is left behind.
fn ffi_exception(exception: Value) -> FfiReturn {
    FfiReturn {
        status: FfiStatus::Exception as u32,
        value: to_ffi(exception),
    }
}

/// A host-side error as an `FfiReturn`: a real exception instance, created
/// without raising — the plugin decides whether to throw it.
fn ffi_error(vm: &mut VM, kind: ExceptionKind, message: &str) -> FfiReturn {
    ffi_exception(vm.create_exception(kind.into(), message))
}

fn ffi_fatal() -> FfiReturn {
    FfiReturn {
        status: FfiStatus::Fatal as u32,
        value: to_ffi(Value::Nil),
    }
}

/// Run a re-entering dunder-style operation with `roots` kept alive on the
/// VM stack (plugin-held arguments are not otherwise GC-reachable). The
/// operation must follow the `invoke_and_run_function` contract: stack
/// restored on success, pending exception on top on `Err(Exception)`.
fn rooted_dunder<T>(
    vm: &mut VM,
    roots: &[Value],
    operation: impl FnOnce(&mut VM) -> VmResult<T>,
) -> Result<T, FfiReturn> {
    let start = vm.stack.len();
    vm.stack.extend_from_slice(roots);
    match operation(vm) {
        Ok(value) => {
            vm.stack.truncate(start);
            Ok(value)
        }
        Err(VmErrorKind::Exception(_)) => {
            let exception = vm.stack.pop().expect("Pending exception missing");
            vm.stack.truncate(start);
            Err(ffi_exception(exception))
        }
        Err(VmErrorKind::Runtime(_)) => {
            vm.stack.truncate(start);
            Err(ffi_fatal())
        }
    }
}

/// Enter the interpreter for `call_value`/`invoke_method`: push the callee
/// (or receiver) and arguments, dispatch, and — if the dispatch pushed a
/// frame instead of executing natively — run that callstack region to
/// completion. Mirrors `invoke_and_run_function`'s exception contract,
/// except the escaped exception is handed to the plugin instead of staying
/// pending.
fn reenter_call(
    vm: &mut VM,
    values: &[Value],
    dispatch: impl FnOnce(&mut VM) -> VmResult,
) -> FfiReturn {
    let entry = vm.current_region();
    let handlers_before = vm.exception_handlers.len();
    vm.stack.extend_from_slice(values);

    let result = match dispatch(vm) {
        Ok(_) if vm.callstack.len() > entry.frames => vm.run_function_from_depth(entry.frames + 1),
        other => other,
    };

    match result {
        Ok(_) => {
            debug_assert_eq!(
                vm.stack.len(),
                entry.stack + 1,
                "re-entering callback must leave exactly the result on the stack"
            );
            ffi_ok(vm.stack.pop().expect("Result missing after re-entry"))
        }
        Err(VmErrorKind::Exception(_)) => {
            // The run loop hands escapes over untruncated; clean the region
            // like `invoke_and_run_function` does, then hand the exception
            // to the plugin.
            let exception = vm.stack.pop().expect("Pending exception missing");
            vm.unwind_region(entry);
            ffi_exception(exception)
        }
        Err(VmErrorKind::Runtime(_)) => {
            vm.unwind_region(entry);
            // A fatal error skips handler resolution; drop any handlers
            // registered inside the region so a plugin that swallows the
            // Fatal status cannot leave stale handlers behind.
            vm.exception_handlers.truncate(handlers_before);
            ffi_fatal()
        }
    }
}

/// The FFI kind code of a value — one code per behavioral kind, decided by
/// the instance backing for native-backed instances. The exhaustive matches
/// (no wildcard) are the coverage guarantee: a new `Value` variant or
/// `NativeClass` backing fails to compile until it is mapped here.
pub(super) fn value_kind_of(heap: &Heap, value: Value) -> u32 {
    let kind = match value {
        Value::Nil => ValueKind::Nil,
        Value::Bool(_) => ValueKind::Bool,
        Value::StopIteration => ValueKind::StopIteration,
        Value::Number(Number::Float(_)) => ValueKind::Float,
        Value::Number(Number::Integer(GenericInt::Small(_))) => ValueKind::Int,
        Value::Number(Number::Integer(GenericInt::Big(id))) => {
            // `Int` promises `int_get` succeeds; oversized bigints are the
            // `BigInt` kind (display/str round-trips still work).
            if i64::try_from(id.to_value(heap)).is_ok() {
                ValueKind::Int
            } else {
                ValueKind::BigInt
            }
        }
        Value::Number(Number::Rational(_)) => ValueKind::Rational,
        Value::String(_) => ValueKind::String,
        Value::Closure(_) | Value::NativeFunction(_) | Value::BoundMethod(_) => ValueKind::Function,
        Value::Class(_) => ValueKind::Class,
        Value::Module(_) => ValueKind::Module,
        // Bare function objects, unbound native methods, and upvalues are
        // VM-internal — not callable via `call_value`.
        Value::Function(_) | Value::NativeMethod(_) | Value::Upvalue(_) => ValueKind::Other,
        Value::Instance(id) => match &id.to_value(heap).backing {
            None => ValueKind::Instance,
            Some(NativeClass::List(_)) => ValueKind::List,
            Some(NativeClass::Tuple(_)) => ValueKind::Tuple,
            Some(NativeClass::Dict(_)) => ValueKind::Dict,
            Some(NativeClass::Set(_)) => ValueKind::Set,
            Some(NativeClass::Range(_)) => ValueKind::Range,
            Some(NativeClass::Exception(_)) => ValueKind::Exception,
            Some(NativeClass::Generator(_)) => ValueKind::Generator,
            Some(
                NativeClass::ListIterator(_)
                | NativeClass::TupleIterator(_)
                | NativeClass::RangeIterator(_)
                | NativeClass::TemplateIterator(_),
            ) => ValueKind::Iterator,
            Some(
                NativeClass::Template(_)
                | NativeClass::Interpolation(_)
                | NativeClass::BoolProxy
                | NativeClass::StringProxy
                | NativeClass::IntegerProxy
                | NativeClass::FloatProxy
                | NativeClass::RationalProxy,
            ) => ValueKind::Other,
        },
    };
    kind as u32
}

// --- inspect (never re-enter) ---

extern "C" fn cb_value_kind(ctx: *mut c_void, value: GenericValue) -> u32 {
    // SAFETY: ctx is the VM pointer from build_host_api (all callbacks).
    let vm = unsafe { vm_from_ctx(ctx) };
    value_kind_of(&vm.heap, from_ffi(value))
}

extern "C" fn cb_bool_get(_ctx: *mut c_void, value: GenericValue, out: *mut bool) -> bool {
    if let Value::Bool(b) = from_ffi(value) {
        // SAFETY: the plugin passes a valid out-pointer (ABI contract).
        unsafe { *out = b };
        true
    } else {
        false
    }
}

extern "C" fn cb_int_get(ctx: *mut c_void, value: GenericValue, out: *mut i64) -> bool {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let n = match from_ffi(value) {
        Value::Number(Number::Integer(GenericInt::Small(n))) => n,
        Value::Number(Number::Integer(GenericInt::Big(id))) => {
            match i64::try_from(id.to_value(&vm.heap)) {
                Ok(n) => n,
                Err(_) => return false,
            }
        }
        _ => return false,
    };
    // SAFETY: valid out-pointer (ABI contract).
    unsafe { *out = n };
    true
}

extern "C" fn cb_float_get(_ctx: *mut c_void, value: GenericValue, out: *mut f64) -> bool {
    if let Value::Number(Number::Float(f)) = from_ffi(value) {
        // SAFETY: valid out-pointer (ABI contract).
        unsafe { *out = f };
        true
    } else {
        false
    }
}

extern "C" fn cb_string_get(ctx: *mut c_void, value: GenericValue, out: *mut FfiStr) -> bool {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    if let Value::String(id) = from_ffi(value) {
        // Interned string bytes are address-stable while the string lives;
        // the ABI limits the borrow to the next re-entering callback.
        let s: &String = id.to_value(&vm.heap);
        // SAFETY: valid out-pointer (ABI contract).
        unsafe {
            *out = FfiStr {
                ptr: s.as_ptr(),
                len: s.len(),
            };
        }
        true
    } else {
        false
    }
}

/// The backing of an instance value, if any.
fn backing_of(heap: &Heap, value: Value) -> Option<&NativeClass> {
    match value {
        Value::Instance(id) => id.to_value(heap).backing.as_ref(),
        _ => None,
    }
}

extern "C" fn cb_list_len(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    if let Some(NativeClass::List(list)) = backing_of(&vm.heap, from_ffi(value)) {
        // SAFETY: valid out-pointer (ABI contract).
        unsafe { *out = list.items.len() };
        true
    } else {
        false
    }
}

extern "C" fn cb_list_get(ctx: *mut c_void, value: GenericValue, index: usize) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let Some(NativeClass::List(list)) = backing_of(&vm.heap, from_ffi(value)) else {
        return ffi_error(vm, ExceptionKind::TypeError, "Target is not a list.");
    };
    if let Some(item) = list.items.get(index) {
        ffi_ok(*item)
    } else {
        let message = format!("Index {index} out of bounds ({}).", list.items.len());
        ffi_error(vm, ExceptionKind::IndexError, &message)
    }
}

extern "C" fn cb_tuple_len(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    if let Some(NativeClass::Tuple(tuple)) = backing_of(&vm.heap, from_ffi(value)) {
        // SAFETY: valid out-pointer (ABI contract).
        unsafe { *out = tuple.items().len() };
        true
    } else {
        false
    }
}

extern "C" fn cb_tuple_get(ctx: *mut c_void, value: GenericValue, index: usize) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let Some(NativeClass::Tuple(tuple)) = backing_of(&vm.heap, from_ffi(value)) else {
        return ffi_error(vm, ExceptionKind::TypeError, "Target is not a tuple.");
    };
    if let Some(item) = tuple.items().get(index) {
        ffi_ok(*item)
    } else {
        let message = format!("Index {index} out of bounds ({}).", tuple.items().len());
        ffi_error(vm, ExceptionKind::IndexError, &message)
    }
}

extern "C" fn cb_dict_len(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    if let Some(NativeClass::Dict(dict)) = backing_of(&vm.heap, from_ffi(value)) {
        // SAFETY: valid out-pointer (ABI contract).
        unsafe { *out = dict.items.len() };
        true
    } else {
        false
    }
}

extern "C" fn cb_builtin_get(ctx: *mut c_void, name: FfiStr) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let Some(name) = str_from_ffi(name) else {
        return ffi_error(
            vm,
            ExceptionKind::TypeError,
            "Builtin name must be non-null UTF-8.",
        );
    };
    let name_id = vm.heap.string_id(&name);
    if let Some(global) = vm.builtins.get(&name_id) {
        ffi_ok(global.value)
    } else {
        let message = format!("Undefined builtin '{name}'.");
        ffi_error(vm, ExceptionKind::NameError, &message)
    }
}

extern "C" fn cb_is_instance(
    ctx: *mut c_void,
    value: GenericValue,
    of_class: GenericValue,
) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let Value::Class(class_id) = from_ffi(of_class) else {
        return ffi_error(vm, ExceptionKind::TypeError, "Second value is not a class.");
    };
    ffi_ok(Value::Bool(value_isinstance(
        &vm.heap,
        from_ffi(value),
        class_id,
    )))
}

extern "C" fn cb_set_len(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    if let Some(NativeClass::Set(set)) = backing_of(&vm.heap, from_ffi(value)) {
        // SAFETY: valid out-pointer (ABI contract).
        unsafe { *out = set.items.len() };
        true
    } else {
        false
    }
}

// --- attributes (never re-enter; generic fields are plain map entries) ---

extern "C" fn cb_attr_get(ctx: *mut c_void, receiver: GenericValue, name: FfiStr) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let Some(name) = str_from_ffi(name) else {
        return ffi_error(
            vm,
            ExceptionKind::TypeError,
            "Attribute name must be non-null UTF-8.",
        );
    };
    let Value::Instance(id) = from_ffi(receiver) else {
        return ffi_error(
            vm,
            ExceptionKind::TypeError,
            "Only instances have attributes.",
        );
    };
    if let Some(value) = id.to_value(&vm.heap).fields.get(name).copied() {
        ffi_ok(value)
    } else {
        let message = format!("Undefined property '{name}'.");
        ffi_error(vm, ExceptionKind::AttributeError, &message)
    }
}

extern "C" fn cb_attr_set(
    ctx: *mut c_void,
    receiver: GenericValue,
    name: FfiStr,
    value: GenericValue,
) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let Some(name) = str_from_ffi(name) else {
        return ffi_error(
            vm,
            ExceptionKind::TypeError,
            "Attribute name must be non-null UTF-8.",
        );
    };
    let Value::Instance(id) = from_ffi(receiver) else {
        return ffi_error(
            vm,
            ExceptionKind::TypeError,
            "Only instances have attributes.",
        );
    };
    let name = name.to_owned();
    id.to_value_mut(&mut vm.heap)
        .fields
        .insert(name, from_ffi(value));
    ffi_ok(Value::Nil)
}

extern "C" fn cb_attr_has(ctx: *mut c_void, receiver: GenericValue, name: FfiStr) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let Some(name) = str_from_ffi(name) else {
        return ffi_error(
            vm,
            ExceptionKind::TypeError,
            "Attribute name must be non-null UTF-8.",
        );
    };
    let Value::Instance(id) = from_ffi(receiver) else {
        return ffi_error(
            vm,
            ExceptionKind::TypeError,
            "Only instances have attributes.",
        );
    };
    ffi_ok(Value::Bool(id.to_value(&vm.heap).fields.contains_key(name)))
}

// --- construct (never re-enter; allocation never collects) ---

extern "C" fn cb_nil_new(_ctx: *mut c_void) -> GenericValue {
    to_ffi(Value::Nil)
}

extern "C" fn cb_bool_new(_ctx: *mut c_void, value: bool) -> GenericValue {
    to_ffi(Value::Bool(value))
}

extern "C" fn cb_int_new(_ctx: *mut c_void, value: i64) -> GenericValue {
    to_ffi(value.into())
}

extern "C" fn cb_float_new(_ctx: *mut c_void, value: f64) -> GenericValue {
    to_ffi(value.into())
}

extern "C" fn cb_string_new(ctx: *mut c_void, value: FfiStr) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let Some(s) = str_from_ffi(value) else {
        return ffi_error(
            vm,
            ExceptionKind::ValueError,
            "String must be non-null UTF-8.",
        );
    };
    let id = vm.heap.string_id(&s);
    ffi_ok(Value::String(id))
}

extern "C" fn cb_list_new(ctx: *mut c_void) -> GenericValue {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let class = get_native_class_id(&vm.heap, "List");
    let instance = Instance::new(class, Some(List::new(vec![]).into()));
    to_ffi(vm.heap.add_instance(instance))
}

extern "C" fn cb_list_push(ctx: *mut c_void, list: GenericValue, item: GenericValue) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    if let Value::Instance(id) = from_ffi(list)
        && let Some(NativeClass::List(list)) = &mut id.to_value_mut(&mut vm.heap).backing
    {
        list.items.push(from_ffi(item));
        ffi_ok(Value::Nil)
    } else {
        ffi_error(vm, ExceptionKind::TypeError, "Target is not a list.")
    }
}

extern "C" fn cb_list_set(
    ctx: *mut c_void,
    list: GenericValue,
    index: usize,
    value: GenericValue,
) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let Value::Instance(id) = from_ffi(list) else {
        return ffi_error(vm, ExceptionKind::TypeError, "Target is not a list.");
    };
    let Some(NativeClass::List(list)) = &mut id.to_value_mut(&mut vm.heap).backing else {
        return ffi_error(vm, ExceptionKind::TypeError, "Target is not a list.");
    };
    let len = list.items.len();
    if let Some(slot) = list.items.get_mut(index) {
        *slot = from_ffi(value);
        ffi_ok(Value::Nil)
    } else {
        let message = format!("Index {index} out of bounds ({len}).");
        ffi_error(vm, ExceptionKind::IndexError, &message)
    }
}

extern "C" fn cb_exception_new(
    ctx: *mut c_void,
    of_class: GenericValue,
    message: FfiStr,
) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let Some(message) = str_from_ffi(message) else {
        return ffi_error(
            vm,
            ExceptionKind::TypeError,
            "Exception message must be non-null UTF-8.",
        );
    };
    let Value::Class(class_id) = from_ffi(of_class) else {
        return ffi_error(vm, ExceptionKind::TypeError, "Value is not a class.");
    };
    if !is_exception_subclass(&vm.heap, class_id) {
        return ffi_error(
            vm,
            ExceptionKind::TypeError,
            "Class does not derive from Exception.",
        );
    }
    let message_id = vm.heap.string_id(&message);
    let exception = vm.create_exception_with_class(class_id, Some(message_id));
    ffi_ok(exception)
}

// --- display (never re-enters) ---

extern "C" fn cb_value_display(ctx: *mut c_void, value: GenericValue) -> GenericValue {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let display = from_ffi(value).to_string(&vm.heap);
    let id = vm.heap.string_id(&display);
    to_ffi(Value::String(id))
}

// --- re-entering (run generic bytecode; GC may occur) ---

extern "C" fn cb_call_value(
    ctx: *mut c_void,
    callee: GenericValue,
    args: *const GenericValue,
    nargs: usize,
) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let Ok(arg_count) = u8::try_from(nargs) else {
        return ffi_error(
            vm,
            ExceptionKind::TypeError,
            "Too many arguments (max 255).",
        );
    };
    let callee = from_ffi(callee);
    let mut values = Vec::with_capacity(nargs + 1);
    values.push(callee);
    values.extend(args_slice(args, nargs).iter().copied());
    reenter_call(vm, &values, |vm| vm.call_value(callee, arg_count))
}

extern "C" fn cb_invoke_method(
    ctx: *mut c_void,
    receiver: GenericValue,
    name: FfiStr,
    args: *const GenericValue,
    nargs: usize,
) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let Some(name) = str_from_ffi(name) else {
        return ffi_error(
            vm,
            ExceptionKind::TypeError,
            "Method name must be non-null UTF-8.",
        );
    };
    let Ok(arg_count) = u8::try_from(nargs) else {
        return ffi_error(
            vm,
            ExceptionKind::TypeError,
            "Too many arguments (max 255).",
        );
    };
    let name_id = vm.heap.string_id(&name);
    let mut values = Vec::with_capacity(nargs + 1);
    values.push(from_ffi(receiver));
    values.extend(args_slice(args, nargs).iter().copied());
    reenter_call(vm, &values, |vm| vm.invoke(name_id, arg_count))
}

/// The argument buffer of a re-entering callback as a `Value` slice.
fn args_slice<'a>(args: *const GenericValue, nargs: usize) -> &'a [Value] {
    if nargs == 0 {
        &[]
    } else {
        // SAFETY: the plugin passes `nargs` contiguous values valid for the
        // call (ABI contract); `GenericValue` and `Value` share layout
        // (const asserts above).
        unsafe { std::slice::from_raw_parts(args.cast::<Value>(), nargs) }
    }
}

extern "C" fn cb_value_str(ctx: *mut c_void, value: GenericValue) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let value = from_ffi(value);
    match rooted_dunder(vm, &[value], |vm| vm.value_to_string(&value)) {
        Ok(id) => ffi_ok(Value::String(id)),
        Err(error) => error,
    }
}

extern "C" fn cb_dict_get(ctx: *mut c_void, dict: GenericValue, key: GenericValue) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let (dict, key) = (from_ffi(dict), from_ffi(key));
    if !matches!(backing_of(&vm.heap, dict), Some(NativeClass::Dict(_))) {
        return ffi_error(vm, ExceptionKind::TypeError, "Target is not a dict.");
    }
    match rooted_dunder(vm, &[dict, key], |vm| Dict::get(vm, &dict, key)) {
        Ok(Some(value)) => ffi_ok(value),
        Ok(None) => {
            let message = format!("Key not found: {}", key.to_string(&vm.heap));
            ffi_error(vm, ExceptionKind::KeyError, &message)
        }
        Err(error) => error,
    }
}

extern "C" fn cb_dict_set(
    ctx: *mut c_void,
    dict: GenericValue,
    key: GenericValue,
    value: GenericValue,
) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let (dict, key, value) = (from_ffi(dict), from_ffi(key), from_ffi(value));
    if !matches!(backing_of(&vm.heap, dict), Some(NativeClass::Dict(_))) {
        return ffi_error(vm, ExceptionKind::TypeError, "Target is not a dict.");
    }
    match rooted_dunder(vm, &[dict, key, value], |vm| {
        Dict::add(vm, &dict, key, value)
    }) {
        Ok(_) => ffi_ok(Value::Nil),
        Err(error) => error,
    }
}

extern "C" fn cb_dict_contains(
    ctx: *mut c_void,
    dict: GenericValue,
    key: GenericValue,
) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let (dict, key) = (from_ffi(dict), from_ffi(key));
    if !matches!(backing_of(&vm.heap, dict), Some(NativeClass::Dict(_))) {
        return ffi_error(vm, ExceptionKind::TypeError, "Target is not a dict.");
    }
    match rooted_dunder(vm, &[dict, key], |vm| Dict::contains(vm, &dict, key)) {
        Ok(contained) => ffi_ok(Value::Bool(contained)),
        Err(error) => error,
    }
}

extern "C" fn cb_set_add(ctx: *mut c_void, set: GenericValue, item: GenericValue) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let (set, item) = (from_ffi(set), from_ffi(item));
    if !matches!(backing_of(&vm.heap, set), Some(NativeClass::Set(_))) {
        return ffi_error(vm, ExceptionKind::TypeError, "Target is not a set.");
    }
    match rooted_dunder(vm, &[set, item], |vm| Set::add(vm, &set, item)) {
        Ok(_) => ffi_ok(Value::Nil),
        Err(error) => error,
    }
}

extern "C" fn cb_set_contains(
    ctx: *mut c_void,
    set: GenericValue,
    item: GenericValue,
) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let (set, item) = (from_ffi(set), from_ffi(item));
    if !matches!(backing_of(&vm.heap, set), Some(NativeClass::Set(_))) {
        return ffi_error(vm, ExceptionKind::TypeError, "Target is not a set.");
    }
    match rooted_dunder(vm, &[set, item], |vm| Set::contains(vm, &set, item)) {
        Ok(contained) => ffi_ok(Value::Bool(contained)),
        Err(error) => error,
    }
}

extern "C" fn cb_value_truthy(ctx: *mut c_void, value: GenericValue) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let value = from_ffi(value);
    match rooted_dunder(vm, &[value], |vm| vm.is_falsey(value)) {
        Ok(falsey) => ffi_ok(Value::Bool(!falsey)),
        Err(error) => error,
    }
}

extern "C" fn cb_value_equals(ctx: *mut c_void, a: GenericValue, b: GenericValue) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let (a, b) = (from_ffi(a), from_ffi(b));
    match rooted_dunder(vm, &[a, b], |vm| vm.compare_values(a, b)) {
        Ok(equal) => ffi_ok(Value::Bool(equal)),
        Err(error) => error,
    }
}

extern "C" fn cb_value_hash(ctx: *mut c_void, value: GenericValue) -> FfiReturn {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let value = from_ffi(value);
    match rooted_dunder(vm, &[value], |vm| vm.compute_hash(value)) {
        #[allow(clippy::cast_possible_wrap)]
        Ok(hash) => ffi_ok(Value::from(hash as i64)),
        Err(error) => error,
    }
}

// --- rooting (never re-enter) ---

extern "C" fn cb_root(ctx: *mut c_void, value: GenericValue) {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    vm.stack.push(from_ffi(value));
}

extern "C" fn cb_unroot(ctx: *mut c_void, n: usize) {
    // SAFETY: ctx per build_host_api.
    let vm = unsafe { vm_from_ctx(ctx) };
    let len = vm.stack.len().saturating_sub(n);
    vm.stack.truncate(len);
}
