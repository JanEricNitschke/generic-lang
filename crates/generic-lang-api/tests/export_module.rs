//! End-to-end test of the `export_module!` glue: descriptor generation and
//! the panic/error mapping of the generated wrappers, driven through a mock
//! host vtable.

// The mock callbacks must match the ABI's fn-pointer types exactly.
#![allow(clippy::missing_const_for_fn)]

use core::ffi::c_void;
use core::mem::MaybeUninit;
use std::cell::RefCell;

use generic_lang_api::{
    FfiReturn, FfiStatus, FfiStr, GENERIC_PLUGIN_ABI_VERSION, GenericValue, Host, HostApi,
    PluginError, ValueKind,
};

/// A blob carrying `first` in its first opaque limb (the rest zeroed) — the
/// tests build values this way to check bytes survive the FFI glue.
fn blob(first: u64) -> GenericValue {
    GenericValue {
        opaque: [
            MaybeUninit::new(first),
            MaybeUninit::new(0),
            MaybeUninit::new(0),
            MaybeUninit::new(0),
        ],
    }
}

/// Read the first opaque limb of a blob built by [`blob`].
///
/// # Safety
///
/// The first limb must be initialized — it is for anything from `blob` or a
/// mock echoing such a value.
unsafe fn limb0(value: GenericValue) -> u64 {
    // SAFETY: guaranteed by the caller.
    unsafe { value.opaque[0].assume_init() }
}

fn add(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let (Some(a), Some(b)) = (host.as_int(args[0]), host.as_int(args[1])) else {
        return Err(host.type_error("add expects two integers"));
    };
    Ok(host.make_int(a + b))
}

fn fail(host: &mut Host, _args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    Err(host.index_error("out of bounds"))
}

fn explode(_host: &mut Host, _args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    panic!("kaboom");
}

fn forward_fatal(_host: &mut Host, _args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    Err(PluginError::Fatal)
}

fn explode_any(_host: &mut Host, _args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    std::panic::panic_any(42);
}

generic_lang_api::export_module![
    ("add", &[2], add),
    ("fail", &[0], fail),
    ("explode", &[0], explode),
    ("forward_fatal", &[0], forward_fatal),
    ("explode_any", &[0], explode_any),
];

// --- mock host ---

thread_local! {
    /// Message strings interned through the mock `string_new`.
    static INTERNED: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
}

// --- Mocks with real behavior: they read arguments, encode values, or
// intern strings, so the tests can check data survives the FFI glue. ---

// `int_get`/`int_new` carry integers in the first opaque limb, so the
// tests verify values travel through the glue byte-exact.
#[allow(clippy::cast_possible_wrap)]
extern "C" fn mock_int_get(_ctx: *mut c_void, value: GenericValue, out: *mut i64) -> bool {
    // SAFETY: the out-pointer is valid, and the first limb is initialized
    // for the values the tests build with `blob` and pass in.
    unsafe { *out = limb0(value) as i64 };
    true
}
#[allow(clippy::cast_sign_loss)]
extern "C" fn mock_int_new(_ctx: *mut c_void, value: i64) -> GenericValue {
    blob(value as u64)
}
extern "C" fn mock_string_new(_ctx: *mut c_void, value: FfiStr) -> FfiReturn {
    // SAFETY: callers pass valid UTF-8 of the given length.
    let s = unsafe {
        core::str::from_utf8_unchecked(core::slice::from_raw_parts(value.ptr, value.len))
    };
    INTERNED.with_borrow_mut(|strings| strings.push(s.to_owned()));
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
// `builtin_get`/`exception_new` intern class names and messages, and hand
// back values echoing the class handle, so the tests can check both travel
// through the glue.
extern "C" fn mock_builtin_get(_ctx: *mut c_void, name: FfiStr) -> FfiReturn {
    // SAFETY: callers pass valid UTF-8 of the given length.
    let s =
        unsafe { core::str::from_utf8_unchecked(core::slice::from_raw_parts(name.ptr, name.len)) };
    INTERNED.with_borrow_mut(|strings| strings.push(s.to_owned()));
    // A fake class handle tagged by the name's length.
    FfiReturn {
        status: 0,
        value: blob(name.len as u64),
    }
}
extern "C" fn mock_exception_new(
    _ctx: *mut c_void,
    class: GenericValue,
    message: FfiStr,
) -> FfiReturn {
    // SAFETY: callers pass valid UTF-8 of the given length.
    let s = unsafe {
        core::str::from_utf8_unchecked(core::slice::from_raw_parts(message.ptr, message.len))
    };
    INTERNED.with_borrow_mut(|strings| strings.push(s.to_owned()));
    // Echo the class handle so the test can see which class was used.
    FfiReturn {
        status: 0,
        value: class,
    }
}

// --- Inert stubs: return a fixed placeholder regardless of input, present
// only to fill out the vtable. ---

extern "C" fn mock_value_kind(_ctx: *mut c_void, _value: GenericValue) -> u32 {
    ValueKind::Int as u32
}
extern "C" fn mock_bool_get(_ctx: *mut c_void, _value: GenericValue, _out: *mut bool) -> bool {
    false
}
extern "C" fn mock_float_get(_ctx: *mut c_void, _value: GenericValue, _out: *mut f64) -> bool {
    false
}
extern "C" fn mock_string_get(_ctx: *mut c_void, _value: GenericValue, _out: *mut FfiStr) -> bool {
    false
}
extern "C" fn mock_list_len(_ctx: *mut c_void, _value: GenericValue, _out: *mut usize) -> bool {
    false
}
extern "C" fn mock_list_get(_ctx: *mut c_void, _value: GenericValue, _index: usize) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
extern "C" fn mock_nil_new(_ctx: *mut c_void) -> GenericValue {
    blob(0)
}
extern "C" fn mock_bool_new(_ctx: *mut c_void, _value: bool) -> GenericValue {
    blob(0)
}
extern "C" fn mock_float_new(_ctx: *mut c_void, _value: f64) -> GenericValue {
    blob(0)
}
extern "C" fn mock_list_new(_ctx: *mut c_void) -> GenericValue {
    blob(0)
}
extern "C" fn mock_list_push(
    _ctx: *mut c_void,
    _list: GenericValue,
    _item: GenericValue,
) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
extern "C" fn mock_value_display(_ctx: *mut c_void, _value: GenericValue) -> GenericValue {
    blob(0)
}
extern "C" fn mock_call_value(
    _ctx: *mut c_void,
    _callee: GenericValue,
    _args: *const GenericValue,
    _nargs: usize,
) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
extern "C" fn mock_invoke_method(
    _ctx: *mut c_void,
    _receiver: GenericValue,
    _name: FfiStr,
    _args: *const GenericValue,
    _nargs: usize,
) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
extern "C" fn mock_value_str(_ctx: *mut c_void, _value: GenericValue) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
extern "C" fn mock_root(_ctx: *mut c_void, _value: GenericValue) {}
extern "C" fn mock_unroot(_ctx: *mut c_void, _n: usize) {}
extern "C" fn mock_tuple_len(_ctx: *mut c_void, _value: GenericValue, _out: *mut usize) -> bool {
    false
}
extern "C" fn mock_tuple_get(_ctx: *mut c_void, _value: GenericValue, _index: usize) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
extern "C" fn mock_container_len(
    _ctx: *mut c_void,
    _value: GenericValue,
    _out: *mut usize,
) -> bool {
    false
}
extern "C" fn mock_attr_get(
    _ctx: *mut c_void,
    _receiver: GenericValue,
    _name: FfiStr,
) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
extern "C" fn mock_attr_set(
    _ctx: *mut c_void,
    _receiver: GenericValue,
    _name: FfiStr,
    _value: GenericValue,
) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
extern "C" fn mock_attr_has(
    _ctx: *mut c_void,
    _receiver: GenericValue,
    _name: FfiStr,
) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
extern "C" fn mock_list_set(
    _ctx: *mut c_void,
    _list: GenericValue,
    _index: usize,
    _value: GenericValue,
) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
extern "C" fn mock_value_binary(
    _ctx: *mut c_void,
    _a: GenericValue,
    _b: GenericValue,
) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
extern "C" fn mock_dict_set(
    _ctx: *mut c_void,
    _dict: GenericValue,
    _key: GenericValue,
    _value: GenericValue,
) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
extern "C" fn mock_value_unary(_ctx: *mut c_void, _value: GenericValue) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}
extern "C" fn mock_is_instance(
    _ctx: *mut c_void,
    _value: GenericValue,
    _class: GenericValue,
) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: blob(0),
    }
}

fn mock_host_api() -> HostApi {
    HostApi {
        abi_version: GENERIC_PLUGIN_ABI_VERSION,
        ctx: core::ptr::null_mut(),
        value_kind: mock_value_kind,
        bool_get: mock_bool_get,
        int_get: mock_int_get,
        float_get: mock_float_get,
        string_get: mock_string_get,
        list_len: mock_list_len,
        list_get: mock_list_get,
        tuple_len: mock_tuple_len,
        tuple_get: mock_tuple_get,
        dict_len: mock_container_len,
        set_len: mock_container_len,
        builtin_get: mock_builtin_get,
        is_instance: mock_is_instance,
        attr_get: mock_attr_get,
        attr_set: mock_attr_set,
        attr_has: mock_attr_has,
        nil_new: mock_nil_new,
        bool_new: mock_bool_new,
        int_new: mock_int_new,
        float_new: mock_float_new,
        string_new: mock_string_new,
        list_new: mock_list_new,
        list_push: mock_list_push,
        list_set: mock_list_set,
        exception_new: mock_exception_new,
        value_display: mock_value_display,
        call_value: mock_call_value,
        invoke_method: mock_invoke_method,
        value_str: mock_value_str,
        dict_get: mock_value_binary,
        dict_set: mock_dict_set,
        dict_contains: mock_value_binary,
        set_add: mock_value_binary,
        set_contains: mock_value_binary,
        value_truthy: mock_value_unary,
        value_equals: mock_value_binary,
        value_hash: mock_value_unary,
        root: mock_root,
        unroot: mock_unroot,
    }
}

fn last_interned() -> String {
    INTERNED.with_borrow(|strings| strings.last().cloned().unwrap_or_default())
}

#[test]
fn descriptor_contents() {
    // Descriptor is non-null and init is idempotent
    let desc = generic_plugin_init();
    assert!(!desc.is_null());
    assert_eq!(desc, generic_plugin_init());

    // ABI version and function count are correct
    // SAFETY: generic_plugin_init returns a valid, leaked descriptor.
    let desc = unsafe { &*desc };
    assert_eq!(desc.abi_version, GENERIC_PLUGIN_ABI_VERSION);
    assert_eq!(desc.functions_len, 5);

    // The exported names, in declaration order.
    // SAFETY: the descriptor references a leaked slice of functions_len entries.
    let functions = unsafe { core::slice::from_raw_parts(desc.functions, desc.functions_len) };
    let names: Vec<&str> = functions
        .iter()
        .map(|f| {
            // SAFETY: names are leaked static strings.
            unsafe {
                core::str::from_utf8_unchecked(core::slice::from_raw_parts(f.name.ptr, f.name.len))
            }
        })
        .collect();
    assert_eq!(
        names,
        ["add", "fail", "explode", "forward_fatal", "explode_any"]
    );

    // Arities: `add` accepts exactly two arguments.
    // SAFETY: arities are leaked static slices.
    let arities =
        unsafe { core::slice::from_raw_parts(functions[0].arities, functions[0].arities_len) };
    assert_eq!(arities, [2]);

    // The macro always emits a non-null function pointer.
    assert!(functions.iter().all(|f| f.fun.is_some()));
}

fn call_exported(index: usize, args: &[GenericValue]) -> FfiReturn {
    // SAFETY: valid leaked descriptor, see descriptor_contents.
    let desc = unsafe { &*generic_plugin_init() };
    // SAFETY: see above.
    let functions = unsafe { core::slice::from_raw_parts(desc.functions, desc.functions_len) };
    let api = mock_host_api();
    let fun = functions[index]
        .fun
        .expect("export_module! emits non-null function pointers");
    fun(&raw const api, args.as_ptr(), args.len())
}

#[test]
fn ok_path() {
    // The mock encodes integers in the first opaque limb, so this checks
    // the arguments and the result travel through the glue byte-exact:
    // 19 + 23 must come back as 42.
    let args = [blob(19), blob(23)];
    let ret = call_exported(0, &args);
    assert_eq!(ret.status, 0);
    // SAFETY: `add` returns an int built via `blob`, so limb 0 is set.
    assert_eq!(unsafe { limb0(ret.value) }, 42);
}

#[test]
fn typed_error_path() {
    let ret = call_exported(1, &[]);
    // A typed error materializes as an exception instance of the class
    // looked up by name and built through `exception_new` (the mock class
    // handle carries the name's length; "IndexError" is 10 long).
    assert_eq!(ret.status, FfiStatus::Exception as u32);
    // SAFETY: the class handle is built via `blob`, so limb 0 is set.
    assert_eq!(unsafe { limb0(ret.value) }, "IndexError".len() as u64);
    let interned = INTERNED.with_borrow(Clone::clone);
    assert_eq!(
        &interned[interned.len() - 2..],
        ["IndexError", "out of bounds"]
    );
}

/// `string_get` writing the null-pointer empty sentinel (allowed by the
/// `FfiStr` contract): `Host::as_str` must hand back `""` instead of
/// building a slice from a null pointer.
extern "C" fn mock_string_get_null_empty(
    _ctx: *mut c_void,
    _value: GenericValue,
    out: *mut FfiStr,
) -> bool {
    // SAFETY: the out-pointer is valid (test-controlled).
    unsafe { *out = FfiStr::null() };
    true
}

#[test]
fn as_str_handles_the_null_empty_sentinel() {
    let mut api = mock_host_api();
    api.string_get = mock_string_get_null_empty;
    let host = Host::new(&api);
    assert_eq!(host.as_str(blob(0)), Some(""));
}

#[test]
fn panic_becomes_exception() {
    // Silence the default panic hook for the expected panic.
    let previous = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let ret = call_exported(2, &[]);
    std::panic::set_hook(previous);

    assert_eq!(ret.status, FfiStatus::Exception as u32);
    // SAFETY: the class handle is built via `blob`, so limb 0 is set.
    assert_eq!(unsafe { limb0(ret.value) }, "Exception".len() as u64);
    assert_eq!(last_interned(), "panic: kaboom");
}

#[test]
fn fatal_forwards_unchanged() {
    let ret = call_exported(3, &[]);
    assert_eq!(ret.status, FfiStatus::Fatal as u32);
    // Pin the wire value: 99 is part of the C ABI.
    assert_eq!(ret.status, 99);
}

#[test]
fn non_string_panic_payload_gets_the_fallback_message() {
    // Silence the default panic hook for the expected panic.
    let previous = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let ret = call_exported(4, &[]);
    std::panic::set_hook(previous);

    assert_eq!(ret.status, FfiStatus::Exception as u32);
    assert_eq!(last_interned(), "panic: plugin function panicked");
}

#[test]
fn null_args_with_zero_nargs_is_accepted() {
    // A C host may pass a null argument pointer for a zero-argument call;
    // the glue must not build a slice from it.
    let desc = unsafe { &*generic_plugin_init() };
    // SAFETY: valid leaked descriptor, see descriptor_contents.
    let functions = unsafe { core::slice::from_raw_parts(desc.functions, desc.functions_len) };
    let api = mock_host_api();
    let fun = functions[1]
        .fun
        .expect("export_module! emits non-null function pointers");
    let ret = fun(&raw const api, core::ptr::null(), 0);
    // `fail` still runs and produces its IndexError.
    assert_eq!(ret.status, FfiStatus::Exception as u32);
}

/// A host callback answering with a status outside the enum: the safe
/// wrapper must surface it as a protocol-violation exception, never
/// interpret `value`.
extern "C" fn mock_call_value_unknown_status(
    _ctx: *mut c_void,
    _callee: GenericValue,
    _args: *const GenericValue,
    _nargs: usize,
) -> FfiReturn {
    FfiReturn {
        status: 7,
        value: blob(0),
    }
}

#[test]
fn unknown_host_status_is_a_protocol_violation_exception() {
    let mut api = mock_host_api();
    api.call_value = mock_call_value_unknown_status;
    let mut host = Host::new(&api);
    let result = host.call(blob(0), &[]);
    assert!(matches!(result, Err(PluginError::Exception(_))));
    assert_eq!(last_interned(), "host callback returned unknown status 7");
}

/// A host answering `builtin_get` with an unknown status too — the very
/// callback the protocol-violation path uses to build its exception. Error
/// construction must bottom out at a nil-carrying exception instead of
/// recursing through itself until the stack overflows.
extern "C" fn mock_builtin_get_unknown_status(_ctx: *mut c_void, _name: FfiStr) -> FfiReturn {
    FfiReturn {
        status: 7,
        value: blob(0),
    }
}

#[test]
fn unknown_status_during_error_construction_does_not_recurse() {
    let mut api = mock_host_api();
    api.call_value = mock_call_value_unknown_status;
    api.builtin_get = mock_builtin_get_unknown_status;
    let mut host = Host::new(&api);
    let result = host.call(blob(0), &[]);
    assert!(matches!(result, Err(PluginError::Exception(_))));
}
