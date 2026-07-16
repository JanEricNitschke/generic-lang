//! End-to-end test of the `export_module!` glue: descriptor generation and
//! the panic/error mapping of the generated wrappers, driven through a mock
//! host vtable.

// The mock callbacks must match the ABI's fn-pointer types exactly.
#![allow(clippy::missing_const_for_fn)]

use core::ffi::c_void;
use std::cell::RefCell;

use generic_lang_api::{
    FfiReturn, FfiStr, GENERIC_PLUGIN_ABI_VERSION, GenericValue, Host, HostApi, PluginError,
    exception_code,
};

fn add(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let (Some(a), Some(b)) = (host.as_int(args[0]), host.as_int(args[1])) else {
        return Err(PluginError::type_error("add expects two integers"));
    };
    Ok(host.make_int(a + b))
}

fn fail(_host: &mut Host, _args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    Err(PluginError::index_error("out of bounds"))
}

fn explode(_host: &mut Host, _args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    panic!("kaboom");
}

generic_lang_api::export_module![
    ("add", &[2], add),
    ("fail", &[0], fail),
    ("explode", &[0], explode),
];

// --- mock host ---

thread_local! {
    /// Message strings interned through the mock `string_new`.
    static INTERNED: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
}

extern "C" fn mock_value_kind(_ctx: *mut c_void, _value: GenericValue) -> u32 {
    generic_lang_api::value_kind::INT
}
extern "C" fn mock_bool_get(_ctx: *mut c_void, _value: GenericValue, _out: *mut bool) -> bool {
    false
}
// The mock encodes integers in the first opaque limb, so the tests verify
// values travel through the FFI glue byte-exact.
#[allow(clippy::cast_possible_wrap)]
extern "C" fn mock_int_get(_ctx: *mut c_void, value: GenericValue, out: *mut i64) -> bool {
    // SAFETY: callers pass a valid out-pointer.
    unsafe { *out = value.opaque[0] as i64 };
    true
}
extern "C" fn mock_float_get(_ctx: *mut c_void, _value: GenericValue, _out: *mut f64) -> bool {
    false
}
extern "C" fn mock_string_get(_ctx: *mut c_void, _value: GenericValue) -> FfiStr {
    FfiStr::null()
}
extern "C" fn mock_list_len(_ctx: *mut c_void, _value: GenericValue, _out: *mut usize) -> bool {
    false
}
extern "C" fn mock_list_get(
    _ctx: *mut c_void,
    _value: GenericValue,
    _index: usize,
    _out: *mut GenericValue,
) -> bool {
    false
}
extern "C" fn mock_nil_new(_ctx: *mut c_void) -> GenericValue {
    GenericValue::zeroed()
}
extern "C" fn mock_bool_new(_ctx: *mut c_void, _value: bool) -> GenericValue {
    GenericValue::zeroed()
}
#[allow(clippy::cast_sign_loss)]
extern "C" fn mock_int_new(_ctx: *mut c_void, value: i64) -> GenericValue {
    GenericValue {
        opaque: [value as u64, 0, 0, 0],
    }
}
extern "C" fn mock_float_new(_ctx: *mut c_void, _value: f64) -> GenericValue {
    GenericValue::zeroed()
}
extern "C" fn mock_string_new(_ctx: *mut c_void, value: FfiStr, out: *mut GenericValue) -> bool {
    // SAFETY: callers pass valid UTF-8 of the given length and a valid
    // out-pointer.
    let s = unsafe {
        core::str::from_utf8_unchecked(core::slice::from_raw_parts(value.ptr, value.len))
    };
    INTERNED.with_borrow_mut(|strings| strings.push(s.to_owned()));
    // SAFETY: see above.
    unsafe { *out = GenericValue::zeroed() };
    true
}
extern "C" fn mock_list_new(_ctx: *mut c_void) -> GenericValue {
    GenericValue::zeroed()
}
extern "C" fn mock_list_push(_ctx: *mut c_void, _list: GenericValue, _item: GenericValue) -> bool {
    false
}
extern "C" fn mock_value_display(_ctx: *mut c_void, _value: GenericValue) -> GenericValue {
    GenericValue::zeroed()
}
extern "C" fn mock_call_value(
    _ctx: *mut c_void,
    _callee: GenericValue,
    _args: *const GenericValue,
    _nargs: usize,
) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: GenericValue::zeroed(),
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
        value: GenericValue::zeroed(),
    }
}
extern "C" fn mock_value_str(_ctx: *mut c_void, _value: GenericValue) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: GenericValue::zeroed(),
    }
}
extern "C" fn mock_root(_ctx: *mut c_void, _value: GenericValue) {}
extern "C" fn mock_unroot(_ctx: *mut c_void, _n: usize) {}
extern "C" fn mock_tuple_len(_ctx: *mut c_void, _value: GenericValue, _out: *mut usize) -> bool {
    false
}
extern "C" fn mock_tuple_get(
    _ctx: *mut c_void,
    _value: GenericValue,
    _index: usize,
    _out: *mut GenericValue,
) -> bool {
    false
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
        value: GenericValue::zeroed(),
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
        value: GenericValue::zeroed(),
    }
}
extern "C" fn mock_attr_has(
    _ctx: *mut c_void,
    _receiver: GenericValue,
    _name: FfiStr,
    _out: *mut bool,
) -> bool {
    false
}
extern "C" fn mock_list_set(
    _ctx: *mut c_void,
    _list: GenericValue,
    _index: usize,
    _value: GenericValue,
) -> bool {
    false
}
extern "C" fn mock_value_binary(
    _ctx: *mut c_void,
    _a: GenericValue,
    _b: GenericValue,
) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: GenericValue::zeroed(),
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
        value: GenericValue::zeroed(),
    }
}
extern "C" fn mock_value_unary(_ctx: *mut c_void, _value: GenericValue) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: GenericValue::zeroed(),
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
    let desc = generic_plugin_init();
    assert!(!desc.is_null());
    // Initialization happens exactly once; repeated calls return the same
    // leaked descriptor.
    assert_eq!(desc, generic_plugin_init());
    // SAFETY: generic_plugin_init returns a valid, leaked descriptor.
    let desc = unsafe { &*desc };
    assert_eq!(desc.abi_version, GENERIC_PLUGIN_ABI_VERSION);
    assert_eq!(desc.functions_len, 3);

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
    assert_eq!(names, ["add", "fail", "explode"]);
    // SAFETY: arities are leaked static slices.
    let arities =
        unsafe { core::slice::from_raw_parts(functions[0].arities, functions[0].arities_len) };
    assert_eq!(arities, [2]);
}

fn call_exported(index: usize, args: &[GenericValue]) -> FfiReturn {
    // SAFETY: valid leaked descriptor, see descriptor_contents.
    let desc = unsafe { &*generic_plugin_init() };
    // SAFETY: see above.
    let functions = unsafe { core::slice::from_raw_parts(desc.functions, desc.functions_len) };
    let api = mock_host_api();
    (functions[index].fun)(&raw const api, args.as_ptr(), args.len())
}

#[test]
fn ok_path() {
    // The mock encodes integers in the first opaque limb, so this checks
    // the arguments and the result travel through the glue byte-exact:
    // 19 + 23 must come back as 42.
    let args = [
        GenericValue {
            opaque: [19, 0, 0, 0],
        },
        GenericValue {
            opaque: [23, 0, 0, 0],
        },
    ];
    let ret = call_exported(0, &args);
    assert_eq!(ret.status, 0);
    assert_eq!(ret.value.opaque[0], 42);
}

#[test]
fn typed_error_path() {
    let ret = call_exported(1, &[]);
    assert_eq!(ret.status, exception_code::INDEX_ERROR);
    assert_eq!(last_interned(), "out of bounds");
}

#[test]
fn panic_becomes_exception() {
    // Silence the default panic hook for the expected panic.
    let previous = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let ret = call_exported(2, &[]);
    std::panic::set_hook(previous);

    assert_eq!(ret.status, exception_code::EXCEPTION);
    assert_eq!(last_interned(), "panic: kaboom");
}
