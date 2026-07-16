//! The raw C ABI shared between the generic interpreter and its plugins.
//!
//! Everything in this module is `#[repr(C)]` and mirrored in the generated
//! `include/generic_plugin.h` for non-Rust plugins. Plugin authors normally
//! use the safe wrapper in the crate root instead of these types directly.

use core::ffi::c_void;

/// Version of the plugin ABI described by this crate.
///
/// The host checks a module's [`ModuleDesc::abi_version`] before calling
/// anything in it and refuses to load mismatching plugins.
pub const GENERIC_PLUGIN_ABI_VERSION: u32 = 1;

/// An opaque generic runtime value.
///
/// This is the host's 32-byte `Value` bit-copied — discriminant and payload
/// included. Plugins must never inspect or fabricate its bytes; values are
/// handles to be passed back to host callbacks (the `PyObject*` / Lua-handle
/// pattern). Use [`HostApi::value_kind`] to ask what a value holds.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct GenericValue {
    /// Opaque storage — represented as `u64`s purely so the type has the
    /// host `Value`'s 8-byte alignment in every language. Never inspect.
    pub opaque: [u64; 4],
}

impl GenericValue {
    /// A zeroed value, used as a placeholder where a `GenericValue` slot is
    /// required but its content is irrelevant (e.g. the `value` of an
    /// [`FfiReturn`] that carries no result). Not a valid runtime value.
    #[must_use]
    pub const fn zeroed() -> Self {
        Self { opaque: [0; 4] }
    }
}

/// A borrowed UTF-8 string. Not NUL-terminated.
///
/// Lifetime rules: an `FfiStr` returned by a host callback stays valid until
/// the next *re-entering* callback (see the rooting contract); an `FfiStr`
/// passed to a host callback only needs to be valid for that call.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FfiStr {
    pub ptr: *const u8,
    pub len: usize,
}

impl FfiStr {
    /// An empty string with a null pointer, used by host callbacks to signal
    /// "not a string".
    #[must_use]
    pub const fn null() -> Self {
        Self {
            ptr: core::ptr::null(),
            len: 0,
        }
    }
}

/// Result of a plugin function or a re-entering host callback.
///
/// `status == 0` means success and `value` is the result. Any other status
/// is an exception-kind code from [`exception_code`](crate::exception_code)
/// and `value` is the message (a string value); unknown codes are treated as
/// the base `Exception`.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FfiReturn {
    pub status: u32,
    pub value: GenericValue,
}

/// The signature every exported plugin function has.
///
/// `args` points at `nargs` contiguous values owned by the host; they stay
/// valid (and GC-rooted) for the whole call.
pub type PluginFn =
    extern "C" fn(host: *const HostApi, args: *const GenericValue, nargs: usize) -> FfiReturn;

/// Description of one exported plugin function.
#[repr(C)]
pub struct FunctionDesc {
    /// Function name as seen from generic code.
    pub name: FfiStr,
    /// Accepted argument counts (the host checks arity before calling).
    pub arities: *const u8,
    pub arities_len: usize,
    pub fun: PluginFn,
}

/// Description of a plugin module; returned by `generic_plugin_init`, the
/// one symbol every plugin must export:
///
/// ```c
/// const ModuleDesc *generic_plugin_init(void);
/// ```
#[repr(C)]
pub struct ModuleDesc {
    pub abi_version: u32,
    pub functions: *const FunctionDesc,
    pub functions_len: usize,
}

// SAFETY: sharing a descriptor only permits reading its fields (copying
// pointer values), which is thread-safe; dereferencing the pointers is
// `unsafe` and carries the following obligations at each use site:
// - `functions` must point to `functions_len` contiguous, initialized
//   `FunctionDesc` entries that are never mutated and outlive every read —
//   for descriptors returned by `generic_plugin_init`, that means the
//   lifetime of the loaded library.
// - Within each entry, `name` must reference `name.len` bytes of valid
//   UTF-8, `arities` must reference `arities_len` initialized bytes, and
//   `fun` must be a function with the documented `PluginFn` ABI — all under
//   the same immutability and lifetime requirements as above.
// The impl is required so a descriptor can live in a `static` (statics must
// be `Sync`); `export_module!` discharges all of the above by building the
// tables from `const` data.
unsafe impl Sync for ModuleDesc {}

/// The host vtable handed to every plugin call.
///
/// `ctx` is an opaque pointer owned by the host; pass it as the first
/// argument to every callback. Callbacks marked **re-entering** run generic
/// bytecode, during which garbage collection may occur — see the rooting
/// contract: across a re-entering callback, `root` every value you still
/// hold and re-fetch any [`FfiStr`] afterward. All other callbacks never
/// trigger collection.
#[repr(C)]
pub struct HostApi {
    pub abi_version: u32,
    pub ctx: *mut c_void,

    // --- inspect (never re-enter) ---
    /// Kind of the value, as a [`value_kind`](crate::value_kind) code.
    pub value_kind: extern "C" fn(ctx: *mut c_void, value: GenericValue) -> u32,
    /// `false` if the value is not a bool.
    pub bool_get: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut bool) -> bool,
    /// `false` if the value is not an integer or does not fit in an `i64`
    /// (big integers; fall back to `value_display`).
    pub int_get: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut i64) -> bool,
    /// `false` if the value is not a float.
    pub float_get: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut f64) -> bool,
    /// The interned bytes of a string value; [`FfiStr::null`] if the value
    /// is not a string. Valid until the next re-entering callback.
    pub string_get: extern "C" fn(ctx: *mut c_void, value: GenericValue) -> FfiStr,
    /// `false` if the value is not a list.
    pub list_len: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool,
    /// `false` if the value is not a list or the index is out of bounds.
    pub list_get: extern "C" fn(
        ctx: *mut c_void,
        value: GenericValue,
        index: usize,
        out: *mut GenericValue,
    ) -> bool,
    /// `false` if the value is not a tuple.
    pub tuple_len: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool,
    /// `false` if the value is not a tuple or the index is out of bounds.
    pub tuple_get: extern "C" fn(
        ctx: *mut c_void,
        value: GenericValue,
        index: usize,
        out: *mut GenericValue,
    ) -> bool,
    /// `false` if the value is not a dict.
    pub dict_len: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool,
    /// `false` if the value is not a set.
    pub set_len: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool,

    // --- attributes (never re-enter; generic fields are plain map entries) ---
    /// A field of an instance; `AttributeError` if absent, `TypeError` if
    /// the receiver is not an instance.
    pub attr_get:
        extern "C" fn(ctx: *mut c_void, receiver: GenericValue, name: FfiStr) -> FfiReturn,
    /// Set a field on an instance; `TypeError` if the receiver is not an
    /// instance (the ok value is nil).
    pub attr_set: extern "C" fn(
        ctx: *mut c_void,
        receiver: GenericValue,
        name: FfiStr,
        value: GenericValue,
    ) -> FfiReturn,
    /// Whether an instance has a field; `false` (the return, not `out`) if
    /// the receiver is not an instance.
    pub attr_has: extern "C" fn(
        ctx: *mut c_void,
        receiver: GenericValue,
        name: FfiStr,
        out: *mut bool,
    ) -> bool,

    // --- construct (never re-enter) ---
    pub nil_new: extern "C" fn(ctx: *mut c_void) -> GenericValue,
    pub bool_new: extern "C" fn(ctx: *mut c_void, value: bool) -> GenericValue,
    pub int_new: extern "C" fn(ctx: *mut c_void, value: i64) -> GenericValue,
    pub float_new: extern "C" fn(ctx: *mut c_void, value: f64) -> GenericValue,
    /// Interns the given UTF-8 bytes; `false` on invalid UTF-8.
    pub string_new: extern "C" fn(ctx: *mut c_void, value: FfiStr, out: *mut GenericValue) -> bool,
    /// A new, empty list.
    pub list_new: extern "C" fn(ctx: *mut c_void) -> GenericValue,
    /// `false` if the target is not a list.
    pub list_push: extern "C" fn(ctx: *mut c_void, list: GenericValue, item: GenericValue) -> bool,
    /// Replace the element at an index; `false` if the target is not a list
    /// or the index is out of bounds.
    pub list_set: extern "C" fn(
        ctx: *mut c_void,
        list: GenericValue,
        index: usize,
        value: GenericValue,
    ) -> bool,

    // --- display (never re-enters) ---
    /// The raw string representation of any value, as a new string value.
    /// Does NOT honor a user class's `__str__` (use `value_str` for that),
    /// which makes it safe to call anywhere, including error paths.
    pub value_display: extern "C" fn(ctx: *mut c_void, value: GenericValue) -> GenericValue,

    // --- re-entering (run generic bytecode; GC may occur) ---
    /// Call a callable value (closure, native, class, …) with the given
    /// arguments. Generic exceptions come back as a nonzero status.
    pub call_value: extern "C" fn(
        ctx: *mut c_void,
        callee: GenericValue,
        args: *const GenericValue,
        nargs: usize,
    ) -> FfiReturn,
    /// Invoke a named method on a receiver.
    pub invoke_method: extern "C" fn(
        ctx: *mut c_void,
        receiver: GenericValue,
        name: FfiStr,
        args: *const GenericValue,
        nargs: usize,
    ) -> FfiReturn,
    /// String conversion honoring a user class's `__str__`.
    pub value_str: extern "C" fn(ctx: *mut c_void, value: GenericValue) -> FfiReturn,
    /// Look up a key (`KeyError` if absent); re-enters for `__hash__`/`__eq__`.
    pub dict_get:
        extern "C" fn(ctx: *mut c_void, dict: GenericValue, key: GenericValue) -> FfiReturn,
    /// Insert or replace a key (the ok value is nil).
    pub dict_set: extern "C" fn(
        ctx: *mut c_void,
        dict: GenericValue,
        key: GenericValue,
        value: GenericValue,
    ) -> FfiReturn,
    /// Whether a dict contains a key (the ok value is a bool).
    pub dict_contains:
        extern "C" fn(ctx: *mut c_void, dict: GenericValue, key: GenericValue) -> FfiReturn,
    /// Add an item to a set (the ok value is nil).
    pub set_add:
        extern "C" fn(ctx: *mut c_void, set: GenericValue, item: GenericValue) -> FfiReturn,
    /// Whether a set contains an item (the ok value is a bool).
    pub set_contains:
        extern "C" fn(ctx: *mut c_void, set: GenericValue, item: GenericValue) -> FfiReturn,
    /// Truthiness honoring `__bool__` (the ok value is a bool).
    pub value_truthy: extern "C" fn(ctx: *mut c_void, value: GenericValue) -> FfiReturn,
    /// Equality honoring `__eq__` (the ok value is a bool).
    pub value_equals:
        extern "C" fn(ctx: *mut c_void, a: GenericValue, b: GenericValue) -> FfiReturn,
    /// Hash honoring `__hash__` (the ok value is an integer).
    pub value_hash: extern "C" fn(ctx: *mut c_void, value: GenericValue) -> FfiReturn,

    // --- rooting (never re-enter) ---
    /// Keep a value alive across re-entering callbacks. Roots are released
    /// automatically when the plugin function returns; `unroot` releases
    /// the `n` most recent roots early.
    pub root: extern "C" fn(ctx: *mut c_void, value: GenericValue),
    pub unroot: extern "C" fn(ctx: *mut c_void, n: usize),
}
