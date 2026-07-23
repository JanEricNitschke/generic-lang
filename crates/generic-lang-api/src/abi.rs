//! The raw C ABI shared between the generic interpreter and its plugins.
//!
//! Everything in this module is `#[repr(C)]` and mirrored in the generated
//! `include/generic.h` for non-Rust plugins. Plugin authors normally
//! use the safe wrapper in the crate root instead of these types directly.

use core::ffi::c_void;
use core::mem::MaybeUninit;

/// Version of the plugin ABI described by this crate.
///
/// The host checks a module's [`ModuleDesc::abi_version`] before calling
/// anything in it and refuses to load mismatching plugins.
pub const GENERIC_PLUGIN_ABI_VERSION: u32 = 1;

/// An opaque generic runtime value.
///
/// This is the host's 32-byte `Value` bit-copied - discriminant and payload
/// included. Plugins must never inspect or fabricate its bytes; values are
/// opaque handles to be passed back to host callbacks. Use
/// [`HostApi::value_kind`] to ask what a value holds.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct GenericValue {
    /// Opaque storage. The limbs are [`MaybeUninit`] because a host `Value`
    /// does not initialize all 32 bytes - small enum variants leave the
    /// rest unwritten - and bit-copying it in must not assert those bytes
    /// are initialized (that would be undefined behavior). `u64` limbs give
    /// the type the host `Value`'s 8-byte alignment; it renders as
    /// `uint64_t opaque[4]` in C. Never inspect.
    pub opaque: [MaybeUninit<u64>; 4],
}

impl core::fmt::Debug for GenericValue {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // The bytes are opaque - nothing meaningful to print.
        f.debug_struct("GenericValue").finish_non_exhaustive()
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
    /// Pointer to the first byte. Must be non-null in both directions - an
    /// empty string is a non-null pointer with `len == 0` (e.g. C's `""`);
    /// a null pointer is not a valid string value.
    pub ptr: *const u8,
    /// Length in bytes.
    pub len: usize,
}

impl FfiStr {
    /// A null-pointer `FfiStr` for initializing the out-parameter of a
    /// bool-probe callback (`string_get` overwrites it on success). Not a
    /// valid string value - see [`FfiStr::ptr`].
    #[must_use]
    pub const fn null() -> Self {
        Self {
            ptr: core::ptr::null(),
            len: 0,
        }
    }
}

/// Discriminator for [`FfiReturn::status`].
///
/// On the wire the status is a plain `u32` (an arbitrary integer from a
/// plugin must not become a Rust enum); decode with
/// [`FfiStatus::from_u32`], encode with `as u32`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FfiStatus {
    /// Success - `value` is the call's result.
    Ok = 0,
    /// `value` is the exception *instance*.
    ///
    /// From a host callback this is the exception generic code raised,
    /// handed over with full identity: returning the same value (the safe
    /// wrapper's `?` does) re-raises exactly that exception - class,
    /// fields, and original stack trace intact. To throw a fresh
    /// exception, create the instance with [`HostApi::exception_new`] and
    /// return it under this status; a caught one can be examined with
    /// [`HostApi::is_instance`] against a class from
    /// [`HostApi::builtin_get`].
    Exception = 1,
    /// A fatal host runtime error passing through the plugin.
    ///
    /// Not an exception - it is uncatchable. A re-entering host
    /// callback returns it when the interpreter hit a fatal error; the
    /// plugin must forward it unchanged (the safe wrapper's `?` does), and
    /// the host re-raises it as a fatal error when the plugin call
    /// returns. `value` carries no meaning for this status.
    Fatal = 99,
}

impl FfiStatus {
    /// Decode a raw status. `None` means the value is not a valid status -
    /// a protocol violation the host surfaces as a plugin bug (there is no
    /// safe fallback: `value` must not be interpreted at all).
    #[must_use]
    pub const fn from_u32(status: u32) -> Option<Self> {
        match status {
            0 => Some(Self::Ok),
            1 => Some(Self::Exception),
            99 => Some(Self::Fatal),
            _ => None,
        }
    }
}

/// Result of a plugin function or a re-entering host callback.
///
/// `value` is always present; `status` (a [`FfiStatus`] as `u32`) says
/// what it is: the call's result, an exception instance to (re-)raise, or
/// a meaningless placeholder accompanying a fatal pass-through error.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FfiReturn {
    /// A [`FfiStatus`] as `u32`: what `value` means.
    pub status: u32,
    /// The result, the exception instance, or a fatal-status placeholder.
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
    /// Number of entries in `arities`.
    pub arities_len: usize,
    /// The function implementation; a null pointer is rejected at load.
    /// The type is [`PluginFn`] spelled out inline - cbindgen only renders
    /// a nullable C function pointer for an inline `Option<fn>`, not
    /// through the alias.
    pub fun: Option<
        extern "C" fn(host: *const HostApi, args: *const GenericValue, nargs: usize) -> FfiReturn,
    >,
}

/// Description of a plugin module; returned by `generic_plugin_init`, the
/// one symbol every plugin must export:
///
/// ```c
/// const ModuleDesc *generic_plugin_init(void);
/// ```
#[repr(C)]
pub struct ModuleDesc {
    /// ABI version the plugin was built against ([`GENERIC_PLUGIN_ABI_VERSION`]).
    pub abi_version: u32,
    /// Pointer to `functions_len` contiguous [`FunctionDesc`] entries.
    pub functions: *const FunctionDesc,
    /// Number of entries in `functions`.
    pub functions_len: usize,
}

// SAFETY: sharing a descriptor only permits reading its fields (copying
// pointer values), which is thread-safe; dereferencing the pointers is
// `unsafe` and carries the following obligations at each use site:
// - `functions` must point to `functions_len` contiguous, initialized
//   `FunctionDesc` entries that are never mutated and outlive every read -
//   for descriptors returned by `generic_plugin_init`, that means the
//   lifetime of the loaded library.
// - Within each entry, `name` must reference `name.len` bytes of valid
//   UTF-8, `arities` must reference `arities_len` initialized bytes, and
//   `fun` must be a function with the documented `PluginFn` ABI - all under
//   the same immutability and lifetime requirements as above.
// The impl is required so a descriptor can live in a `static` (statics must
// be `Sync`); `export_module!` discharges all of the above by building the
// tables from `const` data.
unsafe impl Sync for ModuleDesc {}

/// The host vtable handed to every plugin call.
///
/// `ctx` is an opaque pointer owned by the host; pass it as the first
/// argument to every callback. Callbacks marked **re-entering** run generic
/// bytecode, during which garbage collection may occur - see the rooting
/// contract: across a re-entering callback, `root` every value still
/// held and re-fetch any [`FfiStr`] afterward. All other callbacks never
/// trigger collection.
///
/// Return conventions, decided solely by whether the payload forces an
/// out-parameter:
/// - A payload the caller must receive as something other than a
///   [`GenericValue`] - a raw machine scalar (`bool`, `i64`, `f64`,
///   `usize`) or a borrowed [`FfiStr`] - cannot ride in an [`FfiReturn`]
///   (whose payload is a [`GenericValue`]), so it travels through an
///   out-parameter and the callback returns a plain `bool` - `true` on
///   success, `false` on the sole "wrong kind" failure. These carry no
///   exception.
/// - Everything else - payload is a [`GenericValue`], or there is no
///   payload - returns [`FfiReturn`], carrying a real exception instance on
///   failure whose class and message mirror what the equivalent generic
///   operation would throw.
/// - Infallible callbacks return their value directly.
#[repr(C)]
pub struct HostApi {
    /// ABI version of the host ([`GENERIC_PLUGIN_ABI_VERSION`]).
    pub abi_version: u32,
    /// Opaque host context; pass it back as the first argument of every
    /// callback. Never dereference it.
    pub ctx: *mut c_void,

    // --- inspect (never re-enter) ---
    /// Kind of the value, as a [`ValueKind`](crate::ValueKind) code.
    pub value_kind: extern "C" fn(ctx: *mut c_void, value: GenericValue) -> u32,
    /// `false` if the value is not a bool.
    pub bool_get: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut bool) -> bool,
    /// Read an integer into `out`; `false` if the value is not an integer
    /// or does not fit in an `i64` (big integers - fall back to
    /// `value_display`).
    pub int_get: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut i64) -> bool,
    /// `false` if the value is not a float.
    pub float_get: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut f64) -> bool,
    /// Read the interned bytes of a string value into `out` (valid until
    /// the next re-entering callback); `false` if the value is not a
    /// string.
    pub string_get: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut FfiStr) -> bool,
    /// `false` if the value is not a list.
    pub list_len: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool,
    /// The element at `index`. `TypeError` if the value is not a list;
    /// `IndexError` if the index is out of bounds.
    pub list_get: extern "C" fn(ctx: *mut c_void, value: GenericValue, index: usize) -> FfiReturn,
    /// `false` if the value is not a tuple.
    pub tuple_len: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool,
    /// The element at `index`. `TypeError` if the value is not a tuple;
    /// `IndexError` if the index is out of bounds.
    pub tuple_get: extern "C" fn(ctx: *mut c_void, value: GenericValue, index: usize) -> FfiReturn,
    /// `false` if the value is not a dict.
    pub dict_len: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool,
    /// `false` if the value is not a set.
    pub set_len: extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool,
    /// Look up a builtin global by name (exception classes like
    /// `"TypeError"`, native classes, builtin functions). `NameError` if
    /// absent; `TypeError` if the name is invalid UTF-8.
    pub builtin_get: extern "C" fn(ctx: *mut c_void, name: FfiStr) -> FfiReturn,
    /// Whether `value` is an instance of `of_class` or of a subclass of it
    /// (a bool value on success) - the exact semantics of the `isinstance`
    /// builtin, value-type proxy classes included. `TypeError` if
    /// `of_class` is not a class.
    pub is_instance:
        extern "C" fn(ctx: *mut c_void, value: GenericValue, of_class: GenericValue) -> FfiReturn,

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
    /// Whether an instance has a field (a bool value on success).
    /// `TypeError` if the receiver is not an instance or the name is
    /// invalid UTF-8.
    pub attr_has:
        extern "C" fn(ctx: *mut c_void, receiver: GenericValue, name: FfiStr) -> FfiReturn,

    // --- construct (never re-enter) ---
    /// A new nil value.
    pub nil_new: extern "C" fn(ctx: *mut c_void) -> GenericValue,
    /// A new bool value.
    pub bool_new: extern "C" fn(ctx: *mut c_void, value: bool) -> GenericValue,
    /// A new integer value.
    pub int_new: extern "C" fn(ctx: *mut c_void, value: i64) -> GenericValue,
    /// A new float value.
    pub float_new: extern "C" fn(ctx: *mut c_void, value: f64) -> GenericValue,
    /// Interns the given UTF-8 bytes into a string value; `ValueError` on
    /// invalid UTF-8.
    pub string_new: extern "C" fn(ctx: *mut c_void, value: FfiStr) -> FfiReturn,
    /// A new, empty list.
    pub list_new: extern "C" fn(ctx: *mut c_void) -> GenericValue,
    /// Append to a list (the ok value is nil); `TypeError` if the target is
    /// not a list.
    pub list_push:
        extern "C" fn(ctx: *mut c_void, list: GenericValue, item: GenericValue) -> FfiReturn,
    /// Replace the element at an index (the ok value is nil). `TypeError`
    /// if the target is not a list; `IndexError` if the index is out of
    /// bounds.
    pub list_set: extern "C" fn(
        ctx: *mut c_void,
        list: GenericValue,
        index: usize,
        value: GenericValue,
    ) -> FfiReturn,
    /// A new exception instance of `of_class` carrying `message`.
    /// `TypeError` if `of_class` is not a class deriving from `Exception`
    /// or the message is invalid UTF-8. Sets the message directly,
    /// bypassing the class's `__init__` - exactly like the VM's own throw;
    /// use `call_value` on the class for full construction semantics.
    /// Return the instance under [`FfiStatus::Exception`] to throw it.
    pub exception_new:
        extern "C" fn(ctx: *mut c_void, of_class: GenericValue, message: FfiStr) -> FfiReturn,

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
    /// Release the `n` most recently rooted values. Releasing more roots
    /// than were pushed corrupts interpreter state.
    pub unroot: extern "C" fn(ctx: *mut c_void, n: usize),
}
