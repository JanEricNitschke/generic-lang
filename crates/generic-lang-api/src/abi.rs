//! The raw C ABI shared between the generic interpreter and its plugins.
//!
//! Everything in this module is `#[repr(C)]` and mirrored in the generated
//! `include/generic.h` for non-Rust plugins. Plugin authors normally
//! use the safe wrapper in the crate root instead of these types directly.
//!
//! Every function pointer here is an `unsafe extern "C" fn` stating its
//! contract in a `# Safety` section. The C representation is unaffected, so
//! the generated header is identical.

use core::ffi::c_void;
use core::mem::MaybeUninit;
use core::ptr;

/// Version of the plugin ABI described by this crate.
///
/// The host checks a module's [`ModuleDesc::abi_version`] before calling
/// anything in it and refuses to load mismatching plugins.
pub const GENERIC_PLUGIN_ABI_VERSION: u32 = 1;

/// An opaque generic runtime value.
///
/// This is the host's 32-byte `Value` bit-copied - discriminant and payload
/// included. Never inspect or fabricate the bytes: a value is an opaque handle
/// to be passed back to host callbacks, and decoding one is only sound for
/// values that host issued. Ask [`HostApi::value_kind`] what a value holds.
///
/// For Rust plugins the rule is enforced, not just stated: the storage is
/// private, so safe code cannot produce a value at all - one can only come from
/// the host, or from the `unsafe` `from_limbs` a Rust *host* implementation
/// uses. That is what makes `as_int`, `attr_get` and the rest of
/// [`Host`](crate::Host) safe functions.
///
/// C, C++, and Zig plugins see a plain `uint64_t opaque[4]` and can fill it
/// with anything; there the rule is only a rule, backed by the same trust you
/// extend by loading the library at all.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct GenericValue {
    /// Opaque storage. The limbs are [`MaybeUninit`] because a host `Value`
    /// does not initialize all 32 bytes - small enum variants leave the
    /// rest unwritten - and bit-copying it in must not assert those bytes
    /// are initialized (that would be undefined behavior). `u64` limbs give
    /// the type the host `Value`'s 8-byte alignment; it renders as
    /// `uint64_t opaque[4]` in C. Never inspect.
    opaque: [MaybeUninit<u64>; 4],
}

impl GenericValue {
    /// Assemble a value from raw limbs.
    ///
    /// For *hosts*: an interpreter written in Rust builds values this way (the
    /// generic interpreter itself bit-copies its `Value` instead). A plugin has
    /// no reason to call this - values come from the host.
    ///
    /// # Safety
    ///
    /// `opaque` must be the bit pattern of a value the host that will receive
    /// it issued. Handing a host limbs it did not produce makes every callback
    /// taking the value undefined behavior.
    #[must_use]
    pub const unsafe fn from_limbs(opaque: [MaybeUninit<u64>; 4]) -> Self {
        Self { opaque }
    }

    /// The raw limbs, for a host decoding a value it issued.
    ///
    /// Reading them is safe; interpreting them is not - which limbs are
    /// initialized is up to the host that produced the value.
    #[must_use]
    pub const fn limbs(&self) -> &[MaybeUninit<u64>; 4] {
        &self.opaque
    }
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
            ptr: ptr::null(),
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
///
/// # Safety
///
/// Calling one is a call into foreign code, sound only if the callee upholds
/// the ABI (which loading the library already trusts it to - see
/// `generic_plugin_init`). The caller must pass a `host` pointing to a valid
/// [`HostApi`] and an `args` pointing to `nargs` contiguous, initialized
/// [`GenericValue`]s, both valid for the duration of the call.
pub type PluginFn = unsafe extern "C" fn(
    host: *const HostApi,
    args: *const GenericValue,
    nargs: usize,
) -> FfiReturn;

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
        unsafe extern "C" fn(
            host: *const HostApi,
            args: *const GenericValue,
            nargs: usize,
        ) -> FfiReturn,
    >,
}

/// Reports a held [`GenericValue`] to the host's mark phase during GC.
///
/// The host grays `value`; `ctx` is the `visit_ctx` passed to the enclosing
/// [`PluginTraverseFn`]. Called only from within a [`PluginTraverseFn`], never
/// directly.
///
/// # Safety
///
/// `ctx` must be the `visit_ctx` the host passed to the enclosing
/// [`PluginTraverseFn`], unmodified, and `value` a [`GenericValue`] the host
/// issued (fabricating or altering one is undefined behavior). Calling this
/// outside that traversal - after it returned, or with another context - is
/// undefined behavior.
pub type PluginVisitFn = unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue);

/// Per-class GC traversal callback, declared on [`ClassDesc::traverse`].
///
/// Called by the host's GC during the mark phase, once per live plugin-backed
/// instance. The plugin must call `visit(visit_ctx, v)` for every
/// [`GenericValue`] its opaque struct references; failure to report a held
/// value is a use-after-free bug (the collector is mark-and-sweep, so a value
/// that is never reported is swept even though it is still reachable).
///
/// Returns `0` on success; a non-zero return is reserved and currently ignored.
///
/// The host already traces the instance's generic fields, so the plugin
/// reports only the values held in its own opaque state.
///
/// # Safety
///
/// The host must pass the `opaque_ptr` installed via `instance_set_opaque` on
/// an instance of the very class this callback was declared on - a pointer
/// from another class is type confusion - or null if `__init__` has not run
/// yet (the plugin must handle null gracefully). `visit` and `visit_ctx` must
/// be a valid marking function and its context, both valid for the duration
/// of the call, and the plugin must pass them to `visit` unchanged. Calling
/// this is a call into foreign code, sound only if the plugin upholds the ABI.
pub type PluginTraverseFn = unsafe extern "C" fn(
    opaque_ptr: *mut c_void,
    visit: PluginVisitFn,
    visit_ctx: *mut c_void,
) -> i32;

/// The signature of a plugin value creator.
///
/// Builds one module constant at import time, using the host callbacks to
/// construct the value. May use re-entering callbacks. Returning
/// [`FfiStatus::Exception`] makes the import fail with that exception.
///
/// # Safety
///
/// As [`PluginFn`], minus the arguments: the caller must pass a `host`
/// pointing to a valid [`HostApi`] for the duration of the call, and calling
/// into the plugin is sound only if it upholds the ABI.
pub type PluginValueFn = unsafe extern "C" fn(host: *const HostApi) -> FfiReturn;

/// Description of one exported plugin module value (a module constant,
/// built once at import time).
#[repr(C)]
pub struct ValueDesc {
    /// Value name as seen from generic code.
    pub name: FfiStr,
    /// The creator; a null pointer is rejected at load. This is
    /// [`PluginValueFn`] spelled out inline - cbindgen only renders a
    /// nullable C function pointer for an inline `Option<fn>`, not through
    /// the alias.
    pub fun: Option<unsafe extern "C" fn(host: *const HostApi) -> FfiReturn>,
}

/// The signature of a plugin method: like [`PluginFn`], but the receiver
/// (`self`) arrives as a separate first value, not in `args`. `args`/`nargs`
/// are the remaining arguments only.
///
/// # Safety
///
/// As [`PluginFn`]; `receiver` must additionally be a host-issued
/// [`GenericValue`] passed on unmodified.
pub type PluginMethodFn = unsafe extern "C" fn(
    host: *const HostApi,
    receiver: GenericValue,
    args: *const GenericValue,
    nargs: usize,
) -> FfiReturn;

/// Description of one method of a plugin-defined class.
#[repr(C)]
pub struct MethodDesc {
    /// Method name as seen from generic code (e.g. `"__init__"`, `"value"`).
    pub name: FfiStr,
    /// Accepted argument counts, **excluding** the receiver (the host checks
    /// arity before calling). A method called as `obj.foo(a, b)` declares
    /// `&[2]`; a receiver-only method declares `&[0]`.
    pub arities: *const u8,
    /// Number of entries in `arities`.
    pub arities_len: usize,
    /// The method implementation; a null pointer is rejected at load. This is
    /// [`PluginMethodFn`] spelled out inline - cbindgen only renders a nullable
    /// C function pointer for an inline `Option<fn>`, not through the alias.
    pub fun: Option<
        unsafe extern "C" fn(
            host: *const HostApi,
            receiver: GenericValue,
            args: *const GenericValue,
            nargs: usize,
        ) -> FfiReturn,
    >,
}

/// Description of a plugin-defined class; one entry per class in
/// [`ModuleDesc::classes`].
#[repr(C)]
pub struct ClassDesc {
    /// Class name as seen from generic code (e.g. `"Counter"`).
    pub name: FfiStr,
    /// Pointer to `methods_len` contiguous [`MethodDesc`] entries.
    pub methods: *const MethodDesc,
    /// Number of entries in `methods`.
    pub methods_len: usize,
    /// Destructor for the plugin's opaque per-instance state, called by the
    /// host with the `*mut c_void` installed via `instance_set_opaque` when a
    /// plugin-backed instance is garbage-collected. May be null if the plugin
    /// manages the lifetime itself (rare).
    ///
    /// Calling it requires what freeing an allocation always requires: the
    /// host must pass the pointer installed on an instance of this very class
    /// and must do so exactly once, after which the pointer is dangling.
    pub drop: Option<unsafe extern "C" fn(opaque_ptr: *mut c_void)>,
    /// GC traversal callback, called during the mark phase for each live
    /// plugin-backed instance. May be null if the opaque struct holds no
    /// [`GenericValue`]s. This is [`PluginTraverseFn`] spelled out inline -
    /// cbindgen only renders a nullable C function pointer for an inline
    /// `Option<fn>`, not through the alias - and carries that type's safety
    /// contract.
    pub traverse: Option<
        unsafe extern "C" fn(
            opaque_ptr: *mut c_void,
            visit: PluginVisitFn,
            visit_ctx: *mut c_void,
        ) -> i32,
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
    /// Pointer to `classes_len` contiguous [`ClassDesc`] entries.
    pub classes: *const ClassDesc,
    /// Number of entries in `classes`. May be 0 (function-only plugins).
    pub classes_len: usize,
    /// Pointer to `values_len` contiguous [`ValueDesc`] entries.
    pub values: *const ValueDesc,
    /// Number of entries in `values`. May be 0.
    pub values_len: usize,
}

// SAFETY: sharing a descriptor only permits reading its fields (copying
// pointer values), which is thread-safe; dereferencing the pointers is
// `unsafe` and carries the following obligations at each use site:
// - `functions` must point to `functions_len` contiguous, initialized
//   `FunctionDesc` entries, `classes` to `classes_len` contiguous,
//   initialized `ClassDesc` entries, and `values` to `values_len`
//   contiguous, initialized `ValueDesc` entries - all never mutated and
//   outliving every read; for descriptors returned by
//   `generic_plugin_init`, that means the lifetime of the loaded library.
// - Within each `FunctionDesc`/`MethodDesc`, `name` must reference `name.len`
//   bytes of valid UTF-8, `arities` must reference `arities_len` initialized
//   bytes, and `fun` must be a function with the documented `PluginFn` ABI.
//   Within each `ClassDesc`, `name` is as above and `methods` points to
//   `methods_len` contiguous `MethodDesc` entries; `drop`/`traverse` are
//   either null or functions honoring the contracts documented on those
//   fields - all under the same immutability and lifetime requirements as
//   above.
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
///
/// # Safety
///
/// Every callback is an `unsafe extern "C" fn`, and they share one contract
/// the plugin must uphold on each call (the Rust wrapper [`Host`](crate::Host)
/// discharges all of it):
/// - `ctx` is this vtable's own `ctx` field, passed on unmodified. A pointer
///   from another vtable, or one the plugin invented, is undefined behavior.
/// - Every [`GenericValue`] argument was issued by this host and is passed
///   back unmodified - the bytes are opaque, so fabricating or altering one
///   is undefined behavior (see [`GenericValue`]).
/// - Every [`FfiStr`] argument points to `len` initialized bytes of UTF-8,
///   valid for the duration of the call (see [`FfiStr`]).
/// - Every `out` pointer is non-null, well aligned, and writable for the size
///   of its type; the callback writes through it on success.
/// - `args` points to `nargs` contiguous, initialized [`GenericValue`]s for
///   the duration of the call (`nargs == 0` allows any pointer).
/// - Calls are not re-entered from another thread; the interpreter is
///   single-threaded and a callback borrows the VM for its duration.
#[repr(C)]
pub struct HostApi {
    /// ABI version of the host ([`GENERIC_PLUGIN_ABI_VERSION`]).
    pub abi_version: u32,
    /// Opaque host context; pass it back as the first argument of every
    /// callback. Never dereference it.
    pub ctx: *mut c_void,

    // --- inspect (never re-enter) ---
    /// Kind of the value, as a [`ValueKind`](crate::ValueKind) code.
    pub value_kind: unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue) -> u32,
    /// `false` if the value is not a bool.
    pub bool_get:
        unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut bool) -> bool,
    /// Read an integer into `out`; `false` if the value is not an integer
    /// or does not fit in an `i64` (big integers - fall back to
    /// `value_display`).
    pub int_get: unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut i64) -> bool,
    /// `false` if the value is not a float.
    pub float_get:
        unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut f64) -> bool,
    /// Read the interned bytes of a string value into `out` (valid until
    /// the next re-entering callback); `false` if the value is not a
    /// string.
    pub string_get:
        unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut FfiStr) -> bool,
    /// `false` if the value is not a list.
    pub list_len:
        unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool,
    /// The element at `index`. `TypeError` if the value is not a list;
    /// `IndexError` if the index is out of bounds.
    pub list_get:
        unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue, index: usize) -> FfiReturn,
    /// `false` if the value is not a tuple.
    pub tuple_len:
        unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool,
    /// The element at `index`. `TypeError` if the value is not a tuple;
    /// `IndexError` if the index is out of bounds.
    pub tuple_get:
        unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue, index: usize) -> FfiReturn,
    /// `false` if the value is not a dict.
    pub dict_len:
        unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool,
    /// `false` if the value is not a set.
    pub set_len:
        unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue, out: *mut usize) -> bool,
    /// Look up a builtin global by name (exception classes like
    /// `"TypeError"`, native classes, builtin functions). `NameError` if
    /// absent; `TypeError` if the name is invalid UTF-8.
    pub builtin_get: unsafe extern "C" fn(ctx: *mut c_void, name: FfiStr) -> FfiReturn,
    /// Whether `value` is an instance of `of_class` or of a subclass of it
    /// (a bool value on success) - the exact semantics of the `isinstance`
    /// builtin, value-type proxy classes included. `TypeError` if
    /// `of_class` is not a class.
    pub is_instance: unsafe extern "C" fn(
        ctx: *mut c_void,
        value: GenericValue,
        of_class: GenericValue,
    ) -> FfiReturn,
    /// The class of an instance, as a class value (callable to construct
    /// another instance of it - the analogue of `type(self)`). `TypeError` if
    /// `value` is not an instance. Lets a plugin method reach its own class
    /// from the receiver: e.g. to construct a result of the same class, or to
    /// `is_instance`-check another argument before reading its opaque state.
    pub class_of: unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue) -> FfiReturn,

    // --- attributes (never re-enter; generic fields are plain map entries) ---
    /// A field of an instance; `AttributeError` if absent, `TypeError` if
    /// the receiver is not an instance.
    pub attr_get:
        unsafe extern "C" fn(ctx: *mut c_void, receiver: GenericValue, name: FfiStr) -> FfiReturn,
    /// Set a field on an instance; `TypeError` if the receiver is not an
    /// instance (the ok value is nil).
    pub attr_set: unsafe extern "C" fn(
        ctx: *mut c_void,
        receiver: GenericValue,
        name: FfiStr,
        value: GenericValue,
    ) -> FfiReturn,
    /// Whether an instance has a field (a bool value on success).
    /// `TypeError` if the receiver is not an instance or the name is
    /// invalid UTF-8.
    pub attr_has:
        unsafe extern "C" fn(ctx: *mut c_void, receiver: GenericValue, name: FfiStr) -> FfiReturn,

    // --- construct (never re-enter) ---
    /// A new nil value.
    pub nil_new: unsafe extern "C" fn(ctx: *mut c_void) -> GenericValue,
    /// A new bool value.
    pub bool_new: unsafe extern "C" fn(ctx: *mut c_void, value: bool) -> GenericValue,
    /// A new integer value.
    pub int_new: unsafe extern "C" fn(ctx: *mut c_void, value: i64) -> GenericValue,
    /// A new float value.
    pub float_new: unsafe extern "C" fn(ctx: *mut c_void, value: f64) -> GenericValue,
    /// Interns the given UTF-8 bytes into a string value; `ValueError` on
    /// invalid UTF-8.
    pub string_new: unsafe extern "C" fn(ctx: *mut c_void, value: FfiStr) -> FfiReturn,
    /// A new, empty list.
    pub list_new: unsafe extern "C" fn(ctx: *mut c_void) -> GenericValue,
    /// Append to a list (the ok value is nil); `TypeError` if the target is
    /// not a list.
    pub list_push:
        unsafe extern "C" fn(ctx: *mut c_void, list: GenericValue, item: GenericValue) -> FfiReturn,
    /// Replace the element at an index (the ok value is nil). `TypeError`
    /// if the target is not a list; `IndexError` if the index is out of
    /// bounds.
    pub list_set: unsafe extern "C" fn(
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
    pub exception_new: unsafe extern "C" fn(
        ctx: *mut c_void,
        of_class: GenericValue,
        message: FfiStr,
    ) -> FfiReturn,

    // --- display (never re-enters) ---
    /// The raw string representation of any value, as a new string value.
    /// Does NOT honor a user class's `__str__` (use `value_str` for that),
    /// which makes it usable anywhere, including error paths.
    pub value_display: unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue) -> GenericValue,

    // --- re-entering (run generic bytecode; GC may occur) ---
    /// Call a callable value (closure, native, class, …) with the given
    /// arguments. Generic exceptions come back as a nonzero status.
    pub call_value: unsafe extern "C" fn(
        ctx: *mut c_void,
        callee: GenericValue,
        args: *const GenericValue,
        nargs: usize,
    ) -> FfiReturn,
    /// Invoke a named method on a receiver.
    pub invoke_method: unsafe extern "C" fn(
        ctx: *mut c_void,
        receiver: GenericValue,
        name: FfiStr,
        args: *const GenericValue,
        nargs: usize,
    ) -> FfiReturn,
    /// String conversion honoring a user class's `__str__`.
    pub value_str: unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue) -> FfiReturn,
    /// Look up a key (`KeyError` if absent); re-enters for `__hash__`/`__eq__`.
    pub dict_get:
        unsafe extern "C" fn(ctx: *mut c_void, dict: GenericValue, key: GenericValue) -> FfiReturn,
    /// Insert or replace a key (the ok value is nil).
    pub dict_set: unsafe extern "C" fn(
        ctx: *mut c_void,
        dict: GenericValue,
        key: GenericValue,
        value: GenericValue,
    ) -> FfiReturn,
    /// Whether a dict contains a key (the ok value is a bool).
    pub dict_contains:
        unsafe extern "C" fn(ctx: *mut c_void, dict: GenericValue, key: GenericValue) -> FfiReturn,
    /// Add an item to a set (the ok value is nil).
    pub set_add:
        unsafe extern "C" fn(ctx: *mut c_void, set: GenericValue, item: GenericValue) -> FfiReturn,
    /// Whether a set contains an item (the ok value is a bool).
    pub set_contains:
        unsafe extern "C" fn(ctx: *mut c_void, set: GenericValue, item: GenericValue) -> FfiReturn,
    /// Truthiness honoring `__bool__` (the ok value is a bool).
    pub value_truthy: unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue) -> FfiReturn,
    /// Equality honoring `__eq__` (the ok value is a bool).
    pub value_equals:
        unsafe extern "C" fn(ctx: *mut c_void, a: GenericValue, b: GenericValue) -> FfiReturn,
    /// Hash honoring `__hash__` (the ok value is an integer).
    pub value_hash: unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue) -> FfiReturn,

    // --- rooting (never re-enter) ---
    /// Keep a value alive across re-entering callbacks. Roots are released
    /// automatically when the plugin function returns; `unroot` releases
    /// the `n` most recent roots early.
    pub root: unsafe extern "C" fn(ctx: *mut c_void, value: GenericValue),
    /// Release the `n` most recently rooted values. Releasing more roots
    /// than were pushed corrupts interpreter state.
    pub unroot: unsafe extern "C" fn(ctx: *mut c_void, n: usize),

    // --- plugin instance state (never re-enter) ---
    /// Install the plugin's opaque pointer on a plugin-backed instance
    /// (typically from `__init__`, with the receiver as `self`). The class's
    /// `drop`/`traverse` were declared on its [`ClassDesc`]; this only sets the
    /// pointer. `TypeError` if `receiver` is not a plugin-backed instance.
    ///
    /// Overwriting an already-installed pointer leaks the previous one:
    /// the host does not run `drop` on it, since it cannot know whether the plugin
    /// still holds a copy elsewhere. To replace state, recover the old pointer
    /// with `instance_get_opaque` and free it yourself first.
    ///
    /// Beyond the shared contract: the host stores `ptr` without ever reading
    /// through it, but hands it back to this class's `traverse` and `drop`, so
    /// it must be one those can soundly consume - see
    /// [`Host::set_opaque`](crate::Host::set_opaque).
    pub instance_set_opaque: unsafe extern "C" fn(
        ctx: *mut c_void,
        receiver: GenericValue,
        ptr: *mut c_void,
    ) -> FfiReturn,
    /// Recover the pointer installed by [`HostApi::instance_set_opaque`], or
    /// null if none was installed (e.g. before `__init__` ran) or `receiver`
    /// is not a plugin-backed instance. Never raises.
    pub instance_get_opaque:
        unsafe extern "C" fn(ctx: *mut c_void, receiver: GenericValue) -> *mut c_void,
}
