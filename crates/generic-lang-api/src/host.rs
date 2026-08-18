//! Safe wrapper around the host vtable for Rust plugin authors.

use core::{ffi::c_void, slice, str};
use std::panic::{self, AssertUnwindSafe};

use crate::abi::{FfiReturn, FfiStatus, FfiStr, GenericValue, HostApi};
use crate::{PluginError, ValueKind};

/// Generates the typed error constructors on [`Host`]: one per builtin
/// exception class, with the class name spelled exactly once - a plugin
/// author cannot typo a builtin class name by going through these.
macro_rules! host_error_constructors {
    ($($(#[$doc:meta])* $name:ident => $class_name:literal),* $(,)?) => {
        $(
            $(#[$doc])*
            #[must_use]
            pub fn $name(&self, message: &str) -> PluginError {
                self.error($class_name, message)
            }
        )*
    };
}

/// Safe access to the host VM for the duration of one plugin call.
///
/// Methods that run generic bytecode (and may therefore trigger garbage
/// collection) take `&mut self`: the borrow checker then guarantees the
/// rooting contract's string rule - any [`&str`](str) obtained from
/// the host borrows `self` and cannot be held across a re-entering call.
/// Values held across a re-entering call must be rooted, e.g.
/// via [`Host::rooted`].
///
/// This is the type that turns the raw [`HostApi`] vtable into a safe API:
/// constructing one is `unsafe` ([`Host::new`], called only by the
/// `export_module!` glue) and asserts, once, that the vtable really is the
/// host's. Every method may then rely on that invariant, so plugin authors
/// write no `unsafe` at all.
///
/// # Invariant
///
/// `api` is a vtable the generic interpreter handed to this plugin call, with
/// `api.ctx` its matching context, and both stay valid for `'a` - i.e. every
/// callback in it may be called with `api.ctx` for as long as this `Host`
/// exists, and answers per the [`HostApi`] protocol.
#[derive(Debug)]
pub struct Host<'a> {
    api: &'a HostApi,
}

/// A decoded view of a [`GenericValue`], obtained via [`Host::decode`].
#[derive(Debug, Clone, Copy)]
pub enum ArgValue<'h> {
    /// The `nil` value.
    Nil,
    /// A boolean.
    Bool(bool),
    /// An integer that fits in an `i64`.
    Int(i64),
    /// An integer that does not fit in an `i64`; convert via
    /// [`Host::display`] if a textual form suffices.
    BigInt(GenericValue),
    /// A float.
    Float(f64),
    /// A rational number; inspect via [`Host::display`].
    Rational(GenericValue),
    /// A string, borrowed from the host (see the re-fetch rule).
    Str(&'h str),
    /// A list ([`Host::list_len`], [`Host::list_get`]).
    List(GenericValue),
    /// A tuple ([`Host::tuple_len`], [`Host::tuple_get`]).
    Tuple(GenericValue),
    /// A dict ([`Host::dict_get`], [`Host::dict_set`]).
    Dict(GenericValue),
    /// A set ([`Host::set_add`], [`Host::set_contains`]).
    Set(GenericValue),
    /// A range; inspect via [`Host::display`] or drive its iterator.
    Range(GenericValue),
    /// The exhausted-iterator sentinel.
    StopIteration,
    /// A plain class instance ([`Host::attr_get`], [`Host::invoke`]).
    Instance(GenericValue),
    /// A class ([`Host::call`] instantiates).
    Class(GenericValue),
    /// A callable ([`Host::call`]).
    Function(GenericValue),
    /// A module.
    Module(GenericValue),
    /// An exception instance.
    Exception(GenericValue),
    /// A generator; drive via [`Host::invoke`] with `__next__`.
    Generator(GenericValue),
    /// An iterator (drive via [`Host::invoke`] with `__next__`).
    Iterator(GenericValue),
    /// VM-internal values a plugin should never meaningfully receive.
    Other(GenericValue),
}

impl<'a> Host<'a> {
    /// Wrap a host vtable. Called by the `export_module!` glue.
    ///
    /// # Safety
    ///
    /// `api` must satisfy the invariant documented on [`Host`]: a vtable the
    /// interpreter passed to this plugin call, valid - together with its
    /// `ctx` - for `'a`. This is the single assertion that makes every other
    /// method on [`Host`] safe; a fabricated vtable makes all of them
    /// undefined behavior.
    #[doc(hidden)]
    #[must_use]
    pub const unsafe fn new(api: &'a HostApi) -> Self {
        Self { api }
    }

    // --- inspect ---

    /// The [`ValueKind`] of a value.
    #[must_use]
    pub fn kind(&self, value: GenericValue) -> ValueKind {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        ValueKind::from_u32(unsafe { (self.api.value_kind)(self.api.ctx, value) })
    }

    /// Decode a value into a borrowed view.
    #[must_use]
    pub fn decode(&self, value: GenericValue) -> ArgValue<'_> {
        match self.kind(value) {
            ValueKind::Nil => ArgValue::Nil,
            ValueKind::Bool => ArgValue::Bool(self.as_bool(value).unwrap_or_default()),
            ValueKind::Int => ArgValue::Int(self.as_int(value).unwrap_or_default()),
            ValueKind::BigInt => ArgValue::BigInt(value),
            ValueKind::Float => ArgValue::Float(self.as_float(value).unwrap_or_default()),
            ValueKind::Rational => ArgValue::Rational(value),
            ValueKind::String => ArgValue::Str(self.as_str(value).unwrap_or_default()),
            ValueKind::List => ArgValue::List(value),
            ValueKind::Tuple => ArgValue::Tuple(value),
            ValueKind::Dict => ArgValue::Dict(value),
            ValueKind::Set => ArgValue::Set(value),
            ValueKind::Range => ArgValue::Range(value),
            ValueKind::StopIteration => ArgValue::StopIteration,
            ValueKind::Instance => ArgValue::Instance(value),
            ValueKind::Class => ArgValue::Class(value),
            ValueKind::Function => ArgValue::Function(value),
            ValueKind::Module => ArgValue::Module(value),
            ValueKind::Exception => ArgValue::Exception(value),
            ValueKind::Generator => ArgValue::Generator(value),
            ValueKind::Iterator => ArgValue::Iterator(value),
            ValueKind::Other => ArgValue::Other(value),
        }
    }

    /// `None` if the value is not a bool.
    #[must_use]
    pub fn as_bool(&self, value: GenericValue) -> Option<bool> {
        let mut out = false;
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; `out` is a live local.
        unsafe { (self.api.bool_get)(self.api.ctx, value, &raw mut out) }.then_some(out)
    }

    /// The value as an `i64`; `None` if it is not an integer or does not
    /// fit in an `i64` (big integers - fall back to `display`).
    #[must_use]
    pub fn as_int(&self, value: GenericValue) -> Option<i64> {
        let mut out = 0i64;
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; `out` is a live local.
        unsafe { (self.api.int_get)(self.api.ctx, value, &raw mut out) }.then_some(out)
    }

    /// `None` if the value is not a float.
    #[must_use]
    pub fn as_float(&self, value: GenericValue) -> Option<f64> {
        let mut out = 0f64;
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; `out` is a live local.
        unsafe { (self.api.float_get)(self.api.ctx, value, &raw mut out) }.then_some(out)
    }

    /// The contents of a string value; `None` if the value is not a string
    /// (or the host answered with malformed string data - a null pointer or
    /// invalid UTF-8, both protocol violations).
    ///
    /// The returned string borrows the host and therefore cannot be held
    /// across a re-entering call (`&mut self` methods); the compiler
    /// rejects it:
    ///
    /// ```compile_fail,E0502
    /// use generic_lang_api::{GenericValue, Host, PluginError};
    ///
    /// fn plugin_fn(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    ///     let name = host.as_str(args[0]).unwrap();     // borrows `host`
    ///     host.call(args[1], &[])?;                     // re-enters: needs `&mut host`
    ///     Ok(host.make_str(name))                       // ERROR: `name` still borrowed
    /// }
    /// ```
    ///
    /// Copy the string out (`.to_owned()`) before re-entering if it is
    /// needed afterwards.
    #[must_use]
    pub fn as_str(&self, value: GenericValue) -> Option<&str> {
        let mut out = FfiStr::null();
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; `out` is a live local.
        if !unsafe { (self.api.string_get)(self.api.ctx, value, &raw mut out) } {
            return None;
        }
        // A null pointer is not a valid `FfiStr` (see `abi::FfiStr`): a host
        // that wrote one - or answered `true` without writing at all -
        // violated the protocol. Report "not a string" rather than fabricate
        // a value from it.
        if out.ptr.is_null() {
            return None;
        }
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        let bytes = unsafe { slice::from_raw_parts(out.ptr, out.len) };
        str::from_utf8(bytes).ok()
    }

    /// `None` if the value is not a list.
    #[must_use]
    pub fn list_len(&self, value: GenericValue) -> Option<usize> {
        let mut out = 0usize;
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; `out` is a live local.
        unsafe { (self.api.list_len)(self.api.ctx, value, &raw mut out) }.then_some(out)
    }

    /// The list element at `index`.
    ///
    /// # Errors
    ///
    /// `TypeError` if the value is not a list; `IndexError` if the index
    /// is out of bounds.
    pub fn list_get(&self, value: GenericValue, index: usize) -> Result<GenericValue, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        self.ffi_result(unsafe { (self.api.list_get)(self.api.ctx, value, index) })
    }

    /// `None` if the value is not a tuple.
    #[must_use]
    pub fn tuple_len(&self, value: GenericValue) -> Option<usize> {
        let mut out = 0usize;
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; `out` is a live local.
        unsafe { (self.api.tuple_len)(self.api.ctx, value, &raw mut out) }.then_some(out)
    }

    /// The tuple element at `index`.
    ///
    /// # Errors
    ///
    /// `TypeError` if the value is not a tuple; `IndexError` if the index
    /// is out of bounds.
    pub fn tuple_get(
        &self,
        value: GenericValue,
        index: usize,
    ) -> Result<GenericValue, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        self.ffi_result(unsafe { (self.api.tuple_get)(self.api.ctx, value, index) })
    }

    /// `None` if the value is not a dict.
    #[must_use]
    pub fn dict_len(&self, value: GenericValue) -> Option<usize> {
        let mut out = 0usize;
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; `out` is a live local.
        unsafe { (self.api.dict_len)(self.api.ctx, value, &raw mut out) }.then_some(out)
    }

    /// `None` if the value is not a set.
    #[must_use]
    pub fn set_len(&self, value: GenericValue) -> Option<usize> {
        let mut out = 0usize;
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; `out` is a live local.
        unsafe { (self.api.set_len)(self.api.ctx, value, &raw mut out) }.then_some(out)
    }

    /// Look up a builtin global by name - exception classes like
    /// `"TypeError"`, native classes, builtin functions.
    ///
    /// # Errors
    ///
    /// `NameError` if absent.
    pub fn builtin(&self, name: &str) -> Result<GenericValue, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; the `FfiStr` borrows a live `&str`.
        self.ffi_result(unsafe { (self.api.builtin_get)(self.api.ctx, Self::ffi_str(name)) })
    }

    /// Whether `value` is an instance of `class` or of a subclass of it -
    /// the exact semantics of the `isinstance` builtin, value-type proxy
    /// classes included.
    ///
    /// # Errors
    ///
    /// `TypeError` if `class` is not a class.
    pub fn is_instance(
        &self,
        value: GenericValue,
        class: GenericValue,
    ) -> Result<bool, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        let result =
            self.ffi_result(unsafe { (self.api.is_instance)(self.api.ctx, value, class) })?;
        Ok(self.as_bool(result).unwrap_or_default())
    }

    /// The class of an instance, as a class value (the analogue of
    /// `type(self)`). Call it to construct another instance of the same class,
    /// or pass it to [`Host::is_instance`] to type-check another argument
    /// before reading its opaque state.
    ///
    /// # Errors
    ///
    /// `TypeError` if `value` is not an instance.
    pub fn class_of(&self, value: GenericValue) -> Result<GenericValue, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        self.ffi_result(unsafe { (self.api.class_of)(self.api.ctx, value) })
    }

    // --- attributes (plain field access; never re-enters) ---

    /// A field of an instance.
    ///
    /// # Errors
    ///
    /// `AttributeError` if the field is absent, `TypeError` if the receiver
    /// is not an instance.
    pub fn attr_get(
        &self,
        receiver: GenericValue,
        name: &str,
    ) -> Result<GenericValue, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; the `FfiStr` borrows a live `&str`.
        self.ffi_result(unsafe { (self.api.attr_get)(self.api.ctx, receiver, Self::ffi_str(name)) })
    }

    /// Set a field on an instance.
    ///
    /// # Errors
    ///
    /// `TypeError` if the receiver is not an instance.
    pub fn attr_set(
        &self,
        receiver: GenericValue,
        name: &str,
        value: GenericValue,
    ) -> Result<(), PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; the `FfiStr` borrows a live `&str`.
        self.ffi_result(unsafe {
            (self.api.attr_set)(self.api.ctx, receiver, Self::ffi_str(name), value)
        })
        .map(|_| ())
    }

    /// Whether an instance has a field.
    ///
    /// # Errors
    ///
    /// `TypeError` if the receiver is not an instance.
    pub fn attr_has(&self, receiver: GenericValue, name: &str) -> Result<bool, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; the `FfiStr` borrows a live `&str`.
        let result = self.ffi_result(unsafe {
            (self.api.attr_has)(self.api.ctx, receiver, Self::ffi_str(name))
        })?;
        Ok(self.as_bool(result).unwrap_or_default())
    }

    // --- construct ---

    /// A new `nil` value.
    #[must_use]
    pub fn make_nil(&self) -> GenericValue {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        unsafe { (self.api.nil_new)(self.api.ctx) }
    }

    /// A new boolean value.
    #[must_use]
    pub fn make_bool(&self, value: bool) -> GenericValue {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        unsafe { (self.api.bool_new)(self.api.ctx, value) }
    }

    /// A new integer value.
    #[must_use]
    pub fn make_int(&self, value: i64) -> GenericValue {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        unsafe { (self.api.int_new)(self.api.ctx, value) }
    }

    /// A new float value.
    #[must_use]
    pub fn make_float(&self, value: f64) -> GenericValue {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        unsafe { (self.api.float_new)(self.api.ctx, value) }
    }

    /// Intern a string value.
    ///
    /// # Panics
    ///
    /// Panics if the host rejects the string, which cannot happen for Rust
    /// strings (they are always valid UTF-8).
    #[must_use]
    pub fn make_str(&self, value: &str) -> GenericValue {
        let ffi = FfiStr {
            ptr: value.as_ptr(),
            len: value.len(),
        };
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        self.ffi_result(unsafe { (self.api.string_new)(self.api.ctx, ffi) })
            .expect("host rejected a valid UTF-8 string")
    }

    /// A new, empty list.
    #[must_use]
    pub fn make_list(&self) -> GenericValue {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        unsafe { (self.api.list_new)(self.api.ctx) }
    }

    /// Append to a list value.
    ///
    /// # Errors
    ///
    /// `TypeError` if the target is not a list.
    pub fn list_push(&self, list: GenericValue, item: GenericValue) -> Result<(), PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        self.ffi_result(unsafe { (self.api.list_push)(self.api.ctx, list, item) })
            .map(|_| ())
    }

    /// Replace the element at an index.
    ///
    /// # Errors
    ///
    /// `TypeError` if the target is not a list; `IndexError` if the index
    /// is out of bounds.
    pub fn list_set(
        &self,
        list: GenericValue,
        index: usize,
        value: GenericValue,
    ) -> Result<(), PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        self.ffi_result(unsafe { (self.api.list_set)(self.api.ctx, list, index, value) })
            .map(|_| ())
    }

    /// A new exception instance of `class` (any class deriving from
    /// `Exception` - builtin or user-defined), ready to be thrown
    /// (returned inside [`PluginError::Exception`]) or passed to generic
    /// code. Sets the message directly, bypassing `__init__`. Prefer the
    /// typed constructors below for the common builtin-class case.
    ///
    /// # Errors
    ///
    /// `TypeError` if `class` is not a class deriving from `Exception`.
    pub fn make_exception(
        &self,
        class: GenericValue,
        message: &str,
    ) -> Result<GenericValue, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; the `FfiStr` borrows a live `&str`.
        self.ffi_result(unsafe {
            (self.api.exception_new)(self.api.ctx, class, Self::ffi_str(message))
        })
    }

    /// A [`PluginError`] carrying a fresh instance of the builtin
    /// exception class `class_name`. Unknown names fall back to the base
    /// `Exception` (unreachable through the typed constructors below). A
    /// fatal host error during construction stays [`PluginError::Fatal`] -
    /// it must never be downgraded to something catchable.
    fn error(&self, class_name: &str, message: &str) -> PluginError {
        let result = self
            .builtin(class_name)
            .and_then(|class| self.make_exception(class, message))
            .or_else(|error| {
                // Only fall back for catchable failures (an unknown class
                // name); a fatal host error must propagate as-is.
                if matches!(error, PluginError::Fatal) {
                    return Err(error);
                }
                let class = self.builtin("Exception")?;
                self.make_exception(class, message)
            });
        match result {
            Ok(exception) => PluginError::Exception(exception),
            Err(PluginError::Fatal) => PluginError::Fatal,
            // Unreachable with the real host (the base `Exception` always
            // exists and `builtin_get`/`exception_new` fail catchably at
            // worst), but a real nil keeps this a valid `Value`: if it ever
            // escaped, the host would reject the non-exception gracefully
            // rather than transmute an invalid blob.
            Err(_) => PluginError::Exception(self.make_nil()),
        }
    }

    host_error_constructors!(
        /// A [`PluginError`] carrying a fresh base `Exception` instance.
        exception => "Exception",
        /// A [`PluginError`] carrying a fresh `TypeError` instance.
        type_error => "TypeError",
        /// A [`PluginError`] carrying a fresh `ValueError` instance.
        value_error => "ValueError",
        /// A [`PluginError`] carrying a fresh `NameError` instance.
        name_error => "NameError",
        /// A [`PluginError`] carrying a fresh `ConstReassignmentError` instance.
        const_reassignment_error => "ConstReassignmentError",
        /// A [`PluginError`] carrying a fresh `AttributeError` instance.
        attribute_error => "AttributeError",
        /// A [`PluginError`] carrying a fresh `ImportError` instance.
        import_error => "ImportError",
        /// A [`PluginError`] carrying a fresh `AssertionError` instance.
        assertion_error => "AssertionError",
        /// A [`PluginError`] carrying a fresh `IoError` instance.
        io_error => "IoError",
        /// A [`PluginError`] carrying a fresh `KeyError` instance.
        key_error => "KeyError",
        /// A [`PluginError`] carrying a fresh `IndexError` instance.
        index_error => "IndexError",
    );

    // --- display ---

    /// The raw string representation of any value, as a string value.
    /// Does NOT honor a user class's `__str__` - see [`Host::to_str`].
    #[must_use]
    pub fn display(&self, value: GenericValue) -> GenericValue {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        unsafe { (self.api.value_display)(self.api.ctx, value) }
    }

    /// [`Host::display`], copied out as an owned Rust `String`.
    #[must_use]
    pub fn display_string(&self, value: GenericValue) -> String {
        let displayed = self.display(value);
        self.as_str(displayed).unwrap_or_default().to_owned()
    }

    // --- re-entering (run generic bytecode; GC may occur) ---

    /// Call a callable value with the given arguments.
    ///
    /// # Errors
    ///
    /// Returns the generic exception raised by the callee, if any.
    pub fn call(
        &mut self,
        callee: GenericValue,
        args: &[GenericValue],
    ) -> Result<GenericValue, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; `args` is a live slice.
        self.ffi_result(unsafe {
            (self.api.call_value)(self.api.ctx, callee, args.as_ptr(), args.len())
        })
    }

    /// Invoke a named method on a receiver.
    ///
    /// # Errors
    ///
    /// Returns the generic exception raised by the method, if any.
    pub fn invoke(
        &mut self,
        receiver: GenericValue,
        name: &str,
        args: &[GenericValue],
    ) -> Result<GenericValue, PluginError> {
        let name = FfiStr {
            ptr: name.as_ptr(),
            len: name.len(),
        };
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; `args` is a live slice.
        self.ffi_result(unsafe {
            (self.api.invoke_method)(self.api.ctx, receiver, name, args.as_ptr(), args.len())
        })
    }

    /// String conversion honoring a user class's `__str__`.
    ///
    /// # Errors
    ///
    /// Returns the generic exception raised by `__str__`, if any.
    pub fn to_str(&mut self, value: GenericValue) -> Result<GenericValue, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        self.ffi_result(unsafe { (self.api.value_str)(self.api.ctx, value) })
    }

    /// Look up a key in a dict.
    ///
    /// # Errors
    ///
    /// `KeyError` if absent, `TypeError` for unusable targets/keys, or any
    /// exception raised by `__hash__`/`__eq__`.
    pub fn dict_get(
        &mut self,
        dict: GenericValue,
        key: GenericValue,
    ) -> Result<GenericValue, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        self.ffi_result(unsafe { (self.api.dict_get)(self.api.ctx, dict, key) })
    }

    /// Insert or replace a key in a dict.
    ///
    /// # Errors
    ///
    /// `TypeError` for unusable targets/keys, or any exception raised by
    /// `__hash__`/`__eq__`.
    pub fn dict_set(
        &mut self,
        dict: GenericValue,
        key: GenericValue,
        value: GenericValue,
    ) -> Result<(), PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        self.ffi_result(unsafe { (self.api.dict_set)(self.api.ctx, dict, key, value) })
            .map(|_| ())
    }

    /// Whether a dict contains a key.
    ///
    /// # Errors
    ///
    /// `TypeError` for unusable targets/keys, or any exception raised by
    /// `__hash__`/`__eq__`.
    pub fn dict_contains(
        &mut self,
        dict: GenericValue,
        key: GenericValue,
    ) -> Result<bool, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        let value =
            self.ffi_result(unsafe { (self.api.dict_contains)(self.api.ctx, dict, key) })?;
        Ok(self.as_bool(value).unwrap_or_default())
    }

    /// Add an item to a set.
    ///
    /// # Errors
    ///
    /// `TypeError` for unusable targets/items, or any exception raised by
    /// `__hash__`/`__eq__`.
    pub fn set_add(&mut self, set: GenericValue, item: GenericValue) -> Result<(), PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        self.ffi_result(unsafe { (self.api.set_add)(self.api.ctx, set, item) })
            .map(|_| ())
    }

    /// Whether a set contains an item.
    ///
    /// # Errors
    ///
    /// `TypeError` for unusable targets/items, or any exception raised by
    /// `__hash__`/`__eq__`.
    pub fn set_contains(
        &mut self,
        set: GenericValue,
        item: GenericValue,
    ) -> Result<bool, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        let value = self.ffi_result(unsafe { (self.api.set_contains)(self.api.ctx, set, item) })?;
        Ok(self.as_bool(value).unwrap_or_default())
    }

    /// Truthiness honoring `__bool__`.
    ///
    /// # Errors
    ///
    /// Returns the generic exception raised by `__bool__`, if any.
    pub fn truthy(&mut self, value: GenericValue) -> Result<bool, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        let result = self.ffi_result(unsafe { (self.api.value_truthy)(self.api.ctx, value) })?;
        Ok(self.as_bool(result).unwrap_or_default())
    }

    /// Equality honoring `__eq__`.
    ///
    /// # Errors
    ///
    /// Returns the generic exception raised by `__eq__`, if any.
    pub fn equals(&mut self, a: GenericValue, b: GenericValue) -> Result<bool, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        let result = self.ffi_result(unsafe { (self.api.value_equals)(self.api.ctx, a, b) })?;
        Ok(self.as_bool(result).unwrap_or_default())
    }

    /// Hash honoring `__hash__`.
    ///
    /// # Errors
    ///
    /// Returns the generic exception raised by `__hash__`, if any.
    pub fn hash(&mut self, value: GenericValue) -> Result<i64, PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        let result = self.ffi_result(unsafe { (self.api.value_hash)(self.api.ctx, value) })?;
        Ok(self.as_int(result).unwrap_or_default())
    }

    // --- rooting ---

    /// Keep a value alive across re-entering calls for the rest of this
    /// plugin call (the host releases all roots automatically on return).
    /// Prefer the RAII form, [`Host::rooted`].
    pub fn root(&self, value: GenericValue) {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        unsafe { (self.api.root)(self.api.ctx, value) };
    }

    /// Release the `n` most recent roots early. Releasing more roots than
    /// were pushed corrupts interpreter state; prefer the RAII form,
    /// [`Host::rooted`].
    pub fn unroot(&self, n: usize) {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        unsafe { (self.api.unroot)(self.api.ctx, n) };
    }

    /// Root a value for the lifetime of the returned guard.
    ///
    /// Guards release in LIFO order - drop them in reverse order of
    /// creation (scopes do this naturally).
    #[must_use]
    pub fn rooted(&self, value: GenericValue) -> Rooted<'a> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        unsafe { (self.api.root)(self.api.ctx, value) };
        Rooted {
            api: self.api,
            value,
        }
    }

    // --- plugin instance state ---

    /// Install the plugin's opaque pointer on a plugin-backed instance.
    ///
    /// Typically called from `__init__` with `args[0]` (the receiver) and a
    /// `Box::into_raw(state)` pointer. The class's `drop` callback is called
    /// with this pointer when the instance is garbage-collected.
    ///
    /// Overwriting an already-installed pointer leaks the previous one: the
    /// host does not run `drop` on it, since it cannot know whether the plugin
    /// still holds a copy elsewhere. If a plugin means to replace state, it must
    /// [`Host::get_opaque`] and free the old pointer itself first.
    ///
    /// # Safety
    ///
    /// Installing a pointer hands it to the garbage collector, which will later
    /// pass it to this class's `traverse` (during marking) and `drop` (when the
    /// instance dies) - so `ptr` must be null, or a pointer those two callbacks
    /// can soundly consume: of the type they cast to, valid until `drop` runs,
    /// and owned, since `drop` will free it exactly once. A pointer to a
    /// short-lived local, one of another class's type, or one something else
    /// also frees turns a later collection into undefined behavior.
    ///
    /// # Errors
    ///
    /// `TypeError` if `receiver` is not a plugin-backed instance.
    pub unsafe fn set_opaque(
        &self,
        receiver: GenericValue,
        ptr: *mut c_void,
    ) -> Result<(), PluginError> {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        self.ffi_result(unsafe { (self.api.instance_set_opaque)(self.api.ctx, receiver, ptr) })
            .map(|_| ())
    }

    /// Recover the pointer installed by [`Host::set_opaque`], or null if none
    /// was installed (e.g. before `__init__` ran) or `receiver` is not a
    /// plugin-backed instance. Never raises.
    #[must_use]
    pub fn get_opaque(&self, receiver: GenericValue) -> *mut c_void {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        unsafe { (self.api.instance_get_opaque)(self.api.ctx, receiver) }
    }

    /// Typed mutable view of the opaque pointer, or `None` if it is null or
    /// `receiver` is not a plugin-backed instance.
    ///
    /// # Safety
    ///
    /// The caller must ensure `T` is the correct type for this instance's
    /// opaque state. The reference is valid while the instance is alive (the
    /// GC will not collect it while the plugin holds the instance value).
    // The `&mut T` derives from the opaque `*mut` the plugin installed, not
    // from `&self`; the shared borrow only scopes the call, so the plugin can
    // mutate its own per-instance state through a `&Host`.
    #[allow(clippy::mut_from_ref)]
    #[must_use]
    pub unsafe fn opaque_ref<T>(&self, receiver: GenericValue) -> Option<&mut T> {
        let ptr = self.get_opaque(receiver).cast::<T>();
        // SAFETY: guaranteed by the caller (see the `# Safety` section).
        unsafe { ptr.as_mut() }
    }

    const fn ffi_str(s: &str) -> FfiStr {
        FfiStr {
            ptr: s.as_ptr(),
            len: s.len(),
        }
    }

    fn ffi_result(&self, ret: FfiReturn) -> Result<GenericValue, PluginError> {
        match FfiStatus::from_u32(ret.status) {
            Some(FfiStatus::Ok) => Ok(ret.value),
            Some(FfiStatus::Exception) => Err(PluginError::Exception(ret.value)),
            Some(FfiStatus::Fatal) => Err(PluginError::Fatal),
            // A status outside the enum is a protocol violation.
            None => Err(self.protocol_violation(&format!(
                "host callback returned unknown status {}",
                ret.status
            ))),
        }
    }

    /// Builds the protocol-violation exception without going through
    /// [`Self::ffi_result`]: a host broken enough to answer `builtin_get`/
    /// `exception_new` with unknown statuses too would otherwise recurse
    /// through error construction forever. Any failure here falls back to
    /// a nil-carrying exception rather than another decode attempt.
    fn protocol_violation(&self, message: &str) -> PluginError {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; the `FfiStr` borrows a live `&str`.
        let class = unsafe { (self.api.builtin_get)(self.api.ctx, Self::ffi_str("Exception")) };
        if FfiStatus::from_u32(class.status) == Some(FfiStatus::Ok) {
            // SAFETY: `Host`'s invariant - the vtable and its own `ctx`; the `FfiStr` borrows a live `&str`.
            let exception = unsafe {
                (self.api.exception_new)(self.api.ctx, class.value, Self::ffi_str(message))
            };
            if FfiStatus::from_u32(exception.status) == Some(FfiStatus::Ok) {
                return PluginError::Exception(exception.value);
            }
        }
        PluginError::Exception(self.make_nil())
    }
}

/// RAII guard for a rooted value; see [`Host::rooted`].
///
/// Holds the vtable (not the [`Host`] borrow), so re-entering `&mut Host`
/// methods remain callable while guards are alive.
///
/// # Invariant
///
/// `api` is a vtable satisfying [`Host`]'s invariant - guards are only ever
/// created by [`Host::rooted`], which copies it out of a live [`Host`].
#[derive(Debug)]
pub struct Rooted<'a> {
    api: &'a HostApi,
    value: GenericValue,
}

impl Rooted<'_> {
    /// The rooted value.
    #[must_use]
    pub const fn get(&self) -> GenericValue {
        self.value
    }
}

impl Drop for Rooted<'_> {
    fn drop(&mut self) {
        // SAFETY: `Host`'s invariant - the vtable and its own `ctx`.
        unsafe { (self.api.unroot)(self.api.ctx, 1) };
    }
}

/// Signature of a Rust plugin function used with `export_module!`.
pub type RustPluginFn = fn(&mut Host<'_>, &[GenericValue]) -> Result<GenericValue, PluginError>;

/// Implementation detail of `export_module!`: runs a Rust plugin function
/// behind `catch_unwind` and maps the outcome to an [`FfiReturn`].
///
/// # Safety
///
/// `host` must point to a valid [`HostApi`] and `args` to `nargs`
/// contiguous values, both valid for the duration of the call - which is
/// what the interpreter guarantees when calling an exported plugin function.
#[doc(hidden)]
pub unsafe fn __invoke_plugin_fn(
    fun: RustPluginFn,
    host: *const HostApi,
    args: *const GenericValue,
    nargs: usize,
) -> FfiReturn {
    // SAFETY: guaranteed by the caller, see above.
    let api = unsafe { &*host };
    let args: &[GenericValue] = if nargs == 0 {
        &[]
    } else {
        // SAFETY: guaranteed by the caller, see above.
        unsafe { slice::from_raw_parts(args, nargs) }
    };
    // SAFETY: `api` is the vtable the caller vouched for, valid for this
    // call - exactly `Host`'s invariant.
    let mut host = unsafe { Host::new(api) };

    let result = panic::catch_unwind(AssertUnwindSafe(|| fun(&mut host, args)));

    finish_plugin_invoke(&host, result)
}

/// Signature of a Rust plugin method used with `export_module!`. The receiver
/// (`self`) is a separate parameter; `args` are the remaining arguments only.
pub type RustPluginMethodFn =
    fn(&mut Host, GenericValue, &[GenericValue]) -> Result<GenericValue, PluginError>;

/// Implementation detail of `export_module!`: the method counterpart of
/// [`__invoke_plugin_fn`], threading the receiver through as a separate value.
///
/// # Safety
///
/// As [`__invoke_plugin_fn`]: `host` and `args`/`nargs` must be valid for the
/// call. `receiver` is a bit-copy of the receiver value.
#[doc(hidden)]
pub unsafe fn __invoke_plugin_method_fn(
    fun: RustPluginMethodFn,
    host: *const HostApi,
    receiver: GenericValue,
    args: *const GenericValue,
    nargs: usize,
) -> FfiReturn {
    // SAFETY: guaranteed by the caller, see above.
    let api = unsafe { &*host };
    let args: &[GenericValue] = if nargs == 0 {
        &[]
    } else {
        // SAFETY: guaranteed by the caller, see above.
        unsafe { slice::from_raw_parts(args, nargs) }
    };
    // SAFETY: as in `__invoke_plugin_fn`: the caller's vtable is valid for
    // this call, which is `Host`'s invariant.
    let mut host = unsafe { Host::new(api) };

    let result = panic::catch_unwind(AssertUnwindSafe(|| fun(&mut host, receiver, args)));

    finish_plugin_invoke(&host, result)
}

/// Signature of a Rust plugin value creator used with `export_module!`:
/// builds one module constant at import time.
pub type RustPluginValueFn = fn(&mut Host) -> Result<GenericValue, PluginError>;

/// Implementation detail of `export_module!`: the value-creator counterpart
/// of [`__invoke_plugin_fn`].
///
/// # Safety
///
/// `host` must point to a valid [`HostApi`], valid for the duration of the
/// call - which is what the interpreter guarantees when importing the
/// plugin module.
#[doc(hidden)]
pub unsafe fn __invoke_plugin_value_fn(fun: RustPluginValueFn, host: *const HostApi) -> FfiReturn {
    // SAFETY: guaranteed by the caller, see above.
    let api = unsafe { &*host };
    // SAFETY: as in `__invoke_plugin_fn`: the caller's vtable is valid for
    // this call, which is `Host`'s invariant.
    let mut host = unsafe { Host::new(api) };

    let result = panic::catch_unwind(AssertUnwindSafe(|| fun(&mut host)));

    finish_plugin_invoke(&host, result)
}

/// Map a caught plugin invocation outcome to an [`FfiReturn`], turning a panic
/// into a catchable base `Exception` so nothing unwinds across the C ABI.
fn finish_plugin_invoke(
    host: &Host,
    result: std::thread::Result<Result<GenericValue, PluginError>>,
) -> FfiReturn {
    match result {
        Ok(Ok(value)) => FfiReturn {
            status: FfiStatus::Ok as u32,
            value,
        },
        Ok(Err(error)) => error_return(host, error),
        Err(panic) => {
            let message = panic
                .downcast_ref::<&str>()
                .map(ToString::to_string)
                .or_else(|| panic.downcast_ref::<String>().cloned())
                .unwrap_or_else(|| "plugin function panicked".to_owned());
            error_return(host, host.exception(&format!("panic: {message}")))
        }
    }
}

fn error_return(host: &Host, error: PluginError) -> FfiReturn {
    match error {
        PluginError::Exception(value) => FfiReturn {
            status: FfiStatus::Exception as u32,
            value,
        },
        // The value is never read for a fatal status; a real nil keeps it a
        // valid `Value` rather than a zeroed blob that is not one.
        PluginError::Fatal => FfiReturn {
            status: FfiStatus::Fatal as u32,
            value: host.make_nil(),
        },
    }
}
