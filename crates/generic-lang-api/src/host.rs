//! Safe wrapper around the host vtable for Rust plugin authors.

use crate::abi::{FfiReturn, FfiStatus, FfiStr, GenericValue, HostApi};
use crate::{PluginError, ValueKind};

/// Generates the typed error constructors on [`Host`]: one per builtin
/// exception class, with the class name spelled exactly once — a plugin
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
/// rooting contract's string rule for you — any [`&str`](str) obtained from
/// the host borrows `self` and cannot be held across a re-entering call.
/// Values you want to keep across a re-entering call must be rooted, e.g.
/// via [`Host::rooted`].
pub struct Host<'a> {
    api: &'a HostApi,
}

/// A decoded view of a [`GenericValue`], obtained via [`Host::decode`].
#[derive(Debug, Clone, Copy)]
pub enum ArgValue<'h> {
    Nil,
    Bool(bool),
    Int(i64),
    /// An integer that does not fit in an `i64`; convert via
    /// [`Host::display`] if a textual form suffices.
    BigInt(GenericValue),
    Float(f64),
    Rational(GenericValue),
    Str(&'h str),
    List(GenericValue),
    Tuple(GenericValue),
    Dict(GenericValue),
    Set(GenericValue),
    Range(GenericValue),
    /// The exhausted-iterator sentinel.
    StopIteration,
    /// A plain class instance ([`Host::attr_get`], [`Host::invoke`]).
    Instance(GenericValue),
    /// A class ([`Host::call`] instantiates).
    Class(GenericValue),
    /// A callable ([`Host::call`]).
    Function(GenericValue),
    Module(GenericValue),
    /// An exception instance.
    Exception(GenericValue),
    Generator(GenericValue),
    /// An iterator (drive via [`Host::invoke`] with `__next__`).
    Iterator(GenericValue),
    /// VM-internal values a plugin should never meaningfully receive.
    Other(GenericValue),
}

impl<'a> Host<'a> {
    /// Wrap a host vtable. Called by the `export_module!` glue.
    #[doc(hidden)]
    #[must_use]
    pub const fn new(api: &'a HostApi) -> Self {
        Self { api }
    }

    // --- inspect ---

    /// The [`ValueKind`] of a value.
    #[must_use]
    pub fn kind(&self, value: GenericValue) -> ValueKind {
        ValueKind::from_u32((self.api.value_kind)(self.api.ctx, value))
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
        (self.api.bool_get)(self.api.ctx, value, &raw mut out).then_some(out)
    }

    /// The value as an `i64`; `None` if it is not an integer or does not
    /// fit in an `i64` (big integers — fall back to `display`).
    #[must_use]
    pub fn as_int(&self, value: GenericValue) -> Option<i64> {
        let mut out = 0i64;
        (self.api.int_get)(self.api.ctx, value, &raw mut out).then_some(out)
    }

    /// `None` if the value is not a float.
    #[must_use]
    pub fn as_float(&self, value: GenericValue) -> Option<f64> {
        let mut out = 0f64;
        (self.api.float_get)(self.api.ctx, value, &raw mut out).then_some(out)
    }

    /// The contents of a string value; `None` if the value is not a string.
    ///
    /// The returned string borrows the host and therefore cannot be held
    /// across a re-entering call (`&mut self` methods) — by design. This is
    /// rejected by the compiler:
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
    /// Copy the string out (`.to_owned()`) before re-entering if you need it
    /// afterwards.
    #[must_use]
    pub fn as_str(&self, value: GenericValue) -> Option<&str> {
        let mut out = FfiStr::null();
        if !(self.api.string_get)(self.api.ctx, value, &raw mut out) {
            return None;
        }
        // SAFETY: on success the host wrote a pointer to `len` initialized
        // bytes of interned string data, stable until the next re-entering
        // callback, which `&mut self` methods make unreachable while the
        // returned borrow lives.
        let bytes = unsafe { core::slice::from_raw_parts(out.ptr, out.len) };
        // Host strings are always UTF-8; checked anyway as defense in depth.
        core::str::from_utf8(bytes).ok()
    }

    /// `None` if the value is not a list.
    #[must_use]
    pub fn list_len(&self, value: GenericValue) -> Option<usize> {
        let mut out = 0usize;
        (self.api.list_len)(self.api.ctx, value, &raw mut out).then_some(out)
    }

    /// The list element at `index`.
    ///
    /// # Errors
    ///
    /// `TypeError` if the value is not a list; `IndexError` if the index
    /// is out of bounds.
    pub fn list_get(&self, value: GenericValue, index: usize) -> Result<GenericValue, PluginError> {
        self.ffi_result((self.api.list_get)(self.api.ctx, value, index))
    }

    /// `None` if the value is not a tuple.
    #[must_use]
    pub fn tuple_len(&self, value: GenericValue) -> Option<usize> {
        let mut out = 0usize;
        (self.api.tuple_len)(self.api.ctx, value, &raw mut out).then_some(out)
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
        self.ffi_result((self.api.tuple_get)(self.api.ctx, value, index))
    }

    /// `None` if the value is not a dict.
    #[must_use]
    pub fn dict_len(&self, value: GenericValue) -> Option<usize> {
        let mut out = 0usize;
        (self.api.dict_len)(self.api.ctx, value, &raw mut out).then_some(out)
    }

    /// `None` if the value is not a set.
    #[must_use]
    pub fn set_len(&self, value: GenericValue) -> Option<usize> {
        let mut out = 0usize;
        (self.api.set_len)(self.api.ctx, value, &raw mut out).then_some(out)
    }

    /// Look up a builtin global by name — exception classes like
    /// `"TypeError"`, native classes, builtin functions.
    ///
    /// # Errors
    ///
    /// `NameError` if absent.
    pub fn builtin(&self, name: &str) -> Result<GenericValue, PluginError> {
        self.ffi_result((self.api.builtin_get)(self.api.ctx, Self::ffi_str(name)))
    }

    /// Whether `value` is an instance of `class` or of a subclass of it —
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
        let result = self.ffi_result((self.api.is_instance)(self.api.ctx, value, class))?;
        Ok(self.as_bool(result).unwrap_or_default())
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
        self.ffi_result((self.api.attr_get)(
            self.api.ctx,
            receiver,
            Self::ffi_str(name),
        ))
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
        self.ffi_result((self.api.attr_set)(
            self.api.ctx,
            receiver,
            Self::ffi_str(name),
            value,
        ))
        .map(|_| ())
    }

    /// Whether an instance has a field.
    ///
    /// # Errors
    ///
    /// `TypeError` if the receiver is not an instance.
    pub fn attr_has(&self, receiver: GenericValue, name: &str) -> Result<bool, PluginError> {
        let result = self.ffi_result((self.api.attr_has)(
            self.api.ctx,
            receiver,
            Self::ffi_str(name),
        ))?;
        Ok(self.as_bool(result).unwrap_or_default())
    }

    // --- construct ---

    #[must_use]
    pub fn make_nil(&self) -> GenericValue {
        (self.api.nil_new)(self.api.ctx)
    }

    #[must_use]
    pub fn make_bool(&self, value: bool) -> GenericValue {
        (self.api.bool_new)(self.api.ctx, value)
    }

    #[must_use]
    pub fn make_int(&self, value: i64) -> GenericValue {
        (self.api.int_new)(self.api.ctx, value)
    }

    #[must_use]
    pub fn make_float(&self, value: f64) -> GenericValue {
        (self.api.float_new)(self.api.ctx, value)
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
        self.ffi_result((self.api.string_new)(self.api.ctx, ffi))
            .expect("host rejected a valid UTF-8 string")
    }

    /// A new, empty list.
    #[must_use]
    pub fn make_list(&self) -> GenericValue {
        (self.api.list_new)(self.api.ctx)
    }

    /// Append to a list value.
    ///
    /// # Errors
    ///
    /// `TypeError` if the target is not a list.
    pub fn list_push(&self, list: GenericValue, item: GenericValue) -> Result<(), PluginError> {
        self.ffi_result((self.api.list_push)(self.api.ctx, list, item))
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
        self.ffi_result((self.api.list_set)(self.api.ctx, list, index, value))
            .map(|_| ())
    }

    /// A new exception instance of `class` (any class deriving from
    /// `Exception` — builtin or user-defined), ready to be thrown
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
        self.ffi_result((self.api.exception_new)(
            self.api.ctx,
            class,
            Self::ffi_str(message),
        ))
    }

    /// A [`PluginError`] carrying a fresh instance of the builtin
    /// exception class `class_name`. Unknown names fall back to the base
    /// `Exception` (unreachable through the typed constructors below).
    fn error(&self, class_name: &str, message: &str) -> PluginError {
        self.builtin(class_name)
            .and_then(|class| self.make_exception(class, message))
            .or_else(|_| {
                let class = self.builtin("Exception")?;
                self.make_exception(class, message)
            })
            .map_or_else(
                // Unreachable (the base `Exception` always exists), but a
                // real nil keeps this a valid `Value`: if it ever escaped,
                // the host would reject the non-exception gracefully rather
                // than transmute an invalid blob.
                |_| PluginError::Exception(self.make_nil()),
                PluginError::Exception,
            )
    }

    host_error_constructors!(
        /// The base `Exception` class.
        exception => "Exception",
        type_error => "TypeError",
        value_error => "ValueError",
        name_error => "NameError",
        const_reassignment_error => "ConstReassignmentError",
        attribute_error => "AttributeError",
        import_error => "ImportError",
        assertion_error => "AssertionError",
        io_error => "IoError",
        key_error => "KeyError",
        index_error => "IndexError",
    );

    // --- display ---

    /// The raw string representation of any value, as a string value.
    /// Does NOT honor a user class's `__str__` — see [`Host::to_str`].
    #[must_use]
    pub fn display(&self, value: GenericValue) -> GenericValue {
        (self.api.value_display)(self.api.ctx, value)
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
        self.ffi_result((self.api.call_value)(
            self.api.ctx,
            callee,
            args.as_ptr(),
            args.len(),
        ))
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
        self.ffi_result((self.api.invoke_method)(
            self.api.ctx,
            receiver,
            name,
            args.as_ptr(),
            args.len(),
        ))
    }

    /// String conversion honoring a user class's `__str__`.
    ///
    /// # Errors
    ///
    /// Returns the generic exception raised by `__str__`, if any.
    pub fn to_str(&mut self, value: GenericValue) -> Result<GenericValue, PluginError> {
        self.ffi_result((self.api.value_str)(self.api.ctx, value))
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
        self.ffi_result((self.api.dict_get)(self.api.ctx, dict, key))
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
        self.ffi_result((self.api.dict_set)(self.api.ctx, dict, key, value))
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
        let value = self.ffi_result((self.api.dict_contains)(self.api.ctx, dict, key))?;
        Ok(self.as_bool(value).unwrap_or_default())
    }

    /// Add an item to a set.
    ///
    /// # Errors
    ///
    /// `TypeError` for unusable targets/items, or any exception raised by
    /// `__hash__`/`__eq__`.
    pub fn set_add(&mut self, set: GenericValue, item: GenericValue) -> Result<(), PluginError> {
        self.ffi_result((self.api.set_add)(self.api.ctx, set, item))
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
        let value = self.ffi_result((self.api.set_contains)(self.api.ctx, set, item))?;
        Ok(self.as_bool(value).unwrap_or_default())
    }

    /// Truthiness honoring `__bool__`.
    ///
    /// # Errors
    ///
    /// Returns the generic exception raised by `__bool__`, if any.
    pub fn truthy(&mut self, value: GenericValue) -> Result<bool, PluginError> {
        let result = self.ffi_result((self.api.value_truthy)(self.api.ctx, value))?;
        Ok(self.as_bool(result).unwrap_or_default())
    }

    /// Equality honoring `__eq__`.
    ///
    /// # Errors
    ///
    /// Returns the generic exception raised by `__eq__`, if any.
    pub fn equals(&mut self, a: GenericValue, b: GenericValue) -> Result<bool, PluginError> {
        let result = self.ffi_result((self.api.value_equals)(self.api.ctx, a, b))?;
        Ok(self.as_bool(result).unwrap_or_default())
    }

    /// Hash honoring `__hash__`.
    ///
    /// # Errors
    ///
    /// Returns the generic exception raised by `__hash__`, if any.
    pub fn hash(&mut self, value: GenericValue) -> Result<i64, PluginError> {
        let result = self.ffi_result((self.api.value_hash)(self.api.ctx, value))?;
        Ok(self.as_int(result).unwrap_or_default())
    }

    // --- rooting ---

    /// Keep a value alive across re-entering calls for the rest of this
    /// plugin call (the host releases all roots automatically on return).
    /// Prefer the RAII form, [`Host::rooted`].
    pub fn root(&self, value: GenericValue) {
        (self.api.root)(self.api.ctx, value);
    }

    /// Release the `n` most recent roots early.
    pub fn unroot(&self, n: usize) {
        (self.api.unroot)(self.api.ctx, n);
    }

    /// Root a value for the lifetime of the returned guard.
    ///
    /// Guards release in LIFO order — drop them in reverse order of
    /// creation (scopes do this naturally).
    #[must_use]
    pub fn rooted(&self, value: GenericValue) -> Rooted<'a> {
        (self.api.root)(self.api.ctx, value);
        Rooted {
            api: self.api,
            value,
        }
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
            // The host only produces valid statuses.
            None => Err(self.exception(&format!(
                "host callback returned unknown status {}",
                ret.status
            ))),
        }
    }
}

/// RAII guard for a rooted value; see [`Host::rooted`].
///
/// Holds the vtable (not the [`Host`] borrow), so re-entering `&mut Host`
/// methods remain callable while guards are alive — which is the point.
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
        (self.api.unroot)(self.api.ctx, 1);
    }
}

/// Signature of a Rust plugin function used with `export_module!`.
pub type RustPluginFn = fn(&mut Host, &[GenericValue]) -> Result<GenericValue, PluginError>;

/// Implementation detail of `export_module!`: runs a Rust plugin function
/// behind `catch_unwind` and maps the outcome to an [`FfiReturn`].
///
/// # Safety
///
/// `host` must point to a valid [`HostApi`] and `args` to `nargs`
/// contiguous values, both valid for the duration of the call — which is
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
        unsafe { core::slice::from_raw_parts(args, nargs) }
    };

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let mut host = Host::new(api);
        fun(&mut host, args)
    }));

    let host = Host::new(api);
    match result {
        Ok(Ok(value)) => FfiReturn {
            status: FfiStatus::Ok as u32,
            value,
        },
        Ok(Err(error)) => error_return(&host, error),
        Err(panic) => {
            let message = panic
                .downcast_ref::<&str>()
                .map(ToString::to_string)
                .or_else(|| panic.downcast_ref::<String>().cloned())
                .unwrap_or_else(|| "plugin function panicked".to_owned());
            error_return(&host, host.exception(&format!("panic: {message}")))
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
