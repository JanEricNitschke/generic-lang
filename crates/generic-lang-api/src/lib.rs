//! # Plugin ABI and authoring API for the generic programming language
//!
//! Native modules for generic are shared libraries speaking the C ABI
//! defined in [`abi`]. Rust plugin authors use the safe layer in this crate
//! root; other languages use the generated `include/generic_plugin.h`.

pub mod abi;
mod export;
mod host;

pub use abi::{
    FfiReturn, FfiStr, FunctionDesc, GENERIC_PLUGIN_ABI_VERSION, GenericValue, HostApi, ModuleDesc,
    PluginFn,
};
pub use host::{__invoke_plugin_fn, ArgValue, Host, Rooted, RustPluginFn};

/// Exception-kind codes carried in [`FfiReturn::status`].
///
/// `0` is reserved for "no exception". The nonzero codes duplicate the
/// discriminants of the interpreter's `ExceptionKind` enum
/// (`crates/generic-lang-lib/src/vm/exception_handling.rs`), which is the
/// source of truth; a sync test in the interpreter crate guards the
/// duplication. Unknown codes are treated as the base `Exception`.
pub mod exception_code {
    /// The base exception class; also what runtime errors throw.
    pub const EXCEPTION: u32 = 1;
    pub const TYPE_ERROR: u32 = 2;
    pub const VALUE_ERROR: u32 = 3;
    pub const NAME_ERROR: u32 = 4;
    pub const CONST_REASSIGNMENT_ERROR: u32 = 5;
    pub const ATTRIBUTE_ERROR: u32 = 6;
    pub const IMPORT_ERROR: u32 = 7;
    pub const ASSERTION_ERROR: u32 = 8;
    pub const IO_ERROR: u32 = 9;
    pub const KEY_ERROR: u32 = 10;
    pub const INDEX_ERROR: u32 = 11;
}

/// Value-kind codes returned by [`HostApi::value_kind`].
///
/// A coverage test in the interpreter crate guards that every host `Value`
/// maps to one of these.
pub mod value_kind {
    /// The `nil` value.
    pub const NIL: u32 = 0;
    /// A boolean.
    pub const BOOL: u32 = 1;
    /// An integer that fits in an `i64` (`int_get` succeeds).
    pub const INT: u32 = 2;
    /// An integer that does not fit in an `i64` (`int_get` fails; use
    /// `value_display`/`value_str`).
    pub const BIG_INT: u32 = 3;
    /// A float.
    pub const FLOAT: u32 = 4;
    /// A rational number.
    pub const RATIONAL: u32 = 5;
    /// A string (`string_get` succeeds).
    pub const STRING: u32 = 6;
    /// A list (`list_len`/`list_get` succeed).
    pub const LIST: u32 = 7;
    /// A tuple.
    pub const TUPLE: u32 = 8;
    /// A dict.
    pub const DICT: u32 = 9;
    /// A set.
    pub const SET: u32 = 10;
    /// A range.
    pub const RANGE: u32 = 11;
    /// The `StopIteration` sentinel — what `__next__` returns when an
    /// iterator is exhausted.
    pub const STOP_ITERATION: u32 = 12;
    /// A plain class instance (fields via `attr_get`/`attr_set`, methods
    /// via `invoke_method`).
    pub const INSTANCE: u32 = 13;
    /// A class (instantiate via `call_value`).
    pub const CLASS: u32 = 14;
    /// A callable function value (closure, native function, or bound
    /// method) — use `call_value`.
    pub const FUNCTION: u32 = 15;
    /// A module.
    pub const MODULE: u32 = 16;
    /// An exception instance.
    pub const EXCEPTION: u32 = 17;
    /// A generator.
    pub const GENERATOR: u32 = 18;
    /// A list/tuple/range/template iterator (drive via `invoke_method`
    /// with `__next__`).
    pub const ITERATOR: u32 = 19;
    /// VM-internal values a plugin should never meaningfully receive.
    pub const OTHER: u32 = 20;
}

/// An error to be thrown into the generic VM as an exception.
///
/// Construct one with the typed constructors, or convert from a plain
/// string/`String` (which throws the base `Exception`):
///
/// ```
/// use generic_lang_api::PluginError;
///
/// let type_error = PluginError::type_error("expected a number");
/// let base: PluginError = "something went wrong".into();
/// assert_eq!(base.kind, generic_lang_api::exception_code::EXCEPTION);
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PluginError {
    /// An [`exception_code`] value.
    pub kind: u32,
    pub message: String,
}

macro_rules! plugin_error_constructors {
    ($($(#[$doc:meta])* $name:ident => $code:ident),* $(,)?) => {
        $(
            $(#[$doc])*
            #[must_use]
            pub fn $name(message: impl Into<String>) -> Self {
                Self {
                    kind: exception_code::$code,
                    message: message.into(),
                }
            }
        )*
    };
}

impl PluginError {
    plugin_error_constructors!(
        /// The base `Exception` class.
        exception => EXCEPTION,
        type_error => TYPE_ERROR,
        value_error => VALUE_ERROR,
        name_error => NAME_ERROR,
        const_reassignment_error => CONST_REASSIGNMENT_ERROR,
        attribute_error => ATTRIBUTE_ERROR,
        import_error => IMPORT_ERROR,
        assertion_error => ASSERTION_ERROR,
        io_error => IO_ERROR,
        key_error => KEY_ERROR,
        index_error => INDEX_ERROR,
    );
}

impl From<String> for PluginError {
    fn from(message: String) -> Self {
        Self::exception(message)
    }
}

impl From<&str> for PluginError {
    fn from(message: &str) -> Self {
        Self::exception(message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn abi_type_layout() {
        assert_eq!(size_of::<GenericValue>(), 32);
        assert_eq!(align_of::<GenericValue>(), 8);
        assert_eq!(size_of::<FfiReturn>(), 40);
        assert_eq!(size_of::<FfiStr>(), 2 * size_of::<usize>());
    }

    #[test]
    fn string_error_defaults_to_base_exception() {
        let err: PluginError = String::from("boom").into();
        assert_eq!(err.kind, exception_code::EXCEPTION);
        assert_eq!(
            PluginError::index_error("i").kind,
            exception_code::INDEX_ERROR
        );
    }
}
