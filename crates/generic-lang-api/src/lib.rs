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
/// `0` is reserved for "no exception" and is deliberately not a variant.
/// The discriminants duplicate the interpreter's `ExceptionKind` enum
/// (`crates/generic-lang-lib/src/vm/exception_handling.rs`), which is the
/// source of truth; a sync test in the interpreter crate guards the
/// duplication. Unknown codes are treated as the base `Exception`.
///
/// Over the FFI these travel as plain `u32` — convert with `as u32` /
/// [`ExceptionCode::from_u32`].
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExceptionCode {
    /// The base exception class; also what runtime errors throw.
    Exception = 1,
    TypeError = 2,
    ValueError = 3,
    NameError = 4,
    ConstReassignmentError = 5,
    AttributeError = 6,
    ImportError = 7,
    AssertionError = 8,
    IoError = 9,
    KeyError = 10,
    IndexError = 11,
}

impl ExceptionCode {
    /// Decode a nonzero [`FfiReturn::status`]. Unknown codes (including a
    /// stray `0`, which callers should have handled as "no exception") map
    /// to the base [`ExceptionCode::Exception`].
    #[must_use]
    pub const fn from_u32(code: u32) -> Self {
        match code {
            2 => Self::TypeError,
            3 => Self::ValueError,
            4 => Self::NameError,
            5 => Self::ConstReassignmentError,
            6 => Self::AttributeError,
            7 => Self::ImportError,
            8 => Self::AssertionError,
            9 => Self::IoError,
            10 => Self::KeyError,
            11 => Self::IndexError,
            _ => Self::Exception,
        }
    }
}

/// Value kinds returned by [`HostApi::value_kind`].
///
/// Over the FFI these travel as plain `u32` — convert with `as u32` /
/// [`ValueKind::from_u32`]. The host-side mapping (and the coverage test
/// guarding that every interpreter value maps to one of these) lands with
/// the feature-gated plugin module in the interpreter crate.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueKind {
    /// The `nil` value.
    Nil = 0,
    /// A boolean.
    Bool = 1,
    /// An integer that fits in an `i64` (`int_get` succeeds).
    Int = 2,
    /// An integer that does not fit in an `i64` (`int_get` fails; use
    /// `value_display`/`value_str`).
    BigInt = 3,
    /// A float.
    Float = 4,
    /// A rational number.
    Rational = 5,
    /// A string (`string_get` succeeds).
    String = 6,
    /// A list (`list_len`/`list_get` succeed).
    List = 7,
    /// A tuple.
    Tuple = 8,
    /// A dict.
    Dict = 9,
    /// A set.
    Set = 10,
    /// A range.
    Range = 11,
    /// The `StopIteration` sentinel — what `__next__` returns when an
    /// iterator is exhausted.
    StopIteration = 12,
    /// A plain class instance (fields via `attr_get`/`attr_set`, methods
    /// via `invoke_method`).
    Instance = 13,
    /// A class (instantiate via `call_value`).
    Class = 14,
    /// A callable function value (closure, native function, or bound
    /// method) — use `call_value`.
    Function = 15,
    /// A module.
    Module = 16,
    /// An exception instance.
    Exception = 17,
    /// A generator.
    Generator = 18,
    /// A list/tuple/range/template iterator (drive via `invoke_method`
    /// with `__next__`).
    Iterator = 19,
    /// VM-internal values a plugin should never meaningfully receive.
    Other = 20,
}

impl ValueKind {
    /// Decode a raw kind code from [`HostApi::value_kind`]; unknown codes
    /// map to [`ValueKind::Other`].
    #[must_use]
    pub const fn from_u32(code: u32) -> Self {
        match code {
            0 => Self::Nil,
            1 => Self::Bool,
            2 => Self::Int,
            3 => Self::BigInt,
            4 => Self::Float,
            5 => Self::Rational,
            6 => Self::String,
            7 => Self::List,
            8 => Self::Tuple,
            9 => Self::Dict,
            10 => Self::Set,
            11 => Self::Range,
            12 => Self::StopIteration,
            13 => Self::Instance,
            14 => Self::Class,
            15 => Self::Function,
            16 => Self::Module,
            17 => Self::Exception,
            18 => Self::Generator,
            19 => Self::Iterator,
            _ => Self::Other,
        }
    }
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
/// assert_eq!(base.kind, generic_lang_api::ExceptionCode::Exception as u32);
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PluginError {
    /// An [`ExceptionCode`] discriminant, kept as raw `u32` so reserved
    /// non-exception statuses can travel through unchanged.
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
                    kind: ExceptionCode::$code as u32,
                    message: message.into(),
                }
            }
        )*
    };
}

impl PluginError {
    plugin_error_constructors!(
        /// The base `Exception` class.
        exception => Exception,
        type_error => TypeError,
        value_error => ValueError,
        name_error => NameError,
        const_reassignment_error => ConstReassignmentError,
        attribute_error => AttributeError,
        import_error => ImportError,
        assertion_error => AssertionError,
        io_error => IoError,
        key_error => KeyError,
        index_error => IndexError,
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
        assert_eq!(err.kind, ExceptionCode::Exception as u32);
        assert_eq!(
            PluginError::index_error("i").kind,
            ExceptionCode::IndexError as u32
        );
    }

    #[test]
    fn code_round_trips() {
        for code in [
            ExceptionCode::Exception,
            ExceptionCode::TypeError,
            ExceptionCode::ValueError,
            ExceptionCode::NameError,
            ExceptionCode::ConstReassignmentError,
            ExceptionCode::AttributeError,
            ExceptionCode::ImportError,
            ExceptionCode::AssertionError,
            ExceptionCode::IoError,
            ExceptionCode::KeyError,
            ExceptionCode::IndexError,
        ] {
            assert_eq!(ExceptionCode::from_u32(code as u32), code);
        }
        assert_eq!(ExceptionCode::from_u32(999), ExceptionCode::Exception);

        for kind in [
            ValueKind::Nil,
            ValueKind::Bool,
            ValueKind::Int,
            ValueKind::BigInt,
            ValueKind::Float,
            ValueKind::Rational,
            ValueKind::String,
            ValueKind::List,
            ValueKind::Tuple,
            ValueKind::Dict,
            ValueKind::Set,
            ValueKind::Range,
            ValueKind::StopIteration,
            ValueKind::Instance,
            ValueKind::Class,
            ValueKind::Function,
            ValueKind::Module,
            ValueKind::Exception,
            ValueKind::Generator,
            ValueKind::Iterator,
            ValueKind::Other,
        ] {
            assert_eq!(ValueKind::from_u32(kind as u32), kind);
        }
        assert_eq!(ValueKind::from_u32(999), ValueKind::Other);
    }
}
