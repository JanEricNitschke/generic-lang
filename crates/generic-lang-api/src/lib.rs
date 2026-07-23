//! # Plugin ABI and authoring API for the generic programming language
//!
//! Native modules for generic are shared libraries speaking the C ABI
//! defined in [`abi`]. Rust plugin authors use the safe layer in this crate
//! root; other languages use the generated `include/generic.h`.
//!
//! This crate is versioned independently from the interpreter - it tracks ABI
//! stability for plugin authors. Build against the version matching the
//! target interpreter ([`GENERIC_PLUGIN_ABI_VERSION`] is checked at load).
//!
//! See the [plugin authoring guide][guide] for worked quickstarts (Rust and
//! C), the value model, calling back into generic, the exception model, and
//! the rooting contract.
//!
//! [guide]: https://github.com/JanEricNitschke/generic-lang/blob/main/docs/plugin-authors.md
//!
//! # Example
//!
//! ```no_run
//! use generic_lang_api::{ArgValue, GenericValue, Host, PluginError};
//!
//! fn add(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
//!     match (host.decode(args[0]), host.decode(args[1])) {
//!         (ArgValue::Int(a), ArgValue::Int(b)) => Ok(host.make_int(a + b)),
//!         _ => Err(host.type_error("add expects two ints")),
//!     }
//! }
//!
//! generic_lang_api::export_module![("add", &[2], add)];
//! ```
#![warn(missing_docs)]

pub mod abi;
mod export;
mod host;

pub use abi::{
    FfiReturn, FfiStatus, FfiStr, FunctionDesc, GENERIC_PLUGIN_ABI_VERSION, GenericValue, HostApi,
    ModuleDesc, PluginFn,
};
pub use host::{__invoke_plugin_fn, ArgValue, Host, Rooted, RustPluginFn};

/// Value kinds returned by [`HostApi::value_kind`].
///
/// Over the FFI these travel as plain `u32` - convert with `as u32` /
/// [`ValueKind::from_u32`]. The host-side mapping (and the coverage test
/// guarding that every interpreter value maps to one of these) lives in
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
    /// The `StopIteration` sentinel - what `__next__` returns when an
    /// iterator is exhausted.
    StopIteration = 12,
    /// A plain class instance (fields via `attr_get`/`attr_set`, methods
    /// via `invoke_method`).
    Instance = 13,
    /// A class (instantiate via `call_value`).
    Class = 14,
    /// A callable function value (closure, native function, or bound
    /// method) - use `call_value`.
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

/// The error side of a plugin function: what should reach the generic VM
/// when the function does not return normally.
///
/// An exact mirror of the two non-ok wire statuses
/// ([`FfiStatus::Exception`] / [`FfiStatus::Fatal`]).
///
/// Failed re-entering host calls hand back
/// [`PluginError::Exception`] (the raised exception *instance*) or
/// [`PluginError::Fatal`]; propagating them with `?` re-raises the
/// exception with full identity - class, fields, and original stack trace
/// or forwards the fatal error, respectively. To throw a fresh
/// exception, use the typed constructors on [`Host`]
/// (`host.type_error("…")`, …), which create the instance eagerly; for
/// non-builtin classes, build the instance with
/// [`Host::make_exception`] (or by calling the class) and wrap it:
///
/// ```ignore
/// fn half(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
///     let Some(n) = host.as_int(args[0]) else {
///         return Err(host.type_error("half expects an int"));
///     };
///     Ok(host.make_int(n / 2))
/// }
/// ```
#[derive(Debug, Clone, Copy)]
pub enum PluginError {
    /// An exception instance - freshly created, or caught from a
    /// re-entering host call - to be (re-)raised.
    Exception(GenericValue),
    /// A fatal host error passing through. Only ever forwarded; plugins
    /// must not fabricate it.
    Fatal,
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

    /// The crate version encodes the ABI version: `0.<abi>.<patch>`. A
    /// major bump would decouple the two schemes, and an ABI bump without
    /// the matching minor bump would publish a crate whose version lies
    /// about compatibility.
    #[test]
    fn crate_version_encodes_the_abi_version() {
        let mut parts = env!("CARGO_PKG_VERSION").split('.');
        let major: u32 = parts.next().unwrap().parse().unwrap();
        let minor: u32 = parts.next().unwrap().parse().unwrap();
        assert_eq!(major, 0, "the version scheme is 0.<abi>.<patch>");
        assert_eq!(
            minor, GENERIC_PLUGIN_ABI_VERSION,
            "bump GENERIC_PLUGIN_ABI_VERSION and the crate minor version together"
        );
    }

    #[test]
    fn kind_round_trips() {
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
