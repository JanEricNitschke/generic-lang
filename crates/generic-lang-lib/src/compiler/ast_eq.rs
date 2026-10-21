//! Location-insensitive equality for AST nodes.
//!
//! Parsed ASTs differ in source locations after formatting. The
//! [`LocationLessEq`] trait compares the semantic structure while treating
//! every [`Location`] as equal.

use num_bigint::BigInt;

use super::ast::{
    AccessMode, BinaryOp, BoolOperator, ExprContext, FunctionKind, MethodKind, UnaryOp,
};
use crate::types::{ConditionType, Location, LoopType, Mutability};

/// Structural equality that ignores source locations.
pub(crate) trait LocationLessEq {
    /// Whether `self` and `other` have the same structure, ignoring locations.
    fn loc_eq(&self, other: &Self) -> bool;
}

impl LocationLessEq for Location {
    fn loc_eq(&self, _other: &Self) -> bool {
        true
    }
}

impl LocationLessEq for f64 {
    fn loc_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl LocationLessEq for BigInt {
    fn loc_eq(&self, other: &Self) -> bool {
        self == other
    }
}

macro_rules! partial_eq {
    ($($ty:ty),* $(,)?) => {
        $(
            impl LocationLessEq for $ty {
                fn loc_eq(&self, other: &Self) -> bool {
                    self == other
                }
            }
        )*
    };
}

partial_eq!(
    bool,
    i8,
    i16,
    i32,
    i64,
    isize,
    u8,
    u16,
    u32,
    u64,
    usize,
    String,
    Mutability,
    ConditionType,
    LoopType,
    FunctionKind,
    MethodKind,
    BinaryOp,
    UnaryOp,
    AccessMode,
    ExprContext,
    BoolOperator,
);

impl<T: LocationLessEq> LocationLessEq for Box<T> {
    fn loc_eq(&self, other: &Self) -> bool {
        (**self).loc_eq(other)
    }
}

impl<T: LocationLessEq> LocationLessEq for Option<T> {
    fn loc_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Some(a), Some(b)) => a.loc_eq(b),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<T: LocationLessEq> LocationLessEq for Vec<T> {
    fn loc_eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().zip(other).all(|(a, b)| a.loc_eq(b))
    }
}

impl<A: LocationLessEq, B: LocationLessEq> LocationLessEq for (A, B) {
    fn loc_eq(&self, other: &Self) -> bool {
        self.0.loc_eq(&other.0) && self.1.loc_eq(&other.1)
    }
}

#[cfg(test)]
mod tests {
    use super::LocationLessEq;
    use crate::types::Location;

    #[derive(Debug, generic_lang_derive::LocationLessEq)]
    struct Sample {
        value: u8,
        location: Location,
        #[loc_less(ignore)]
        ignored: String,
    }

    #[derive(Debug, generic_lang_derive::LocationLessEq)]
    enum TupleSample {
        Pair(u8, #[loc_less(ignore)] String),
    }

    #[test]
    fn ignores_locations_and_ignored_fields() {
        let a = Sample {
            value: 1,
            location: Location::default(),
            ignored: String::from("a"),
        };
        let mut b = Sample {
            value: 1,
            location: Location::default(),
            ignored: String::from("b"),
        };
        assert_eq!(a.ignored, "a");
        assert!(a.loc_eq(&b));

        b.value = 2;
        assert!(!a.loc_eq(&b));
    }

    #[test]
    fn floats_follow_language_equality() {
        assert!(0.0_f64.loc_eq(&-0.0));
        assert!(!f64::NAN.loc_eq(&f64::NAN));
    }

    #[test]
    fn ignores_ignored_tuple_variant_fields() {
        let a = TupleSample::Pair(1, String::from("a"));
        let b = TupleSample::Pair(1, String::from("b"));
        let TupleSample::Pair(_, ignored) = &a;
        assert_eq!(ignored, "a");
        assert!(a.loc_eq(&b));
        assert!(!TupleSample::Pair(2, String::from("a")).loc_eq(&a));
    }
}
