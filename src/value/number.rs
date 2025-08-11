use crate::heap::{BigIntId, Heap};

use derive_more::From;
use num_bigint::BigInt;
use num_traits::Pow;
use num_traits::Signed;
use num_traits::identities::Zero;
use std::hash::{Hash, Hasher};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Rem, Sub};
// These could probably be individual entries in the enum tbh.
/// Enum summarizing all of the generic number types.
#[derive(Debug, Clone, From, Copy, PartialEq)]
pub enum Number {
    Float(f64),
    Integer(GenericInt),
    Rational(GenericRational),
}

// Conversions
impl From<Number> for Result<Number, String> {
    fn from(n: Number) -> Self {
        Ok(n)
    }
}

impl From<i64> for Number {
    fn from(n: i64) -> Self {
        Self::Integer(n.into())
    }
}

impl Number {
    pub(super) fn to_string(&self, heap: &Heap) -> String {
        match self {
            Self::Float(num) => format!("{num:?}"),
            Self::Integer(num) => num.to_string(heap),
            Self::Rational(num) => num.to_string(heap),
        }
    }

    pub(crate) fn to_f64(&self, heap: &Heap) -> f64 {
        match self {
            Self::Float(f) => *f,
            Self::Integer(i) => i.to_f64(heap),
            Self::Rational(r) => r.to_f64(heap),
        }
    }

    pub(crate) fn from_usize(n: usize, heap: &mut Heap) -> Self {
        Self::Integer(GenericInt::from_usize(n, heap))
    }
}

// Arithmethics
impl Number {
    /// Expressions `a**b` on numbers produce integers, only if
    /// both are already integers and `b`is not negative.
    /// Everything else produces a float result.
    #[allow(clippy::option_if_let_else)]
    pub(crate) fn pow(self, exp: Self, heap: &mut Heap) -> Self {
        match (self, exp) {
            (Self::Integer(a), Self::Integer(b)) if b.ge_i64(0, heap) => Self::Integer(
                a.pow(b, heap)
                    .expect("Only calling integer pow for exp >= 0"),
            ),
            (Self::Rational(a), Self::Integer(b)) => Self::Rational(
                a.pow(b, heap)
                    .expect("Rational power should not fail for integer exponent"),
            ),
            (Self::Integer(a), Self::Rational(b)) => {
                Self::Float(a.to_f64(heap).powf(b.to_f64(heap)))
            }
            (Self::Float(a), Self::Integer(b)) => match b.try_to_i32(heap) {
                Ok(b) => Self::Float(a.powi(b)),
                Err(_) => Self::Float(f64::INFINITY),
            },
            (Self::Integer(a), Self::Float(b)) => Self::Float(a.to_f64(heap).powf(b)),
            (Self::Float(a), Self::Float(b)) => Self::Float(a.powf(b)),
            (Self::Rational(a), Self::Float(b)) => Self::Float(a.to_f64(heap).powf(b)),
            (Self::Float(a), Self::Rational(b)) => Self::Float(a.powf(b.to_f64(heap))),
            (Self::Rational(a), Self::Rational(b)) => {
                Self::Float(a.to_f64(heap).powf(b.to_f64(heap)))
            }
            (Self::Integer(a), Self::Integer(b)) => match b.try_to_i32(heap) {
                Ok(b) => Self::Float(a.to_f64(heap).powi(b)),
                Err(_) => Self::Float(f64::INFINITY),
            },
        }
    }

    /// Floor division only produces integers if both operands are themselves integers.
    /// Otherwise the result is a floored float.
    pub(crate) fn floor_div(self, rhs: Self, heap: &mut Heap) -> Result<Self, String> {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Integer((a.div(b, heap))?)),
            (Self::Rational(a), Self::Rational(b)) => {
                let result = a.div(b, heap)?;
                match result.to_int(heap) {
                    Ok(int_result) => Ok(Self::Integer(int_result)),
                    Err(_) => Ok(Self::Float(result.to_f64(heap).floor())),
                }
            }
            (Self::Float(a), Self::Integer(b)) => Ok(Self::Float((a / b.to_f64(heap)).floor())),
            (Self::Integer(a), Self::Float(b)) => Ok(Self::Float((a.to_f64(heap) / b).floor())),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float((a / b).floor())),
            (Self::Rational(a), Self::Integer(b)) => {
                let b_rat = GenericRational::new(b, GenericInt::Small(1), heap)?;
                let result = a.div(b_rat, heap)?;
                match result.to_int(heap) {
                    Ok(int_result) => Ok(Self::Integer(int_result)),
                    Err(_) => Ok(Self::Float(result.to_f64(heap).floor())),
                }
            }
            (Self::Integer(a), Self::Rational(b)) => {
                let a_rat = GenericRational::new(a, GenericInt::Small(1), heap)?;
                let result = a_rat.div(b, heap)?;
                match result.to_int(heap) {
                    Ok(int_result) => Ok(Self::Integer(int_result)),
                    Err(_) => Ok(Self::Float(result.to_f64(heap).floor())),
                }
            }
            (Self::Float(a), Self::Rational(b)) => Ok(Self::Float((a / b.to_f64(heap)).floor())),
            (Self::Rational(a), Self::Float(b)) => Ok(Self::Float((a.to_f64(heap) / b).floor())),
        }
    }

    pub(crate) fn neg(self, heap: &mut Heap) -> Self {
        match self {
            Self::Integer(n) => Self::Integer(n.neg(heap)),
            Self::Float(f) => Self::Float(-f),
            Self::Rational(r) => Self::Rational(r.neg(heap)),
        }
    }

    /// Standard division produces rationals for integer/integer and floats otherwise
    pub(crate) fn div(self, rhs: Self, heap: &mut Heap) -> Result<Self, String> {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => match GenericRational::new(a, b, heap) {
                Ok(rat) => Ok(Self::Rational(rat)),
                Err(_) => Ok(Self::Float(a.to_f64(heap) / b.to_f64(heap))),
            },
            (Self::Rational(a), Self::Rational(b)) => Ok(Self::Rational(a.div(b, heap)?)),
            (Self::Float(a), Self::Integer(b)) => Ok(Self::Float(a / b.to_f64(heap))),
            (Self::Integer(a), Self::Float(b)) => Ok(Self::Float(a.to_f64(heap) / b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a / b)),
            (Self::Rational(a), Self::Integer(b)) => {
                let b_rat = GenericRational::new(b, GenericInt::Small(1), heap)?;
                Ok(Self::Rational(a.div(b_rat, heap)?))
            }
            (Self::Integer(a), Self::Rational(b)) => {
                let a_rat = GenericRational::new(a, GenericInt::Small(1), heap)?;
                Ok(Self::Rational(a_rat.div(b, heap)?))
            }
            (Self::Float(a), Self::Rational(b)) => Ok(Self::Float(a / b.to_f64(heap))),
            (Self::Rational(a), Self::Float(b)) => Ok(Self::Float(a.to_f64(heap) / b)),
        }
    }

    pub(crate) fn add(self, rhs: Self, heap: &mut Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a.add(b, heap)),
            (Self::Rational(a), Self::Rational(b)) => Self::Rational(a.add(b, heap)),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a + b.to_f64(heap)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(a.to_f64(heap) + b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a + b),
            (Self::Rational(a), Self::Integer(b)) => {
                let b_rat = GenericRational::new(b, GenericInt::Small(1), heap)
                    .expect("Creating rational from integer should not fail");
                Self::Rational(a.add(b_rat, heap))
            }
            (Self::Integer(a), Self::Rational(b)) => {
                let a_rat = GenericRational::new(a, GenericInt::Small(1), heap)
                    .expect("Creating rational from integer should not fail");
                Self::Rational(a_rat.add(b, heap))
            }
            (Self::Float(a), Self::Rational(b)) => Self::Float(a + b.to_f64(heap)),
            (Self::Rational(a), Self::Float(b)) => Self::Float(a.to_f64(heap) + b),
        }
    }

    pub(crate) fn sub(self, rhs: Self, heap: &mut Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a.sub(b, heap)),
            (Self::Rational(a), Self::Rational(b)) => Self::Rational(a.sub(b, heap)),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a - b.to_f64(heap)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(a.to_f64(heap) - b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a - b),
            (Self::Rational(a), Self::Integer(b)) => {
                let b_rat = GenericRational::new(b, GenericInt::Small(1), heap)
                    .expect("Creating rational from integer should not fail");
                Self::Rational(a.sub(b_rat, heap))
            }
            (Self::Integer(a), Self::Rational(b)) => {
                let a_rat = GenericRational::new(a, GenericInt::Small(1), heap)
                    .expect("Creating rational from integer should not fail");
                Self::Rational(a_rat.sub(b, heap))
            }
            (Self::Float(a), Self::Rational(b)) => Self::Float(a - b.to_f64(heap)),
            (Self::Rational(a), Self::Float(b)) => Self::Float(a.to_f64(heap) - b),
        }
    }

    pub(crate) fn mul(self, rhs: Self, heap: &mut Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a.mul(b, heap)),
            (Self::Rational(a), Self::Rational(b)) => Self::Rational(a.mul(b, heap)),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a * b.to_f64(heap)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(a.to_f64(heap) * b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a * b),
            (Self::Rational(a), Self::Integer(b)) => {
                let b_rat = GenericRational::new(b, GenericInt::Small(1), heap)
                    .expect("Creating rational from integer should not fail");
                Self::Rational(a.mul(b_rat, heap))
            }
            (Self::Integer(a), Self::Rational(b)) => {
                let a_rat = GenericRational::new(a, GenericInt::Small(1), heap)
                    .expect("Creating rational from integer should not fail");
                Self::Rational(a_rat.mul(b, heap))
            }
            (Self::Float(a), Self::Rational(b)) => Self::Float(a * b.to_f64(heap)),
            (Self::Rational(a), Self::Float(b)) => Self::Float(a.to_f64(heap) * b),
        }
    }

    pub(crate) fn bitand(self, rhs: Self, heap: &mut Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a.bitand(b, heap)),
            _ => unreachable!("Did not get two integers for bitwise and."),
        }
    }

    pub(crate) fn bitor(self, rhs: Self, heap: &mut Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a.bitor(b, heap)),
            _ => unreachable!("Did not get two integers for bitwise or."),
        }
    }

    pub(crate) fn bitxor(self, rhs: Self, heap: &mut Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a.bitxor(b, heap)),
            _ => unreachable!("Did not get two integers for bitwise xor."),
        }
    }

    pub(crate) fn rem(self, rhs: Self, heap: &mut Heap) -> Result<Self, String> {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Integer((a.rem(b, heap))?)),
            (Self::Float(a), Self::Integer(b)) => Ok(Self::Float(a % b.to_f64(heap))),
            (Self::Integer(a), Self::Float(b)) => Ok(Self::Float(a.to_f64(heap) % b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a % b)),
            (Self::Rational(a), Self::Rational(b)) => {
                Ok(Self::Float(a.to_f64(heap) % b.to_f64(heap)))
            }
            (Self::Rational(a), Self::Integer(b)) => {
                Ok(Self::Float(a.to_f64(heap) % b.to_f64(heap)))
            }
            (Self::Integer(a), Self::Rational(b)) => {
                Ok(Self::Float(a.to_f64(heap) % b.to_f64(heap)))
            }
            (Self::Float(a), Self::Rational(b)) => Ok(Self::Float(a % b.to_f64(heap))),
            (Self::Rational(a), Self::Float(b)) => Ok(Self::Float(a.to_f64(heap) % b)),
        }
    }

    pub(crate) fn eq(&self, other: &Self, heap: &Heap) -> bool {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a.eq(b, heap),
            (Self::Float(a), Self::Float(b)) => a == b,
            (Self::Rational(a), Self::Rational(b)) => a.eq(b, heap),
            (Self::Integer(a), Self::Float(b)) => a.to_f64(heap) == *b,
            (Self::Float(a), Self::Integer(b)) => *a == b.to_f64(heap),
            (Self::Integer(a), Self::Rational(b)) => {
                let a_rat = GenericRational::from_int(*a);
                a_rat.eq(b, heap)
            }
            (Self::Rational(a), Self::Integer(b)) => {
                let b_rat = GenericRational::from_int(*b);
                a.eq(&b_rat, heap)
            }
            (Self::Float(a), Self::Rational(b)) => *a == b.to_f64(heap),
            (Self::Rational(a), Self::Float(b)) => a.to_f64(heap) == *b,
        }
    }

    fn partial_cmp(&self, other: &Self, heap: &Heap) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a.partial_cmp(b, heap),
            (Self::Float(a), Self::Float(b)) => a.partial_cmp(b),
            (Self::Rational(a), Self::Rational(b)) => a.partial_cmp(b, heap),
            (Self::Integer(a), Self::Float(b)) => a.to_f64(heap).partial_cmp(b),
            (Self::Float(a), Self::Integer(b)) => a.partial_cmp(&b.to_f64(heap)),
            (Self::Integer(a), Self::Rational(b)) => {
                let a_rat = GenericRational::from_int(*a);
                a_rat.partial_cmp(b, heap)
            }
            (Self::Rational(a), Self::Integer(b)) => {
                let b_rat = GenericRational::from_int(*b);
                a.partial_cmp(&b_rat, heap)
            }
            (Self::Float(a), Self::Rational(b)) => a.partial_cmp(&b.to_f64(heap)),
            (Self::Rational(a), Self::Float(b)) => a.to_f64(heap).partial_cmp(b),
        }
    }

    pub(crate) fn lt(&self, other: &Self, heap: &Heap) -> bool {
        self.partial_cmp(other, heap) == Some(std::cmp::Ordering::Less)
    }

    pub(crate) fn gt(&self, other: &Self, heap: &Heap) -> bool {
        self.partial_cmp(other, heap) == Some(std::cmp::Ordering::Greater)
    }

    pub(crate) fn ge(&self, other: &Self, heap: &Heap) -> bool {
        matches!(
            self.partial_cmp(other, heap),
            Some(std::cmp::Ordering::Greater | std::cmp::Ordering::Equal)
        )
    }

    pub(crate) fn le(&self, other: &Self, heap: &Heap) -> bool {
        matches!(
            self.partial_cmp(other, heap),
            Some(std::cmp::Ordering::Less | std::cmp::Ordering::Equal)
        )
    }
}

// These could probably be individual entries in the enum tbh.
/// Enum summarizing all of the generic number types.
#[derive(Debug, Clone, From, Copy, PartialEq, Eq)]
pub enum GenericInt {
    Small(i64),
    Big(BigIntId),
}

// General handling and conversions
impl GenericInt {
    pub const fn new(value: i64) -> Self {
        Self::Small(value)
    }

    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        match self {
            Self::Small(n) => format!("{n}"),
            Self::Big(n) => format!("{}", n.to_value(heap)),
        }
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn to_bigint(&self, heap: &Heap) -> BigInt {
        match self {
            Self::Small(n) => BigInt::from(*n),
            Self::Big(n) => n.to_value(heap).clone(),
        }
    }

    fn promote(lhs: &Self, rhs: &Self, heap: &Heap) -> (BigInt, BigInt) {
        (lhs.to_bigint(heap), rhs.to_bigint(heap))
    }

    pub fn is_zero(&self, heap: &Heap) -> bool {
        match self {
            Self::Small(n) => *n == 0,
            Self::Big(n) => n.to_value(heap).is_zero(),
        }
    }

    #[allow(clippy::option_if_let_else)]
    pub fn from_usize(n: usize, heap: &mut Heap) -> Self {
        if let Ok(n) = i64::try_from(n) {
            Self::Small(n)
        } else {
            // If usize is too large for i64, we use BigInt
            let big_int = BigInt::from(n);
            *heap.add_big_int(big_int).as_generic_int()
        }
    }
}

impl Default for GenericInt {
    fn default() -> Self {
        Self::Small(0)
    }
}

// Arithmetics
macro_rules! impl_op {
    ($method:ident, $checked_method:ident) => {
        impl GenericInt {
            pub(crate) fn $method(self, rhs: Self, heap: &mut Heap) -> GenericInt {
                match (self, rhs) {
                    (GenericInt::Small(a), GenericInt::Small(b)) => match a.$checked_method(b) {
                        Some(res) => GenericInt::Small(res).into(),
                        None => {
                            let (big_a, big_b) =
                                Self::promote(&GenericInt::Small(a), &GenericInt::Small(b), heap);
                            *heap.add_big_int(big_a.$method(big_b)).as_generic_int()
                        }
                    },
                    (lhs, rhs) => {
                        let (big_lhs, big_rhs) = Self::promote(&lhs, &rhs, heap);
                        *heap.add_big_int(big_lhs.$method(big_rhs)).as_generic_int()
                    }
                }
            }
        }
    };
}

impl_op!(add, checked_add);
impl_op!(sub, checked_sub);
impl_op!(mul, checked_mul);

// Implementing division and modulus with error handling
macro_rules! impl_div_rem {
    ($method:ident, $checked_method:ident) => {
        impl GenericInt {
            pub(crate) fn $method(self, rhs: Self, heap: &mut Heap) -> Result<GenericInt, String> {
                if rhs.is_zero(heap) {
                    return Err("Division by zero".to_string());
                }

                match (self, rhs) {
                    (GenericInt::Small(a), GenericInt::Small(b)) => match a.$checked_method(b) {
                        Some(res) => Ok(GenericInt::Small(res)),
                        None => {
                            let (big_a, big_b) =
                                Self::promote(&GenericInt::Small(a), &GenericInt::Small(b), heap);
                            Ok(*heap.add_big_int(big_a.$method(big_b)).as_generic_int())
                        }
                    },
                    (lhs, rhs) => {
                        let (big_lhs, big_rhs) = Self::promote(&lhs, &rhs, heap);
                        { Ok(*heap.add_big_int(big_lhs.$method(big_rhs)).as_generic_int()) }
                    }
                }
            }
        }
    };
}

// Implementing div and rem
impl_div_rem!(div, checked_div);
impl_div_rem!(rem, checked_rem);

// Implementing bitwise operations
macro_rules! impl_bitwise_op {
    ($trait:ident, $method:ident) => {
        impl GenericInt {
            pub(crate) fn $method(self, rhs: Self, heap: &mut Heap) -> GenericInt {
                match (self, rhs) {
                    (GenericInt::Small(a), GenericInt::Small(b)) => GenericInt::Small(a.$method(b)),
                    (lhs, rhs) => {
                        let (big_lhs, big_rhs) = Self::promote(&lhs, &rhs, heap);
                        *heap.add_big_int(big_lhs.$method(big_rhs)).as_generic_int()
                    }
                }
            }
        }
    };
}

// Implementing bitwise AND, OR, XOR
impl_bitwise_op!(BitAnd, bitand);
impl_bitwise_op!(BitOr, bitor);
impl_bitwise_op!(BitXor, bitxor);

impl GenericInt {
    #[allow(clippy::option_if_let_else)]
    fn pow(self, rhs: Self, heap: &mut Heap) -> Result<Self, String> {
        // rhs < 0
        if rhs.lt_i64(0, heap) {
            return Err("Negative exponent".to_string());
        }
        match (self, rhs) {
            (Self::Small(a), Self::Small(b)) => {
                if let Ok(b_u32) = u32::try_from(b) {
                    Ok(Self::Small(a.pow(b_u32)))
                } else {
                    // Fallback to `BigInt` exponentiation for large exponents
                    Ok(*heap
                        .add_big_int(BigInt::from(a).pow(
                            u64::try_from(b).expect("Previously checked that rhs is positive"),
                        ))
                        .as_generic_int())
                }
            }
            (Self::Big(a), Self::Big(b)) => Ok(*heap
                .add_big_int(
                    (a.to_value(heap)).clone().pow(
                        u64::try_from((b.to_value(heap)).clone())
                            .expect("Previously checked that rhs is positive"),
                    ),
                )
                .as_generic_int()),
            (Self::Small(a), Self::Big(b)) => Ok(*heap
                .add_big_int(
                    BigInt::from(a).pow(
                        u64::try_from((b.to_value(heap)).clone())
                            .expect("Previously checked that rhs is positive"),
                    ),
                )
                .as_generic_int()),
            (Self::Big(a), Self::Small(b)) => Ok(*heap
                .add_big_int(
                    (a.to_value(heap))
                        .clone()
                        .pow(u64::try_from(b).expect("Previously checked that rhs is positive")),
                )
                .as_generic_int()),
        }
    }

    fn neg(self, heap: &mut Heap) -> Self {
        match self {
            Self::Small(n) => Self::Small(n.neg()),
            Self::Big(n) => *heap
                .add_big_int((n.to_value(heap)).clone().neg())
                .as_generic_int(),
        }
    }
}

// Comparisons for GenericInt against other GenericInt
#[allow(dead_code)]
impl GenericInt {
    pub fn eq(&self, other: &Self, heap: &Heap) -> bool {
        match (self, other) {
            (Self::Small(a), Self::Small(b)) => a == b,
            (Self::Big(a), Self::Big(b)) => a == b || a.to_value(heap) == b.to_value(heap),
            (Self::Small(a), Self::Big(b)) => &BigInt::from(*a) == b.to_value(heap),
            (Self::Big(a), Self::Small(b)) => a.to_value(heap) == &BigInt::from(*b),
        }
    }

    fn partial_cmp(&self, other: &Self, heap: &Heap) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Small(a), Self::Small(b)) => a.partial_cmp(b),
            (Self::Big(a), Self::Big(b)) => a.partial_cmp(b),
            (Self::Small(a), Self::Big(b)) => BigInt::from(*a).partial_cmp(b.to_value(heap)),
            (Self::Big(a), Self::Small(b)) => (a.to_value(heap)).partial_cmp(&BigInt::from(*b)),
        }
    }

    pub(crate) fn abs(&self, heap: &mut Heap) -> Self {
        match self {
            Self::Small(n) if n > &i64::MIN => Self::Small(n.abs()),
            n => *heap.add_big_int(n.to_bigint(heap).abs()).as_generic_int(),
        }
    }

    pub(crate) fn lt(&self, other: &Self, heap: &Heap) -> bool {
        self.partial_cmp(other, heap) == Some(std::cmp::Ordering::Less)
    }

    fn gt(&self, other: &Self, heap: &Heap) -> bool {
        self.partial_cmp(other, heap) == Some(std::cmp::Ordering::Greater)
    }

    fn ge(&self, other: &Self, heap: &Heap) -> bool {
        matches!(
            self.partial_cmp(other, heap),
            Some(std::cmp::Ordering::Greater | std::cmp::Ordering::Equal)
        )
    }

    pub(crate) fn le(&self, other: &Self, heap: &Heap) -> bool {
        matches!(
            self.partial_cmp(other, heap),
            Some(std::cmp::Ordering::Less | std::cmp::Ordering::Equal)
        )
    }

    fn eq_i64(&self, other: i64, heap: &Heap) -> bool {
        match self {
            Self::Small(a) => a == &other,
            Self::Big(a) => a.to_value(heap) == &BigInt::from(other),
        }
    }

    fn partial_cmp_i64(&self, other: i64, heap: &Heap) -> Option<std::cmp::Ordering> {
        match self {
            Self::Small(a) => a.partial_cmp(&other),
            Self::Big(a) => (a.to_value(heap)).partial_cmp(&BigInt::from(other)),
        }
    }

    pub(crate) fn lt_i64(&self, other: i64, heap: &Heap) -> bool {
        self.partial_cmp_i64(other, heap) == Some(std::cmp::Ordering::Less)
    }

    pub(crate) fn ge_i64(&self, other: i64, heap: &Heap) -> bool {
        matches!(
            self.partial_cmp_i64(other, heap),
            Some(std::cmp::Ordering::Greater | std::cmp::Ordering::Equal)
        )
    }
}

// Conversions
impl GenericInt {
    #[allow(clippy::option_if_let_else)]
    #[allow(clippy::cast_precision_loss)]
    pub(crate) fn to_f64(&self, heap: &Heap) -> f64 {
        match self {
            Self::Small(n) => *n as f64,
            Self::Big(n) => match i64::try_from((n.to_value(heap)).clone()) {
                Ok(n) => n as f64,
                Err(_) => f64::INFINITY, // Or f64::MAX?
            },
        }
    }

    #[allow(clippy::option_if_let_else)]
    fn try_to_i32(&self, heap: &Heap) -> Result<i32, String> {
        match self {
            Self::Small(n) => match i32::try_from(*n) {
                Ok(n) => Ok(n),
                Err(_) => Err("Number too large to fit in i32".to_string()),
            },
            Self::Big(n) => match i32::try_from((n.to_value(heap)).clone()) {
                Ok(n) => Ok(n),
                Err(_) => Err("Number too large to fit in i32".to_string()),
            },
        }
    }

    #[allow(clippy::option_if_let_else)]
    pub(crate) fn try_to_u64(&self, heap: &Heap) -> Result<u64, String> {
        match self {
            Self::Small(n) => match u64::try_from(*n) {
                Ok(n) => Ok(n),
                Err(_) => Err("Number loses sign.".to_string()),
            },
            Self::Big(n) => match u64::try_from((n.to_value(heap)).clone()) {
                Ok(n) => Ok(n),
                Err(_) => Err("Number too large to fit in u64 or signed".to_string()),
            },
        }
    }

    #[allow(clippy::option_if_let_else)]
    #[allow(clippy::cast_precision_loss)]
    #[allow(clippy::cast_possible_truncation)]
    pub(crate) fn try_from_f64(f: f64, heap: &mut Heap) -> Result<Self, String> {
        if !f.is_finite() {
            return Err("f64 is not finite".to_string());
        }
        let truncated = f.trunc();
        if truncated >= i64::MIN as f64 && truncated <= i64::MAX as f64 {
            Ok(Self::Small(truncated as i64))
        } else {
            let s = format!("{truncated:.0}");
            match s.parse::<BigInt>() {
                Ok(big) => Ok(*heap.add_big_int(big).as_generic_int()),
                Err(_) => Err(format!("Could not convert {f} to integer")),
            }
        }
    }

    #[allow(clippy::option_if_let_else)]
    pub(crate) fn try_to_usize(&self, heap: &Heap) -> Result<usize, String> {
        match self {
            Self::Small(n) => match usize::try_from(*n) {
                Ok(n) => Ok(n),
                Err(_) => Err("Number too large to fit in usize".to_string()),
            },
            Self::Big(n) => match usize::try_from((n.to_value(heap)).clone()) {
                Ok(n) => Ok(n),
                Err(_) => Err("Number too large to fit in usize".to_string()),
            },
        }
    }
}

#[derive(Debug, Clone, From, Copy, PartialEq, Eq)]
pub struct GenericRational {
    numerator: GenericInt,
    denominator: GenericInt,
}

impl GenericRational {
    pub fn new(
        numerator: GenericInt,
        denominator: GenericInt,
        heap: &mut Heap,
    ) -> Result<Self, String> {
        if denominator.is_zero(heap) {
            return Err("Denominator cannot be zero".to_string());
        }
        let (numerator, denominator) = Self::reduce(numerator, denominator, heap);
        Ok(Self {
            numerator,
            denominator,
        })
    }

    pub fn numerator(&self) -> GenericInt {
        self.numerator
    }

    pub fn denominator(&self) -> GenericInt {
        self.denominator
    }

    pub fn from_int(numerator: GenericInt) -> Self {
        Self {
            numerator,
            denominator: GenericInt::Small(1),
        }
    }

    fn reduce(
        mut numerator: GenericInt,
        mut denominator: GenericInt,
        heap: &mut Heap,
    ) -> (GenericInt, GenericInt) {
        let gcd = Self::gcd(numerator, denominator, heap);

        numerator = numerator
            .div(gcd, heap)
            .expect("Failed to divide numerator");
        denominator = denominator
            .div(gcd, heap)
            .expect("Failed to divide denominator");

        // Ensure denominator is positive by moving sign to numerator if needed
        if denominator.lt(&GenericInt::Small(0), heap) {
            numerator = numerator.neg(heap);
            denominator = denominator.neg(heap);
        }

        (numerator, denominator)
    }

    fn gcd(mut a: GenericInt, mut b: GenericInt, heap: &mut Heap) -> GenericInt {
        while !b.is_zero(heap) {
            let temp = b;
            b = a.rem(temp, heap).unwrap();
            a = temp;
        }
        a
    }

    pub fn to_int(&self, heap: &Heap) -> Result<GenericInt, String> {
        if self.denominator.eq(&GenericInt::Small(1), heap) {
            Ok(self.numerator)
        } else {
            Err("Cannot convert rational to integer".to_string())
        }
    }

    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        format!(
            "{}:{}",
            self.numerator.to_string(heap),
            self.denominator.to_string(heap)
        )
    }

    pub fn hash<H: Hasher>(&self, state: &mut H, heap: &Heap) {
        match self.numerator {
            GenericInt::Small(n) => n.hash(state),
            GenericInt::Big(n) => n.to_value(heap).hash(state),
        }

        match self.denominator {
            GenericInt::Small(n) => n.hash(state),
            GenericInt::Big(n) => n.to_value(heap).hash(state),
        }
    }

    // Arithmetic operations
    pub fn add(self, rhs: Self, heap: &mut Heap) -> Self {
        // a/b + c/d = (a*d + b*c) / (b*d)
        let num = self
            .numerator
            .mul(rhs.denominator, heap)
            .add(self.denominator.mul(rhs.numerator, heap), heap);
        let den = self.denominator.mul(rhs.denominator, heap);
        Self::new(num, den, heap).expect("Failed to create rational")
    }

    pub fn sub(self, rhs: Self, heap: &mut Heap) -> Self {
        // a/b - c/d = (a*d - b*c) / (b*d)
        let num = self
            .numerator
            .mul(rhs.denominator, heap)
            .sub(self.denominator.mul(rhs.numerator, heap), heap);
        let den = self.denominator.mul(rhs.denominator, heap);
        Self::new(num, den, heap).expect("Failed to create rational")
    }

    pub fn mul(self, rhs: Self, heap: &mut Heap) -> Self {
        // a/b * c/d = (a*c) / (b*d)
        let num = self.numerator.mul(rhs.numerator, heap);
        let den = self.denominator.mul(rhs.denominator, heap);
        Self::new(num, den, heap).expect("Failed to create rational")
    }

    pub fn div(self, rhs: Self, heap: &mut Heap) -> Result<Self, String> {
        // a/b / c/d = (a*d) / (b*c)
        if rhs.numerator.is_zero(heap) {
            return Err("Division by zero".to_string());
        }
        let num = self.numerator.mul(rhs.denominator, heap);
        let den = self.denominator.mul(rhs.numerator, heap);
        Self::new(num, den, heap)
    }

    pub fn neg(self, heap: &mut Heap) -> Self {
        Self {
            numerator: self.numerator.neg(heap),
            denominator: self.denominator,
        }
    }

    // Comparison operations
    pub fn eq(&self, other: &Self, heap: &Heap) -> bool {
        // Since rationals are always in reduced form, we can compare directly
        self.numerator.eq(&other.numerator, heap) && self.denominator.eq(&other.denominator, heap)
    }

    pub fn partial_cmp(&self, other: &Self, heap: &Heap) -> Option<std::cmp::Ordering> {
        // a/b <=> c/d is equivalent to a*d <=> b*c
        if let (
            GenericInt::Small(a),
            GenericInt::Small(b),
            GenericInt::Small(c),
            GenericInt::Small(d),
        ) = (
            self.numerator,
            self.denominator,
            other.numerator,
            other.denominator,
        ) {
            // For small values, try direct calculation first
            if let (Some(lhs), Some(rhs)) = (a.checked_mul(d), b.checked_mul(c)) {
                lhs.partial_cmp(&rhs)
            } else {
                // Overflow, fall back to BigInt
                let lhs = self.numerator.to_bigint(heap) * other.denominator.to_bigint(heap);
                let rhs = self.denominator.to_bigint(heap) * other.numerator.to_bigint(heap);
                lhs.partial_cmp(&rhs)
            }
        } else {
            // For larger values, use BigInt arithmetic
            let lhs = self.numerator.to_bigint(heap) * other.denominator.to_bigint(heap);
            let rhs = self.denominator.to_bigint(heap) * other.numerator.to_bigint(heap);
            lhs.partial_cmp(&rhs)
        }
    }

    pub fn lt(&self, other: &Self, heap: &Heap) -> bool {
        self.partial_cmp(other, heap) == Some(std::cmp::Ordering::Less)
    }

    pub fn gt(&self, other: &Self, heap: &Heap) -> bool {
        self.partial_cmp(other, heap) == Some(std::cmp::Ordering::Greater)
    }

    pub fn ge(&self, other: &Self, heap: &Heap) -> bool {
        matches!(
            self.partial_cmp(other, heap),
            Some(std::cmp::Ordering::Greater | std::cmp::Ordering::Equal)
        )
    }

    pub fn le(&self, other: &Self, heap: &Heap) -> bool {
        matches!(
            self.partial_cmp(other, heap),
            Some(std::cmp::Ordering::Less | std::cmp::Ordering::Equal)
        )
    }

    // Conversion operations
    pub fn to_f64(&self, heap: &Heap) -> f64 {
        self.numerator.to_f64(heap) / self.denominator.to_f64(heap)
    }

    pub fn is_zero(&self, heap: &Heap) -> bool {
        self.numerator.is_zero(heap)
    }

    pub fn pow(self, exp: GenericInt, heap: &mut Heap) -> Result<Self, String> {
        if exp.lt_i64(0, heap) {
            // For negative exponents, return the reciprocal raised to the positive power
            let pos_exp = exp.neg(heap);
            let result = self.pow(pos_exp, heap)?;
            // Return reciprocal: swap numerator and denominator
            if result.numerator.is_zero(heap) {
                return Err("Division by zero in rational power".to_string());
            }
            Self::new(result.denominator, result.numerator, heap)
        } else {
            // For positive exponents, raise both numerator and denominator to the power
            let num = self.numerator.pow(exp, heap)?;
            let den = self.denominator.pow(exp, heap)?;
            Self::new(num, den, heap)
        }
    }
}
