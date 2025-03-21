//! Runtime representation of generic runtime values.

use crate::{
    chunk::Chunk,
    heap::{
        BigIntId, BoundMethodId, ClassId, ClosureId, FunctionId, Heap, InstanceId, ModuleId,
        NativeFunctionId, NativeMethodId, StringId, UpvalueId,
    },
    vm::{Global, VM},
};

use derivative::Derivative;
use derive_more::From;
use hashbrown::HashTable;
use hashbrown::hash_table::Entry;
use num_bigint::BigInt;
use num_traits::Pow;
use num_traits::identities::Zero;
use rustc_hash::{FxHashMap as HashMap, FxHasher};
use std::hash::{Hash, Hasher};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Rem, Sub};
use std::path::PathBuf;

/// Central enum for the types of runtime values that exist in generic.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    StopIteration,
    Number(Number),

    String(StringId),

    Function(FunctionId),
    Module(ModuleId),

    Closure(ClosureId),
    NativeFunction(NativeFunctionId),
    NativeMethod(NativeMethodId),

    Upvalue(UpvalueId),

    Class(ClassId),
    Instance(InstanceId),
    BoundMethod(BoundMethodId),
}

// This is fake btw. But it is only used for hash,
// which throws unreachable for all the variants where it doesnt actually hold.
impl Value {
    pub(crate) fn to_hash(&self, heap: &Heap) -> u64 {
        let mut state = FxHasher::default();
        match self {
            Self::Bool(b) => b.hash(&mut state),
            Self::Nil => state.write_u8(0),
            Self::StopIteration => state.write_u8(1),
            Self::Number(n) => match n {
                Number::Float(f) => {
                    let f = if *f == 0.0 { 0.0 } else { *f };
                    // If f has no fractional part, we treat it like an integer.
                    if f.fract() == 0.0 {
                        // Convert to an integer if the float has no fractional part
                        #[allow(clippy::cast_possible_truncation)]
                        (BigInt::from(f as i64)).hash(&mut state);
                    } else {
                        f.to_bits().hash(&mut state); // Otherwise, hash the float as is
                    }
                }
                Number::Integer(i) => match i {
                    GenericInt::Small(n) => BigInt::from(*n).hash(&mut state),
                    &GenericInt::Big(n) => (n.to_value(heap)).hash(&mut state),
                },
            },
            Self::String(s) => s.hash(&mut state),
            _ => {
                unreachable!("Only hashable types are Bool, Nil, Integer, and String.")
            }
        }
        state.finish()
    }

    pub(crate) fn eq(&self, other: &Self, heap: &Heap) -> bool {
        match (self, other) {
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Nil, Self::Nil) | (Self::StopIteration, Self::StopIteration) => true,
            (Self::Number(a), Self::Number(b)) => a.eq(b, heap),
            (Self::String(a), Self::String(b)) => a == b || a.to_value(heap) == b.to_value(heap),
            (Self::Function(a), Self::Function(b)) => {
                a == b || a.to_value(heap) == b.to_value(heap)
            }
            (Self::Module(a), Self::Module(b)) => a == b || a.to_value(heap) == b.to_value(heap),
            (Self::Closure(a), Self::Closure(b)) => a == b || a.to_value(heap) == b.to_value(heap),
            (Self::NativeFunction(a), Self::NativeFunction(b)) => {
                a == b || a.to_value(heap) == b.to_value(heap)
            }
            (Self::NativeMethod(a), Self::NativeMethod(b)) => {
                a == b || a.to_value(heap) == b.to_value(heap)
            }
            (Self::Upvalue(a), Self::Upvalue(b)) => {
                a == b || a.to_value(heap).eq(b.to_value(heap), heap)
            }
            (Self::Class(a), Self::Class(b)) => a == b || a.to_value(heap) == b.to_value(heap),
            (Self::Instance(a), Self::Instance(b)) => {
                a == b || a.to_value(heap) == b.to_value(heap)
            }
            (Self::BoundMethod(a), Self::BoundMethod(b)) => {
                a == b || a.to_value(heap) == b.to_value(heap)
            }
            _ => false, // Return false if the variants are different
        }
    }
}

impl Value {
    pub fn to_string(&self, heap: &Heap) -> String {
        match self {
            Self::Bool(bool) => format!("{bool}"),
            Self::Number(num) => num.to_string(heap),
            Self::Nil => "nil".to_string(),
            Self::StopIteration => "StopIteration".to_string(),
            Self::String(s) => (*s.to_value(heap)).to_string(),
            // Can i do all of these just like string?
            Self::Function(ref_id) => ref_id.to_value(heap).to_string(heap),
            Self::Closure(ref_id) => ref_id.to_value(heap).to_string(heap),
            Self::NativeFunction(ref_id) => ref_id.to_value(heap).to_string(heap),
            Self::NativeMethod(ref_id) => ref_id.to_value(heap).to_string(heap),
            Self::Class(ref_id) => ref_id.to_value(heap).to_string(heap),
            Self::Instance(ref_id) => ref_id.to_value(heap).to_string(heap),
            Self::BoundMethod(ref_id) => ref_id.to_value(heap).to_string(heap),
            Self::Upvalue(ref_id) => format!("{}", ref_id.to_value(heap)),
            Self::Module(ref_id) => ref_id.to_value(heap).to_string(heap),
        }
    }
}

// These could probably be individual entries in the enum tbh.
/// Enum summarizing all of the generic number types.
#[derive(Debug, Clone, From, Copy, PartialEq)]
pub enum Number {
    Float(f64),
    Integer(GenericInt),
}

impl From<Number> for Result<Number, String> {
    fn from(n: Number) -> Self {
        Ok(n)
    }
}

// These could probably be individual entries in the enum tbh.
/// Enum summarizing all of the generic number types.
#[derive(Debug, Clone, From, Copy, PartialEq, Eq)]
pub enum GenericInt {
    Small(i64),
    Big(BigIntId),
}

impl GenericInt {
    /// Upvalues are implementation details and should never be seen by the user.
    /// So this is only used for debugging.
    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        match self {
            Self::Small(n) => format!("{n}"),
            Self::Big(n) => format!("{}", n.to_value(heap)),
        }
    }
}

impl GenericInt {
    pub const fn new(value: i64) -> Self {
        Self::Small(value)
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
}

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

// Comparisons for GenericInt against other GenericInt
impl GenericInt {
    fn eq(&self, other: &Self, heap: &Heap) -> bool {
        match (self, other) {
            (Self::Small(a), Self::Small(b)) => a == b,
            (Self::Big(a), Self::Big(b)) => a == b || a.to_value(heap) == b.to_value(heap),
            (Self::Small(a), Self::Big(b)) => &BigInt::from(*a) == b.to_value(heap),
            (Self::Big(a), Self::Small(b)) => a.to_value(heap) == &BigInt::from(*b),
        }
    }
}

#[allow(dead_code)]
impl GenericInt {
    fn partial_cmp(&self, other: &Self, heap: &Heap) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Small(a), Self::Small(b)) => a.partial_cmp(b),
            (Self::Big(a), Self::Big(b)) => a.partial_cmp(b),
            (Self::Small(a), Self::Big(b)) => BigInt::from(*a).partial_cmp(b.to_value(heap)),
            (Self::Big(a), Self::Small(b)) => (a.to_value(heap)).partial_cmp(&BigInt::from(*b)),
        }
    }

    fn lt(&self, other: &Self, heap: &Heap) -> bool {
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

    fn le(&self, other: &Self, heap: &Heap) -> bool {
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
}

impl GenericInt {
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
}

impl GenericInt {
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
}

impl GenericInt {
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
}

impl GenericInt {
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

impl Number {
    fn to_string(&self, heap: &Heap) -> String {
        match self {
            Self::Float(num) => format!("{num:?}"),
            Self::Integer(num) => num.to_string(heap),
        }
    }
}

impl Number {
    pub(crate) fn to_f64(&self, heap: &Heap) -> f64 {
        match self {
            Self::Float(f) => *f,
            Self::Integer(i) => i.to_f64(heap),
        }
    }
}

impl From<i64> for Number {
    fn from(n: i64) -> Self {
        Self::Integer(n.into())
    }
}

impl Number {
    pub(super) fn eq(&self, other: &Self, heap: &Heap) -> bool {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a.eq(b, heap),
            (Self::Float(a), Self::Float(b)) => a == b,
            (Self::Integer(a), Self::Float(b)) => a.to_f64(heap) == *b,
            (Self::Float(a), Self::Integer(b)) => *a == b.to_f64(heap),
        }
    }

    /// Expressions `a**b` on numbers produce integers, only if
    /// both are already integers and `b`is not negative.
    /// Everything else produces a float result.
    #[allow(clippy::option_if_let_else)]
    pub(super) fn pow(self, exp: Self, heap: &mut Heap) -> Self {
        match (self, exp) {
            (Self::Integer(a), Self::Integer(b)) if b.ge_i64(0, heap) => Self::Integer(
                a.pow(b, heap)
                    .expect("Only calling integer pow for exp >= 0"),
            ),
            (Self::Float(a), Self::Integer(b)) => match b.try_to_i32(heap) {
                Ok(b) => Self::Float(a.powi(b)),
                Err(_) => Self::Float(f64::INFINITY), // Or f64::MAX?
            },
            (Self::Integer(a), Self::Float(b)) => Self::Float(a.to_f64(heap).powf(b)),
            (Self::Float(a), Self::Float(b)) => Self::Float(a.powf(b)),
            (Self::Integer(a), Self::Integer(b)) => match b.try_to_i32(heap) {
                Ok(b) => Self::Float(a.to_f64(heap).powi(b)),
                Err(_) => Self::Float(f64::INFINITY), // Or f64::MAX?
            },
        }
    }

    /// Floor division only produces integers if both operands are themselves integers.
    /// Otherwise the result is a floored float.
    pub(super) fn floor_div(self, rhs: Self, heap: &mut Heap) -> Result<Self, String> {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Integer((a.div(b, heap))?)),
            (Self::Float(a), Self::Integer(b)) => Ok(Self::Float((a / b.to_f64(heap)).floor())),
            (Self::Integer(a), Self::Float(b)) => Ok(Self::Float((a.to_f64(heap) / b).floor())),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float((a / b).floor())),
        }
    }

    pub(super) fn neg(self, heap: &mut Heap) -> Self {
        match self {
            Self::Integer(n) => Self::Integer(n.neg(heap)),
            Self::Float(f) => Self::Float(-f),
        }
    }
}

impl Number {
    /// Standard division ALWAYS produces floats, even for two integer arguments and even
    /// if the result could be represented as an integer.
    pub(crate) fn div(self, rhs: Self, heap: &Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Float(a.to_f64(heap) / b.to_f64(heap)),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a / b.to_f64(heap)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(a.to_f64(heap) / b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a / b),
        }
    }
    pub(crate) fn add(self, rhs: Self, heap: &mut Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a.add(b, heap)),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a + b.to_f64(heap)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(a.to_f64(heap) + b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a + b),
        }
    }

    pub(crate) fn sub(self, rhs: Self, heap: &mut Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a.sub(b, heap)),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a - b.to_f64(heap)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(a.to_f64(heap) - b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a - b),
        }
    }

    pub(crate) fn mul(self, rhs: Self, heap: &mut Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a.mul(b, heap)),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a * b.to_f64(heap)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(a.to_f64(heap) * b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a * b),
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
        }
    }

    fn partial_cmp(&self, other: &Self, heap: &Heap) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a.partial_cmp(b, heap),
            (Self::Float(a), Self::Float(b)) => a.partial_cmp(b),
            (Self::Integer(a), Self::Float(b)) => a.to_f64(heap).partial_cmp(b),
            (Self::Float(a), Self::Integer(b)) => a.partial_cmp(&b.to_f64(heap)),
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

/// Uncaptured (open) upvalues point to the stack index of the value,
/// while captured upvalues point to the value in the heap.
#[derive(Debug, Clone, PartialEq)]
pub enum Upvalue {
    Open(usize),
    Closed(Value),
}

impl std::fmt::Display for Upvalue {
    /// Upvalues are implementation details and should never be seen by the user.
    /// So this is only used for debugging.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("upvalue")
    }
}

impl Upvalue {
    pub(super) fn as_open(&self) -> usize {
        match self {
            Self::Open(n) => *n,
            Self::Closed(_) => unreachable!("Only call as_open on a known open upvalue!"),
        }
    }

    pub(super) fn eq(&self, other: &Self, heap: &Heap) -> bool {
        match (self, other) {
            (Self::Open(a), Self::Open(b)) => a == b,
            (Self::Closed(a), Self::Closed(b)) => a.eq(b, heap),
            _ => false,
        }
    }
}

/// Closure are wrappers around runable code.
///
/// They contain a reference to the actual function they wrap,
/// the captured upvalues and their count.
///
/// They also additionally store whether the wrapped code
/// is a general function or a module.
///
/// For the correct resolution of global variables referenced from function
/// in imported modules, it is also necessary that the module that contains
/// that function is also available for that resolution.
#[derive(Debug, PartialOrd, Clone)]
pub struct Closure {
    pub(super) function: FunctionId,
    pub(super) upvalues: Vec<UpvalueId>,
    pub(super) upvalue_count: usize,
    pub(super) is_module: bool,
    pub(super) containing_module: Option<ModuleId>,
}

impl Closure {
    fn to_string(&self, heap: &Heap) -> String {
        self.function.to_value(heap).to_string(heap)
    }
}

impl std::fmt::Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<Closure Value>")
    }
}

impl PartialEq for Closure {
    fn eq(&self, _other: &Self) -> bool {
        // Two different closures are always considered different, even if they close over exactly the same things
        false
    }
}

impl Closure {
    pub(super) fn new(
        function: FunctionId,
        is_module: bool,
        containing_module: Option<ModuleId>,
        heap: &Heap,
    ) -> Self {
        let upvalue_count = function.to_value(heap).upvalue_count;
        Self {
            function,
            upvalues: Vec::with_capacity(upvalue_count),
            upvalue_count,
            is_module,
            containing_module,
        }
    }
}

impl Value {
    pub(super) fn bound_method(receiver: Self, method: Self, heap: &mut Heap) -> Self {
        heap.add_bound_method(BoundMethod { receiver, method })
    }
}

#[derive(Debug, Clone)]
pub struct BoundMethod {
    // Probably could be an InstanceId now
    pub(super) receiver: Value,
    // Has to be a general Value because it can be a NativeMethod or Closure
    pub(super) method: Value,
}

impl std::fmt::Display for BoundMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<bound method Value>")
    }
}

impl BoundMethod {
    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "<bound method {}.{} of {}>",
            *self.receiver_class_name(heap).to_value(heap),
            *self.method_name(heap).to_value(heap),
            self.receiver.to_string(heap)
        )
    }

    fn method_name(&self, heap: &Heap) -> StringId {
        match self.method {
            Value::NativeMethod(native) => native.to_value(heap).name,
            Value::Closure(closure) => closure.to_value(heap).function.to_value(heap).name,
            x => unreachable!(
                "Bound method only binds over closures or native methods, got `{}` instead.",
                x.to_string(heap)
            ),
        }
    }

    fn receiver_class_name(&self, heap: &Heap) -> StringId {
        match self.receiver {
            Value::Instance(instance) => instance.to_value(heap).class.to_value(heap).name,
            x => unreachable!(
                "Bound methods can only have instances as receivers, got `{}` instead.",
                x.to_string(heap)
            ),
        }
    }
}

/// Object for actual function implementations
///
/// Contains the name, number of expected arguments and number of
/// captured upvalues.
///
/// Additionally hold the chunk of compiled bytecode.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub(super) arity: usize,
    pub(super) chunk: Chunk,
    pub(super) name: StringId,
    pub(super) upvalue_count: usize,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<fn Value>")
    }
}

impl Function {
    #[must_use]
    pub(super) fn new(arity: usize, name: StringId) -> Self {
        Self {
            arity,
            name,
            chunk: Chunk::new(name),
            upvalue_count: 0,
        }
    }

    fn to_string(&self, heap: &Heap) -> String {
        format!("<fn {}>", *self.name.to_value(heap))
    }
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, PartialOrd, Clone)]
pub struct NativeFunction {
    pub(super) name: StringId,
    pub(super) arity: &'static [u8],

    #[derivative(
            Debug = "ignore",
            // Treat the implementation as always equal; we discriminate built-in functions by name
            PartialEq(compare_with = "always_equals"),
            PartialOrd = "ignore"
        )]
    pub(super) fun: NativeFunctionImpl,
}

impl std::fmt::Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<native fn Value>")
    }
}

impl NativeFunction {
    fn to_string(&self, heap: &Heap) -> String {
        format!("<native fn {}>", *self.name.to_value(heap))
    }
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, PartialOrd, Clone)]
pub struct NativeMethod {
    pub(super) class: StringId,
    pub(super) name: StringId,
    pub(super) arity: &'static [u8],

    #[derivative(
            Debug = "ignore",
            // Treat the implementation as always equal; we discriminate built-in functions by name
            PartialEq(compare_with = "always_equals"),
            PartialOrd = "ignore"
        )]
    pub(super) fun: NativeMethodImpl,
}

impl std::fmt::Display for NativeMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<native method Value>")
    }
}

impl NativeMethod {
    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "<native method {} of class {}>",
            *self.name.to_value(heap),
            *self.class.to_value(heap)
        )
    }
}

pub type NativeFunctionImpl = fn(&mut VM, &mut [&mut Value]) -> Result<Value, String>;
pub type NativeMethodImpl = fn(&mut VM, &mut Value, &mut [&mut Value]) -> Result<Value, String>;
pub type ModuleContents = Vec<(&'static str, &'static [u8], NativeFunctionImpl)>;

const fn always_equals<T>(_: &T, _: &T) -> bool {
    true
}

#[derive(Debug, Clone, Derivative)]
#[derivative(PartialOrd, PartialEq)]
pub struct Class {
    pub(super) name: StringId,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    // Have to be general Value because it can be a nativemethod(how?) or a closure
    // Should probably also be String and not StringId?
    pub(super) methods: HashMap<StringId, Value>,
    pub(super) is_native: bool,
}

impl Eq for Class {}

impl Class {
    #[must_use]
    pub(super) fn new(name: StringId, is_native: bool) -> Self {
        Self {
            name,
            methods: HashMap::default(),
            is_native,
        }
    }

    fn to_string(&self, heap: &Heap) -> String {
        format!("<class {}>", *self.name.to_value(heap))
    }
}

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<class Value>")
    }
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, PartialOrd, Clone)]
pub struct Instance {
    pub(super) class: ClassId,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    pub(super) fields: HashMap<String, Value>,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    pub(super) backing: Option<NativeClass>,
}

impl Instance {
    #[must_use]
    pub(super) fn new(class: Value, backing: Option<NativeClass>) -> Self {
        let id = *class.as_class();
        Self {
            class: id,
            fields: HashMap::default(),
            backing,
        }
    }

    #[allow(clippy::option_if_let_else)]
    fn to_string(&self, heap: &Heap) -> String {
        match &self.backing {
            Some(native_class) => native_class.to_string(heap),
            None => format!(
                "<{} instance>",
                self.class.to_value(heap).name.to_value(heap)
            ),
        }
    }
}

impl std::fmt::Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.backing {
            Some(_) => f.pad("<native instance Value>"),
            None => f.pad("<instance Value>"),
        }
    }
}

impl PartialEq for BoundMethod {
    fn eq(&self, _other: &Self) -> bool {
        // Two different bound methods are always considered different
        false
    }
}

#[derive(Debug, Clone, Derivative)]
#[derivative(PartialOrd, PartialEq)]
pub struct Module {
    pub(super) name: StringId,
    pub(super) path: PathBuf,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    pub(super) globals: HashMap<StringId, Global>,
    pub(super) names_to_import: Option<Vec<StringId>>,
    pub(super) alias: StringId,
    pub(super) local_import: bool,
}

impl Eq for Module {}

impl Module {
    pub(super) fn new(
        name: StringId,
        path: PathBuf,
        names_to_import: Option<Vec<StringId>>,
        alias: StringId,
        local_import: bool,
    ) -> Self {
        Self {
            name,
            path,
            globals: HashMap::default(),
            names_to_import,
            alias,
            local_import,
        }
    }

    fn to_string(&self, heap: &Heap) -> String {
        format!("<module {}>", *self.name.to_value(heap))
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<module Value>")
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Self::Number(f.into())
    }
}

impl From<i64> for Value {
    fn from(f: i64) -> Self {
        Self::Number(Number::Integer(f.into()))
    }
}

impl From<GenericInt> for Value {
    fn from(i: GenericInt) -> Self {
        Self::Number(Number::Integer(i))
    }
}

impl From<Number> for Value {
    fn from(n: Number) -> Self {
        Self::Number(n)
    }
}

impl From<StringId> for Value {
    fn from(s: StringId) -> Self {
        Self::String(s)
    }
}

impl From<BigIntId> for Value {
    fn from(b: BigIntId) -> Self {
        Self::Number(Number::Integer(GenericInt::Big(b)))
    }
}

impl From<FunctionId> for Value {
    fn from(f: FunctionId) -> Self {
        Self::Function(f)
    }
}

impl From<ClosureId> for Value {
    fn from(c: ClosureId) -> Self {
        Self::Closure(c)
    }
}

impl From<NativeFunctionId> for Value {
    fn from(n: NativeFunctionId) -> Self {
        Self::NativeFunction(n)
    }
}

impl From<NativeMethodId> for Value {
    fn from(n: NativeMethodId) -> Self {
        Self::NativeMethod(n)
    }
}

impl From<UpvalueId> for Value {
    fn from(u: UpvalueId) -> Self {
        Self::Upvalue(u)
    }
}

impl From<ClassId> for Value {
    fn from(c: ClassId) -> Self {
        Self::Class(c)
    }
}

impl From<InstanceId> for Value {
    fn from(i: InstanceId) -> Self {
        Self::Instance(i)
    }
}

impl From<BoundMethodId> for Value {
    fn from(b: BoundMethodId) -> Self {
        Self::BoundMethod(b)
    }
}

impl From<ModuleId> for Value {
    fn from(m: ModuleId) -> Self {
        Self::Module(m)
    }
}

impl Value {
    pub(super) const fn is_falsey(&self) -> bool {
        matches!(self, Self::Bool(false) | Self::Nil)
    }

    pub(super) const fn is_hasheable(&self) -> bool {
        matches!(
            self,
            Self::Bool(_) | Self::Nil | Self::Number(_) | Self::String(_) | Self::StopIteration
        )
    }

    pub(super) fn as_generic_int(&self) -> &GenericInt {
        match self {
            Self::Number(Number::Integer(n)) => n,
            _ => unreachable!("Expected Number, found `{:?}`", self),
        }
    }

    pub(super) fn as_closure(&self) -> &ClosureId {
        match self {
            Self::Closure(c) => c,
            _ => unreachable!("Expected Closure, found `{:?}`", self),
        }
    }

    pub(super) fn as_string(&self) -> &StringId {
        match self {
            Self::String(s) => s,
            _ => unreachable!("Expected String, found `{:?}`", self),
        }
    }

    pub(super) fn as_native_method(&self) -> &NativeMethodId {
        match self {
            Self::NativeMethod(n) => n,
            _ => unreachable!("Expected Native, found `{:?}`", self),
        }
    }

    pub(super) fn as_function(&self) -> &FunctionId {
        match self {
            Self::Function(f) => f,
            _ => unreachable!("Expected Function, found `{:?}`", self),
        }
    }

    pub(super) fn as_class(&self) -> &ClassId {
        match self {
            Self::Class(c) => c,
            _ => unreachable!("Expected Class, found `{:?}`", self),
        }
    }

    pub(super) fn as_instance(&mut self) -> &InstanceId {
        match self {
            Self::Instance(i) => i,
            _ => unreachable!("Expected Instance, found `{:?}`", self),
        }
    }

    pub(super) fn as_class_mut(&mut self) -> &mut ClassId {
        match self {
            Self::Class(c) => c,
            _ => unreachable!("Expected Class, found `{:?}`", self),
        }
    }

    pub(super) fn upvalue_location(&self) -> &UpvalueId {
        match self {
            Self::Upvalue(v) => v,
            _ => unreachable!("Expected upvalue, found `{:?}`", self),
        }
    }

    pub(super) fn class_name(&self, heap: &Heap) -> StringId {
        match &self {
            Self::Instance(instance) => instance.to_value(heap).class.to_value(heap).name,
            x => unreachable!("Only instances have classes. Got `{:?}`", x),
        }
    }

    pub(super) fn as_module(&self) -> &ModuleId {
        match self {
            Self::Module(m) => m,
            _ => unreachable!("Expected Module, found `{:?}`", self),
        }
    }

    pub(super) fn as_list<'a>(&self, heap: &'a Heap) -> &'a List {
        match self {
            Self::Instance(inst) => match &inst.to_value(heap).backing {
                Some(NativeClass::List(list)) => list,
                _ => unreachable!("Expected List, found `{:?}`", self),
            },
            _ => unreachable!("Expected List, found `{:?}`", self),
        }
    }

    pub(super) fn as_list_mut<'a>(&mut self, heap: &'a mut Heap) -> &'a mut List {
        match self {
            Self::Instance(inst) => match &mut inst.to_value_mut(heap).backing {
                Some(NativeClass::List(list)) => list,
                _ => unreachable!("Expected List, found something else."),
            },
            _ => unreachable!("Expected List, found `{:?}`", self),
        }
    }

    pub(super) fn as_list_iter_mut<'a>(&mut self, heap: &'a mut Heap) -> &'a mut ListIterator {
        match self {
            Self::Instance(inst) => match &mut inst.to_value_mut(heap).backing {
                Some(NativeClass::ListIterator(list_iter)) => list_iter,
                _ => unreachable!("Expected ListIterator, found something else."),
            },
            _ => unreachable!("Expected ListIterator, found `{:?}`", self),
        }
    }

    pub(super) fn as_set<'a>(&self, heap: &'a Heap) -> &'a Set {
        match self {
            Self::Instance(inst) => match &inst.to_value(heap).backing {
                Some(NativeClass::Set(set)) => set,
                _ => unreachable!("Expected Set, found `{:?}`", self),
            },
            _ => unreachable!("Expected Set, found `{:?}`", self),
        }
    }

    pub(super) fn as_set_mut<'a>(&mut self, heap: &'a mut Heap) -> &'a mut Set {
        match self {
            Self::Instance(inst) => match &mut inst.to_value_mut(heap).backing {
                Some(NativeClass::Set(set)) => set,
                _ => unreachable!("Expected Set, found something else."),
            },
            _ => unreachable!("Expected Set, found `{:?}`", self),
        }
    }

    pub(super) fn as_dict<'a>(&self, heap: &'a Heap) -> &'a Dict {
        match self {
            Self::Instance(inst) => match &inst.to_value(heap).backing {
                Some(NativeClass::Dict(dict)) => dict,
                _ => unreachable!("Expected Dict, found `{:?}`", self),
            },
            _ => unreachable!("Expected Dict, found `{:?}`", self),
        }
    }

    pub(super) fn as_dict_mut<'a>(&mut self, heap: &'a mut Heap) -> &'a mut Dict {
        match self {
            Self::Instance(inst) => match &mut inst.to_value_mut(heap).backing {
                Some(NativeClass::Dict(dict)) => dict,
                _ => unreachable!("Expected Dict, found something else."),
            },
            _ => unreachable!("Expected Dict, found `{:?}`", self),
        }
    }
}

#[derive(Debug, Clone)]
pub enum NativeClass {
    List(List),
    ListIterator(ListIterator),
    Set(Set),
    Dict(Dict),
}

impl NativeClass {
    pub(super) fn new(kind: &str) -> Self {
        match kind {
            "List" => Self::List(List::new()),
            "ListIterator" => Self::ListIterator(ListIterator::new(None)),
            "Set" => Self::Set(Set::new()),
            "Dict" => Self::Dict(Dict::new()),
            _ => unreachable!("Unknown native class `{}`.", kind),
        }
    }

    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        match self {
            Self::List(list) => list.to_string(heap),
            Self::ListIterator(list_iter) => list_iter.to_string(heap),
            Self::Set(set) => set.to_string(heap),
            Self::Dict(dict) => dict.to_string(heap),
        }
    }
}

#[derive(Debug, Clone)]
pub struct List {
    pub(super) items: Vec<Value>,
}

impl List {
    #[must_use]
    pub(super) const fn new() -> Self {
        Self { items: Vec::new() }
    }

    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "[{}]",
            self.items
                .iter()
                .map(|item| item.to_string(heap))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<List Value>")
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Default)]
pub struct ListIterator {
    pub(super) list: Option<InstanceId>,
    pub(super) index: usize,
}

impl ListIterator {
    pub(super) const fn new(list: Option<InstanceId>) -> Self {
        Self { list, index: 0 }
    }

    pub(super) fn get_list<'a>(&self, heap: &'a Heap) -> Option<&'a List> {
        self.list
            .as_ref()
            .and_then(|item| match &item.to_value(heap).backing {
                Some(NativeClass::List(list)) => Some(list),
                _ => None,
            })
    }

    #[allow(clippy::option_if_let_else)]
    fn to_string(&self, heap: &Heap) -> String {
        match self.list {
            Some(list) => format!("<list iterator of {}>", list.to_value(heap).to_string(heap)),
            None => "<list iterator>".to_string(),
        }
    }
}

impl std::fmt::Display for ListIterator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.list {
            Some(_) => f.pad("<list iterator of Value>"),
            None => f.pad("<list iterator Value>"),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Set {
    pub(super) items: HashTable<Value>,
}

impl Set {
    #[must_use]
    pub(super) fn new() -> Self {
        Self {
            items: HashTable::default(),
        }
    }

    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "{{{}}}",
            self.items
                .iter()
                .map(|item| item.to_string(heap))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    pub(super) fn add(&mut self, item: Value, heap: &Heap) {
        if let Entry::Vacant(entry) = self.items.entry(
            item.to_hash(heap),
            |val| val.eq(&item, heap),
            |val| val.to_hash(heap),
        ) {
            entry.insert(item);
        }
    }

    pub(super) fn remove(&mut self, item: &Value, heap: &Heap) -> bool {
        self.items
            .find_entry(item.to_hash(heap), |val| val.eq(item, heap))
            .is_ok_and(|entry| {
                entry.remove();
                true
            })
    }

    pub(super) fn contains(&self, item: &Value, heap: &Heap) -> bool {
        self.items
            .find(item.to_hash(heap), |val| val.eq(item, heap))
            .is_some()
    }
}

impl std::fmt::Display for Set {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<Set Value>")
    }
}

#[derive(Debug, Clone, Default)]
pub struct Dict {
    pub(super) items: HashTable<(Value, Value)>,
}

impl Dict {
    #[must_use]
    pub(super) fn new() -> Self {
        Self {
            items: HashTable::default(),
        }
    }

    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "{{{}}}",
            self.items
                .iter()
                .map(|(key, value)| format!("{}: {}", key.to_string(heap), value.to_string(heap)))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    pub(super) fn add(&mut self, key: Value, value: Value, heap: &Heap) {
        match self.items.entry(
            key.to_hash(heap),
            |(k, _v)| k.eq(&key, heap),
            |(k, _v)| k.to_hash(heap),
        ) {
            Entry::Vacant(entry) => {
                entry.insert((key, value));
            }
            Entry::Occupied(mut entry) => {
                entry.get_mut().1 = value;
            }
        }
    }

    pub(super) fn get(&self, key: &Value, heap: &Heap) -> Option<&Value> {
        self.items
            .find(key.to_hash(heap), |(k, _v)| k.eq(key, heap))
            .map(|(_k, v)| v)
    }
}

impl std::fmt::Display for Dict {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<Dict Value>")
    }
}

impl From<List> for NativeClass {
    fn from(list: List) -> Self {
        Self::List(list)
    }
}

impl From<ListIterator> for NativeClass {
    fn from(list_iterator: ListIterator) -> Self {
        Self::ListIterator(list_iterator)
    }
}

impl From<Set> for NativeClass {
    fn from(set: Set) -> Self {
        Self::Set(set)
    }
}

impl From<Dict> for NativeClass {
    fn from(dict: Dict) -> Self {
        Self::Dict(dict)
    }
}
