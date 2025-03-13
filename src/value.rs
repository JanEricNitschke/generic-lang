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
use num_bigint::BigInt;
use num_traits::Pow;
use num_traits::identities::Zero;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::hash::{Hash, Hasher};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Rem, Sub};
use std::path::PathBuf;

/// Central enum for the types of runtime values that exist in generic.
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
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
impl Eq for Value {}
impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Bool(b) => b.hash(state),
            Self::Nil => state.write_u8(0),
            Self::StopIteration => state.write_u8(1),
            Self::Number(n) => match n {
                Number::Float(f) => {
                    let f = if *f == 0.0 { 0.0 } else { *f };
                    f.to_bits().hash(state);
                }
                Number::Integer(i) => i.hash(state),
            },
            Self::String(s) => s.hash(state),
            _ => {
                unreachable!("Only hashable types are Bool, Nil, Integer, and String, got {self}.")
            }
        }
    }
}

impl std::fmt::Display for Value {
    /// Displaying of `Value` mostly delegates to the underlying types being pointed to.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(bool) => f.pad(&format!("{bool}")),
            Self::Number(num) => f.pad(&format!("{num}")),
            Self::Nil => f.pad("nil"),
            Self::StopIteration => f.pad("StopIteration"),
            Self::String(s) => f.pad(s),
            // Can i do all of these just like string?
            Self::Function(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::Closure(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::NativeFunction(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::NativeMethod(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::Class(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::Instance(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::BoundMethod(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::Upvalue(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::Module(ref_id) => f.pad(&format!("{}", **ref_id)),
        }
    }
}

// These could probably be individual entries in the enum tbh.
/// Enum summarizing all of the generic number types.
#[derive(Debug, PartialEq, PartialOrd, Clone, From, Copy)]
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
#[derive(Debug, Clone, From, Copy)]
pub enum GenericInt {
    Small(i64),
    Big(BigIntId),
}

impl std::fmt::Display for GenericInt {
    /// Upvalues are implementation details and should never be seen by the user.
    /// So this is only used for debugging.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Small(n) => f.pad(&format!("{n}")),
            Self::Big(n) => f.pad(&format!("{}", **n)),
        }
    }
}

impl From<BigIntId> for BigInt {
    fn from(id: BigIntId) -> Self {
        (*id).clone()
    }
}

impl GenericInt {
    pub const fn new(value: i64) -> Self {
        Self::Small(value)
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn to_bigint(&self) -> BigInt {
        match self {
            Self::Small(n) => BigInt::from(*n),
            Self::Big(n) => (*n).into(),
        }
    }

    fn promote(lhs: &Self, rhs: &Self) -> (BigInt, BigInt) {
        (lhs.to_bigint(), rhs.to_bigint())
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Self::Small(n) => *n == 0,
            Self::Big(n) => n.is_zero(),
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
                                Self::promote(&GenericInt::Small(a), &GenericInt::Small(b));
                            *heap.add_big_int(big_a.$method(big_b)).as_generic_int()
                        }
                    },
                    (lhs, rhs) => {
                        let (big_lhs, big_rhs) = Self::promote(&lhs, &rhs);
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
                if rhs.is_zero() {
                    return Err("Division by zero".to_string());
                }

                match (self, rhs) {
                    (GenericInt::Small(a), GenericInt::Small(b)) => match a.$checked_method(b) {
                        Some(res) => Ok(GenericInt::Small(res)),
                        None => {
                            let (big_a, big_b) =
                                Self::promote(&GenericInt::Small(a), &GenericInt::Small(b));
                            Ok(*heap.add_big_int(big_a.$method(big_b)).as_generic_int())
                        }
                    },
                    (lhs, rhs) => {
                        let (big_lhs, big_rhs) = Self::promote(&lhs, &rhs);
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
                        let (big_lhs, big_rhs) = Self::promote(&lhs, &rhs);
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
impl PartialEq for GenericInt {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Small(a), Self::Small(b)) => a == b,
            (Self::Big(a), Self::Big(b)) => **a == **b,
            (Self::Small(a), Self::Big(b)) => BigInt::from(*a) == **b,
            (Self::Big(a), Self::Small(b)) => **a == BigInt::from(*b),
        }
    }
}

impl Hash for GenericInt {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Small(n) => BigInt::from(*n).hash(state),
            Self::Big(n) => (**n).hash(state),
        }
    }
}

impl PartialOrd for GenericInt {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Small(a), Self::Small(b)) => a.partial_cmp(b),
            (Self::Big(a), Self::Big(b)) => a.partial_cmp(b),
            (Self::Small(a), Self::Big(b)) => BigInt::from(*a).partial_cmp(b),
            (Self::Big(a), Self::Small(b)) => (**a).partial_cmp(&BigInt::from(*b)),
        }
    }
}

// Comparisons against integers (i64)
impl PartialEq<i64> for GenericInt {
    fn eq(&self, other: &i64) -> bool {
        match self {
            Self::Small(a) => a == other,
            Self::Big(a) => **a == BigInt::from(*other),
        }
    }
}

impl PartialOrd<i64> for GenericInt {
    fn partial_cmp(&self, other: &i64) -> Option<std::cmp::Ordering> {
        match self {
            Self::Small(a) => a.partial_cmp(other),
            Self::Big(a) => (**a).partial_cmp(&BigInt::from(*other)),
        }
    }
}

impl GenericInt {
    #[allow(clippy::option_if_let_else)]
    fn pow(self, rhs: Self, heap: &mut Heap) -> Result<Self, String> {
        if rhs < 0 {
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
                .add_big_int((*a).clone().pow(
                    u64::try_from((*b).clone()).expect("Previously checked that rhs is positive"),
                ))
                .as_generic_int()),
            (Self::Small(a), Self::Big(b)) => Ok(*heap
                .add_big_int(BigInt::from(a).pow(
                    u64::try_from((*b).clone()).expect("Previously checked that rhs is positive"),
                ))
                .as_generic_int()),
            (Self::Big(a), Self::Small(b)) => Ok(*heap
                .add_big_int(
                    (*a).clone()
                        .pow(u64::try_from(b).expect("Previously checked that rhs is positive")),
                )
                .as_generic_int()),
        }
    }

    fn neg(self, heap: &mut Heap) -> Self {
        match self {
            Self::Small(n) => Self::Small(n.neg()),
            Self::Big(n) => *heap.add_big_int((*n).clone().neg()).as_generic_int(),
        }
    }
}

impl From<GenericInt> for f64 {
    #[allow(clippy::option_if_let_else)]
    #[allow(clippy::cast_precision_loss)]
    fn from(n: GenericInt) -> Self {
        match n {
            GenericInt::Small(n) => n as Self,
            GenericInt::Big(n) => match i64::try_from((*n).clone()) {
                Ok(n) => n as Self,
                Err(_) => Self::INFINITY, // Or f64::MAX?
            },
        }
    }
}

impl TryFrom<GenericInt> for i32 {
    type Error = String;
    #[allow(clippy::option_if_let_else)]
    fn try_from(n: GenericInt) -> Result<Self, Self::Error> {
        match n {
            GenericInt::Small(n) => match Self::try_from(n) {
                Ok(n) => Ok(n),
                Err(_) => Err("Number too large to fit in i32".to_string()),
            },
            GenericInt::Big(n) => match Self::try_from((*n).clone()) {
                Ok(n) => Ok(n),
                Err(_) => Err("Number too large to fit in i32".to_string()),
            },
        }
    }
}

impl TryFrom<GenericInt> for u64 {
    type Error = String;
    #[allow(clippy::option_if_let_else)]
    fn try_from(n: GenericInt) -> Result<Self, Self::Error> {
        match n {
            GenericInt::Small(n) => match Self::try_from(n) {
                Ok(n) => Ok(n),
                Err(_) => Err("Number loses sign.".to_string()),
            },
            GenericInt::Big(n) => match Self::try_from((*n).clone()) {
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

impl TryFrom<GenericInt> for usize {
    type Error = String;
    #[allow(clippy::option_if_let_else)]
    fn try_from(n: GenericInt) -> Result<Self, Self::Error> {
        match n {
            GenericInt::Small(n) => match Self::try_from(n) {
                Ok(n) => Ok(n),
                Err(_) => Err("Number too large to fit in usize".to_string()),
            },
            GenericInt::Big(n) => match Self::try_from((*n).clone()) {
                Ok(n) => Ok(n),
                Err(_) => Err("Number too large to fit in usize".to_string()),
            },
        }
    }
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(num) => f.pad(&format!("{num:?}")),
            Self::Integer(num) => f.pad(&format!("{num}")),
        }
    }
}

impl From<Number> for f64 {
    fn from(n: Number) -> Self {
        match n {
            Number::Float(f) => f,
            Number::Integer(i) => Self::from(i),
        }
    }
}

impl From<i64> for Number {
    fn from(n: i64) -> Self {
        Self::Integer(n.into())
    }
}

impl Number {
    /// Expressions `a**b` on numbers produce integers, only if
    /// both are already integers and `b`is not negative.
    /// Everything else produces a float result.
    #[allow(clippy::option_if_let_else)]
    pub(super) fn pow(self, exp: Self, heap: &mut Heap) -> Self {
        match (self, exp) {
            (Self::Integer(a), Self::Integer(b)) if b >= 0 => Self::Integer(
                a.pow(b, heap)
                    .expect("Only calling integer pow for exp >= 0"),
            ),
            (Self::Float(a), Self::Integer(b)) => match i32::try_from(b) {
                Ok(b) => Self::Float(a.powi(b)),
                Err(_) => Self::Float(f64::INFINITY), // Or f64::MAX?
            },
            (Self::Integer(a), Self::Float(b)) => Self::Float(f64::from(a).powf(b)),
            (Self::Float(a), Self::Float(b)) => Self::Float(a.powf(b)),
            (Self::Integer(a), Self::Integer(b)) => match i32::try_from(b) {
                Ok(b) => Self::Float(f64::from(a).powi(b)),
                Err(_) => Self::Float(f64::INFINITY), // Or f64::MAX?
            },
        }
    }

    /// Floor division only produces integers if both operands are themselves integers.
    /// Otherwise the result is a floored float.
    pub(super) fn floor_div(self, rhs: Self, heap: &mut Heap) -> Result<Self, String> {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Integer((a.div(b, heap))?)),
            (Self::Float(a), Self::Integer(b)) => Ok(Self::Float((a / f64::from(b)).floor())),
            (Self::Integer(a), Self::Float(b)) => Ok(Self::Float((f64::from(a) / b).floor())),
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
    pub(crate) fn div(self, rhs: Self, _heap: &mut Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Float(f64::from(a) / f64::from(b)),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a / f64::from(b)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(f64::from(a) / b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a / b),
        }
    }
    pub(crate) fn add(self, rhs: Self, heap: &mut Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a.add(b, heap)),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a + f64::from(b)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(f64::from(a) + b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a + b),
        }
    }

    pub(crate) fn sub(self, rhs: Self, heap: &mut Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a.sub(b, heap)),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a - f64::from(b)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(f64::from(a) - b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a - b),
        }
    }

    pub(crate) fn mul(self, rhs: Self, heap: &mut Heap) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a.mul(b, heap)),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a * f64::from(b)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(f64::from(a) * b),
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
            (Self::Float(a), Self::Integer(b)) => Ok(Self::Float(a % f64::from(b))),
            (Self::Integer(a), Self::Float(b)) => Ok(Self::Float(f64::from(a) % b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a % b)),
        }
    }
}

/// Uncaptured (open) upvalues point to the stack index of the value,
/// while captured upvalues point to the value in the heap.
#[derive(Debug, PartialEq, Eq, PartialOrd, Clone)]
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

impl std::fmt::Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!("{}", *self.function))
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
    ) -> Self {
        let upvalue_count = function.upvalue_count;
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

#[derive(Debug, PartialOrd, Clone)]
pub struct BoundMethod {
    // Probably could be an InstanceId now
    pub(super) receiver: Value,
    // Has to be a general Value because it can be a NativeMethod or Closure
    pub(super) method: Value,
}

impl std::fmt::Display for BoundMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!(
            "<bound method {}.{} of {}>",
            *self.receiver_class_name(),
            *self.method_name(),
            self.receiver
        ))
    }
}

impl BoundMethod {
    fn method_name(&self) -> StringId {
        match self.method {
            Value::NativeMethod(native) => native.name,
            Value::Closure(closure) => closure.function.name,
            x => unreachable!(
                "Bound method only binds over closures or native methods, got `{}` instead.",
                x
            ),
        }
    }

    fn receiver_class_name(&self) -> StringId {
        match self.receiver {
            Value::Instance(instance) => instance.class.name,
            x => unreachable!(
                "Bound methods can only have instances as receivers, got `{}` instead.",
                x
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
#[derive(Debug, PartialEq, Eq, PartialOrd, Clone)]
pub struct Function {
    pub(super) arity: usize,
    pub(super) chunk: Chunk,
    pub(super) name: StringId,
    pub(super) upvalue_count: usize,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!("<fn {}>", *self.name))
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
        f.pad(&format!("<native fn {}>", *self.name))
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
        f.pad(&format!(
            "<native method {} of class {}>",
            *self.name, *self.class
        ))
    }
}

pub type NativeFunctionImpl = fn(&mut VM, &mut [&mut Value]) -> Result<Value, String>;
pub type NativeMethodImpl = fn(&mut VM, &mut Value, &mut [&mut Value]) -> Result<Value, String>;
pub type ModuleContents = Vec<(&'static str, &'static [u8], NativeFunctionImpl)>;

const fn always_equals<T>(_: &T, _: &T) -> bool {
    true
}

#[derive(Debug, PartialEq, Eq, Clone, Derivative)]
#[derivative(PartialOrd)]
pub struct Class {
    pub(super) name: StringId,
    #[derivative(PartialOrd = "ignore")]
    // Have to be general Value because it can be a nativemethod(how?) or a closure
    pub(super) methods: HashMap<StringId, Value>,
    pub(super) is_native: bool,
}

impl Class {
    #[must_use]
    pub(super) fn new(name: StringId, is_native: bool) -> Self {
        Self {
            name,
            methods: HashMap::default(),
            is_native,
        }
    }
}

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!("<class {}>", *self.name))
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
}

impl std::fmt::Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.backing {
            Some(native_class) => f.pad(&format!("{native_class}")),
            None => f.pad(&format!("<{} instance>", *(self.class.name))),
        }
    }
}

impl PartialEq for BoundMethod {
    fn eq(&self, _other: &Self) -> bool {
        // Two different bound methods are always considered different
        false
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Derivative)]
#[derivative(PartialOrd)]
pub struct Module {
    pub(super) name: StringId,
    pub(super) path: PathBuf,
    #[derivative(PartialOrd = "ignore")]
    pub(super) globals: HashMap<StringId, Global>,
    pub(super) names_to_import: Option<Vec<StringId>>,
    pub(super) alias: StringId,
}

impl Module {
    pub(super) fn new(
        name: StringId,
        path: PathBuf,
        names_to_import: Option<Vec<StringId>>,
        alias: StringId,
    ) -> Self {
        Self {
            name,
            path,
            globals: HashMap::default(),
            names_to_import,
            alias,
        }
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!("<module {}>", *self.name))
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
            _ => unreachable!("Expected Number, found `{}`", self),
        }
    }

    pub(super) fn as_closure(&self) -> &ClosureId {
        match self {
            Self::Closure(c) => c,
            _ => unreachable!("Expected Closure, found `{}`", self),
        }
    }

    pub(super) fn as_string(&self) -> &StringId {
        match self {
            Self::String(s) => s,
            _ => unreachable!("Expected String, found `{}`", self),
        }
    }

    pub(super) fn as_native_method(&self) -> &NativeMethodId {
        match self {
            Self::NativeMethod(n) => n,
            _ => unreachable!("Expected Native, found `{}`", self),
        }
    }

    pub(super) fn as_function(&self) -> &FunctionId {
        match self {
            Self::Function(f) => f,
            _ => unreachable!("Expected Function, found `{}`", self),
        }
    }

    pub(super) fn as_class(&self) -> &ClassId {
        match self {
            Self::Class(c) => c,
            _ => unreachable!("Expected Class, found `{}`", self),
        }
    }

    pub(super) fn as_instance(&mut self) -> &InstanceId {
        match self {
            Self::Instance(i) => i,
            _ => unreachable!("Expected Instance, found `{}`", self),
        }
    }

    pub(super) fn as_class_mut(&mut self) -> &mut ClassId {
        match self {
            Self::Class(c) => c,
            _ => unreachable!("Expected Class, found `{}`", self),
        }
    }

    pub(super) fn upvalue_location(&self) -> &UpvalueId {
        match self {
            Self::Upvalue(v) => v,
            _ => unreachable!("Expected upvalue, found `{}`", self),
        }
    }

    pub(super) fn class_name(&self) -> StringId {
        match &self {
            Self::Instance(instance) => instance.class.name,
            x => unreachable!("Only instances have classes. Got `{}`", x),
        }
    }

    pub(super) fn as_module(&self) -> &ModuleId {
        match self {
            Self::Module(m) => m,
            _ => unreachable!("Expected Module, found `{}`", self),
        }
    }

    pub(super) fn as_list(&self) -> &List {
        match self {
            Self::Instance(inst) => match &inst.backing {
                Some(NativeClass::List(list)) => list,
                _ => unreachable!("Expected List, found `{}`", self),
            },
            _ => unreachable!("Expected List, found `{}`", self),
        }
    }

    pub(super) fn as_list_mut(&mut self) -> &mut List {
        match self {
            Self::Instance(inst) => match &mut inst.backing {
                Some(NativeClass::List(list)) => list,
                _ => unreachable!("Expected List, found something else."),
            },
            _ => unreachable!("Expected List, found `{}`", self),
        }
    }

    pub(super) fn as_list_iter_mut(&mut self) -> &mut ListIterator {
        match self {
            Self::Instance(inst) => match &mut inst.backing {
                Some(NativeClass::ListIterator(list_iter)) => list_iter,
                _ => unreachable!("Expected ListIterator, found something else."),
            },
            _ => unreachable!("Expected ListIterator, found `{}`", self),
        }
    }

    pub(super) fn as_set(&self) -> &Set {
        match self {
            Self::Instance(inst) => match &inst.backing {
                Some(NativeClass::Set(set)) => set,
                _ => unreachable!("Expected Set, found `{}`", self),
            },
            _ => unreachable!("Expected Set, found `{}`", self),
        }
    }

    pub(super) fn as_set_mut(&mut self) -> &mut Set {
        match self {
            Self::Instance(inst) => match &mut inst.backing {
                Some(NativeClass::Set(set)) => set,
                _ => unreachable!("Expected Set, found something else."),
            },
            _ => unreachable!("Expected Set, found `{}`", self),
        }
    }

    pub(super) fn as_dict(&self) -> &Dict {
        match self {
            Self::Instance(inst) => match &inst.backing {
                Some(NativeClass::Dict(dict)) => dict,
                _ => unreachable!("Expected Dict, found `{}`", self),
            },
            _ => unreachable!("Expected Dict, found `{}`", self),
        }
    }

    pub(super) fn as_dict_mut(&mut self) -> &mut Dict {
        match self {
            Self::Instance(inst) => match &mut inst.backing {
                Some(NativeClass::Dict(dict)) => dict,
                _ => unreachable!("Expected Dict, found something else."),
            },
            _ => unreachable!("Expected Dict, found `{}`", self),
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
}

impl std::fmt::Display for NativeClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::List(list) => list.fmt(f),
            Self::ListIterator(list_iter) => list_iter.fmt(f),
            Self::Set(set) => set.fmt(f),
            Self::Dict(dict) => dict.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone)]
pub struct List {
    pub(super) items: Vec<Value>,
}

impl List {
    #[must_use]
    pub(super) const fn new() -> Self {
        Self { items: Vec::new() }
    }
}

impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let comma_separated = format!(
            "[{}]",
            self.items
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        );
        f.pad(&comma_separated)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone)]
pub struct ListIterator {
    pub(super) list: Option<InstanceId>,
    pub(super) index: usize,
}

impl ListIterator {
    pub(super) const fn new(list: Option<InstanceId>) -> Self {
        Self { list, index: 0 }
    }

    pub(super) fn get_list(&self) -> Option<&List> {
        self.list.as_ref().and_then(|item| match &item.backing {
            Some(NativeClass::List(list)) => Some(list),
            _ => None,
        })
    }
}

impl std::fmt::Display for ListIterator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.list {
            Some(list) => f.pad(&format!("<list iterator of {}>", *list)),
            None => f.pad("<list iterator>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Set {
    pub(super) items: HashSet<Value>,
}

impl Set {
    #[must_use]
    pub(super) fn new() -> Self {
        Self {
            items: HashSet::default(),
        }
    }
}

impl std::fmt::Display for Set {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let comma_separated = format!(
            "{{{}}}",
            self.items
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        );
        f.pad(&comma_separated)
    }
}

#[derive(Debug, Clone)]
pub struct Dict {
    pub(super) items: HashMap<Value, Value>,
}

impl Dict {
    #[must_use]
    pub(super) fn new() -> Self {
        Self {
            items: HashMap::default(),
        }
    }
}

impl std::fmt::Display for Dict {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let comma_separated = format!(
            "{{{}}}",
            self.items
                .iter()
                .map(|(key, value)| format!("{key}: {value}"))
                .collect::<Vec<_>>()
                .join(", ")
        );
        f.pad(&comma_separated)
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
