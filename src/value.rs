use crate::{
    chunk::Chunk,
    heap::{FunctionId, Heap, StringId, ValueId},
};
use derivative::Derivative;
use derive_more::{From, Neg};
use rustc_hash::FxHashMap as HashMap;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(Number),

    String(StringId),

    Function(FunctionId),
    Closure(Closure),
    NativeFunction(NativeFunction),
    NativeMethod(NativeMethod),

    Upvalue(Upvalue),

    Class(Class),
    Instance(Instance),
    BoundMethod(BoundMethod),

    List(List),
}

#[derive(Debug, Copy, PartialEq, PartialOrd, Clone, From, Neg)]
pub enum Number {
    Float(f64),
    Integer(i64),
}

#[allow(clippy::cast_possible_truncation, clippy::cast_precision_loss)]
pub fn ias_f64(i: i64) -> f64 {
    let result = i as f64;
    assert!(
        (result as i64 == i),
        "Could not losslessly convert i64 `{i}` to f64."
    );
    result
}

#[allow(clippy::cast_possible_truncation)]
pub fn ias_i32(i: i64) -> i32 {
    assert!(
        (i <= i64::from(i32::MAX)),
        "Could not losslessly convert i64 `{i}` to i32."
    );
    i as i32
}

#[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
pub fn ias_u64(i: i64) -> u64 {
    let result = i as u64;
    assert!(
        (result as i64 == i),
        "Could not losslessly convert i64 `{i}` to u64."
    );
    result
}

#[allow(clippy::cast_possible_truncation)]
pub const fn fas_i64(f: f64) -> i64 {
    f as i64
}

impl From<Number> for f64 {
    fn from(n: Number) -> Self {
        match n {
            Number::Float(n) => n,
            Number::Integer(n) => ias_f64(n),
        }
    }
}

impl From<Number> for i64 {
    fn from(n: Number) -> Self {
        match n {
            Number::Float(f) => fas_i64(f),
            Number::Integer(i) => i,
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

impl Number {
    pub fn pow(self, exp: Self) -> Self {
        match (self, exp) {
            (Self::Integer(a), Self::Integer(b)) if b > 0 => Self::Integer(a.pow(
                u32::try_from(b).unwrap_or_else(|_| panic!("Could not convert i64 `{b}` to u32.")),
            )),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a.powi(ias_i32(b))),
            (Self::Integer(a), Self::Float(b)) => Self::Float((ias_f64(a)).powf(b)),
            (Self::Float(a), Self::Float(b)) => Self::Float(a.powf(b)),
            (Self::Integer(a), Self::Integer(b)) => Self::Float(ias_f64(a).powi(ias_i32(b))),
        }
    }

    pub fn floor_div(self, exp: Self) -> Self {
        match (self, exp) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a / b),
            (Self::Float(a), Self::Integer(b)) => Self::Float((a / (ias_f64(b))).floor()),
            (Self::Integer(a), Self::Float(b)) => Self::Float(((ias_f64(a)) / b).floor()),
            (Self::Float(a), Self::Float(b)) => Self::Float((a / b).floor()),
        }
    }
}

impl ::core::ops::Div for Number {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Float(ias_f64(a) / ias_f64(b)),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a / ias_f64(b)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(ias_f64(a) / b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a / b),
        }
    }
}

impl ::core::ops::Add for Number {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a + b),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a + ias_f64(b)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(ias_f64(a) + b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a + b),
        }
    }
}

impl ::core::ops::Sub for Number {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a - b),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a - ias_f64(b)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(ias_f64(a) - b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a - b),
        }
    }
}

impl ::core::ops::Mul for Number {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a * b),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a * ias_f64(b)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(ias_f64(a) * b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a * b),
        }
    }
}

impl ::core::ops::BitAnd for Number {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a & b),
            _ => unreachable!("Did not get two integers for bitwise and."),
        }
    }
}

impl ::core::ops::BitOr for Number {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a | b),
            _ => unreachable!("Did not get two integers for bitwise or."),
        }
    }
}

impl ::core::ops::BitXor for Number {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a ^ b),
            _ => unreachable!("Did not get two integers for bitwise xor."),
        }
    }
}

impl ::core::ops::Rem for Number {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a % b),
            (Self::Float(a), Self::Integer(b)) => Self::Float(a % ias_f64(b)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(ias_f64(a) % b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a % b),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Upvalue {
    Open(usize),
    Closed(ValueId),
}

impl Upvalue {
    pub fn as_open(&self) -> usize {
        match self {
            Self::Open(n) => *n,
            Self::Closed(_) => unreachable!("Only call as_open on a known open upvalue!"),
        }
    }
}

#[derive(Debug, PartialOrd, Clone)]
pub struct Closure {
    pub function: FunctionId,
    pub upvalues: Vec<ValueId>,
    pub upvalue_count: usize,
}

impl PartialEq for Closure {
    fn eq(&self, _other: &Self) -> bool {
        // Two different closures are always considered different, even if they close over exactly the same things
        false
    }
}

impl Closure {
    pub fn new(function: FunctionId) -> Self {
        let upvalue_count = function.upvalue_count;
        Self {
            function,
            upvalues: Vec::with_capacity(upvalue_count),
            upvalue_count,
        }
    }
}

impl Value {
    pub fn closure(function: FunctionId) -> Self {
        let upvalue_count = function.upvalue_count;
        Self::Closure(Closure {
            function,
            upvalues: Vec::with_capacity(upvalue_count),
            upvalue_count,
        })
    }

    pub const fn bound_method(receiver: ValueId, method: ValueId) -> Self {
        Self::BoundMethod(BoundMethod { receiver, method })
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
        Self::Number(f.into())
    }
}

impl From<StringId> for Value {
    fn from(s: StringId) -> Self {
        Self::String(s)
    }
}

impl From<FunctionId> for Value {
    fn from(f: FunctionId) -> Self {
        Self::Function(f)
    }
}

impl From<Closure> for Value {
    fn from(c: Closure) -> Self {
        Self::Closure(c)
    }
}

impl From<Class> for Value {
    fn from(c: Class) -> Self {
        Self::Class(c)
    }
}

impl From<Instance> for Value {
    fn from(i: Instance) -> Self {
        Self::Instance(i)
    }
}

impl From<Number> for Value {
    fn from(n: Number) -> Self {
        Self::Number(n)
    }
}

impl From<List> for Value {
    fn from(l: List) -> Self {
        Self::List(l)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(bool) => f.pad(&format!("{bool}")),
            Self::Number(num) => f.pad(&format!("{num}")),
            Self::Nil => f.pad("nil"),
            Self::String(s) => f.pad(s),
            Self::Function(function_id) => f.pad(&format!("<fn {}>", *function_id.name)),
            Self::Closure(closure) => f.pad(&format!("<fn {}>", *closure.function.name)),
            Self::NativeFunction(fun) => f.pad(&format!("<native fn {}>", *fun.name)),
            Self::NativeMethod(method) => f.pad(&format!(
                "<native method {} of class {}>",
                *method.name, *method.class
            )),
            Self::Upvalue(_) => f.pad("upvalue"),
            Self::Class(c) => f.pad(&format!("<class {}>", *c.name)),
            Self::Instance(instance) => f.pad(&format!(
                "<{} instance>",
                *(*instance.class).as_class().name
            )),
            Self::BoundMethod(method) => f.pad(&format!(
                "<bound method {}.{} of {}>",
                *method.receiver_class_name(),
                *method.method_name(),
                *method.receiver,
            )),
            Self::List(list) => f.pad(&{
                let items = &list.items;
                let mut comma_separated = String::new();
                comma_separated.push('[');
                if !items.is_empty() {
                    for num in &items[0..items.len() - 1] {
                        comma_separated.push_str(&num.to_string());
                        comma_separated.push_str(", ");
                    }

                    comma_separated.push_str(&items[items.len() - 1].to_string());
                }
                comma_separated.push(']');
                comma_separated
            }),
        }
    }
}

impl Value {
    pub const fn is_falsey(&self) -> bool {
        matches!(self, Self::Bool(false) | Self::Nil)
    }

    pub fn as_closure(&self) -> &Closure {
        match self {
            Self::Closure(c) => c,
            _ => unreachable!("Expected Closure, found `{}`", self),
        }
    }

    pub fn as_native_method(&self) -> &NativeMethod {
        match self {
            Self::NativeMethod(n) => n,
            _ => unreachable!("Expected Native, found `{}`", self),
        }
    }

    pub fn as_function(&self) -> &FunctionId {
        match self {
            Self::Function(f) => f,
            _ => unreachable!("Expected Function, found `{}`", self),
        }
    }

    pub fn as_class(&self) -> &Class {
        match self {
            Self::Class(c) => c,
            _ => unreachable!("Expected Class, found `{}`", self),
        }
    }

    // pub fn as_instance(&self) -> &Instance {
    //     match self {
    //         Value::Instance(i) => i,
    //         _ => unreachable!("Expected Instance, found `{}`", self),
    //     }
    // }

    pub fn as_instance_mut(&mut self) -> &mut Instance {
        match self {
            Self::Instance(i) => i,
            _ => unreachable!("Expected Instance, found `{}`", self),
        }
    }

    pub fn as_class_mut(&mut self) -> &mut Class {
        match self {
            Self::Class(c) => c,
            _ => unreachable!("Expected Class, found `{}`", self),
        }
    }

    pub fn upvalue_location(&self) -> &Upvalue {
        match self {
            Self::Upvalue(v) => v,
            _ => unreachable!("Expected upvalue, found `{}`", self),
        }
    }

    pub fn upvalue_location_mut(&mut self) -> &mut Upvalue {
        match self {
            Self::Upvalue(v) => v,
            _ => unreachable!("Expected upvalue, found `{}`", self),
        }
    }

    pub fn class_name(&self) -> StringId {
        match &self {
            Self::Instance(instance) => instance.class.as_class().name,
            Self::List(list) => list.class.as_class().name,
            x => unreachable!(
                "Only instances and lists currently have classes. Got `{}`",
                x
            ),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: StringId,
    pub upvalue_count: usize,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!("<fn {}>", *self.name))
    }
}

impl Function {
    #[must_use]
    pub fn new(arity: usize, name: StringId) -> Self {
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
    pub name: StringId,
    pub arity: &'static [u8],

    #[derivative(
            Debug = "ignore",
            // Treat the implementation as always equal; we discriminate built-in functions by name
            PartialEq(compare_with = "always_equals"),
            PartialOrd = "ignore"
        )]
    pub fun: NativeFunctionImpl,
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, PartialOrd, Clone)]
pub struct NativeMethod {
    pub class: StringId,
    pub name: StringId,
    pub arity: &'static [u8],

    #[derivative(
            Debug = "ignore",
            // Treat the implementation as always equal; we discriminate built-in functions by name
            PartialEq(compare_with = "always_equals"),
            PartialOrd = "ignore"
        )]
    pub fun: NativeMethodImpl,
}

pub type NativeFunctionImpl = fn(&mut Heap, &[&ValueId]) -> Result<ValueId, String>;
pub type NativeMethodImpl = fn(&mut Heap, &ValueId, &[&ValueId]) -> Result<ValueId, String>;

const fn always_equals<T>(_: &T, _: &T) -> bool {
    true
}

#[derive(Debug, PartialEq, Clone, Derivative)]
#[derivative(PartialOrd)]
pub struct Class {
    pub name: StringId,
    #[derivative(PartialOrd = "ignore")]
    pub methods: HashMap<StringId, ValueId>,
    pub is_native: bool,
}

impl Class {
    #[must_use]
    pub fn new(name: StringId, is_native: bool) -> Self {
        Self {
            name,
            methods: HashMap::default(),
            is_native,
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, PartialOrd, Clone)]
pub struct Instance {
    pub class: ValueId,
    #[derivative(PartialOrd = "ignore")]
    pub fields: HashMap<String, ValueId>,
}

impl Instance {
    #[must_use]
    pub fn new(class: ValueId) -> Self {
        Self {
            class,
            fields: HashMap::default(),
        }
    }
}

#[derive(Debug, PartialOrd, Clone)]
pub struct BoundMethod {
    pub receiver: ValueId,
    pub method: ValueId,
}

impl BoundMethod {
    fn method_name(&self) -> StringId {
        let method = &*self.method;
        match method {
            Value::NativeMethod(native) => native.name,
            Value::Closure(closure) => closure.function.name,
            x => unreachable!(
                "Bound method only binds over closures or native methods, got `{}` instead.",
                x
            ),
        }
    }

    fn receiver_class_name(&self) -> StringId {
        match &*self.receiver {
            Value::Instance(instance) => instance.class.as_class().name,
            Value::List(list) => list.class.as_class().name,
            x => unreachable!(
                "Bound methods can only have instances or lists as receivers, got `{}` instead.",
                x
            ),
        }
    }
}

impl PartialEq for BoundMethod {
    fn eq(&self, _other: &Self) -> bool {
        // Two different bound methods are always considered different
        false
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct List {
    pub items: Vec<ValueId>,
    pub class: ValueId,
}

impl List {
    #[must_use]
    pub const fn new(array_class: ValueId) -> Self {
        Self {
            items: Vec::new(),
            class: array_class,
        }
    }
}
