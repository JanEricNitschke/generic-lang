use crate::{
    chunk::Chunk,
    heap::{
        BoundMethodId, ClassId, ClosureId, FunctionId, Heap, InstanceId, ListId, ModuleId,
        NativeFunctionId, NativeMethodId, StringId, UpvalueId,
    },
    vm::Global,
};
use derivative::Derivative;
use derive_more::{From, Neg};
use rustc_hash::FxHashMap as HashMap;

use std::path::PathBuf;
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Value {
    Bool(bool),
    Nil,
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

    // This should really just be "NativeClass"
    List(ListId),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(bool) => f.pad(&format!("{bool}")),
            Self::Number(num) => f.pad(&format!("{num}")),
            Self::Nil => f.pad("nil"),
            Self::String(s) => f.pad(s),
            // Can i do all of these just like string?
            Self::Function(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::Closure(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::NativeFunction(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::NativeMethod(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::Class(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::Instance(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::BoundMethod(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::List(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::Upvalue(ref_id) => f.pad(&format!("{}", **ref_id)),
            Self::Module(ref_id) => f.pad(&format!("{}", **ref_id)),
        }
    }
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
    Closed(Value),
}

impl std::fmt::Display for Upvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("upvalue")
    }
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
    pub upvalues: Vec<UpvalueId>,
    pub upvalue_count: usize,
    pub is_module: bool,
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
    pub fn new(function: FunctionId, is_module: bool) -> Self {
        let upvalue_count = function.upvalue_count;
        Self {
            function,
            upvalues: Vec::with_capacity(upvalue_count),
            upvalue_count,
            is_module,
        }
    }
}

impl Value {
    pub fn bound_method(receiver: Self, method: Self, heap: &mut Heap) -> Self {
        heap.add_bound_method(BoundMethod { receiver, method })
    }
}

#[derive(Debug, PartialOrd, Clone)]
pub struct BoundMethod {
    // Has to be a general Value because it can be an Instance or List
    pub receiver: Value,
    // Has to be a general Value because it can be a NativeMethod or Closure
    pub method: Value,
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
            Value::List(list) => list.class.name,
            x => unreachable!(
                "Bound methods can only have instances or lists as receivers, got `{}` instead.",
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

impl std::fmt::Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!("<native fn {}>", *self.name))
    }
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

impl std::fmt::Display for NativeMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!(
            "<native method {} of class {}>",
            *self.name, *self.class
        ))
    }
}

pub type NativeFunctionImpl = fn(&mut Heap, &mut [&mut Value]) -> Result<Value, String>;
pub type NativeMethodImpl = fn(&mut Heap, &mut Value, &mut [&mut Value]) -> Result<Value, String>;

const fn always_equals<T>(_: &T, _: &T) -> bool {
    true
}

#[derive(Debug, PartialEq, Clone, Derivative)]
#[derivative(PartialOrd)]
pub struct Class {
    pub name: StringId,
    #[derivative(PartialOrd = "ignore")]
    // Have to be general Value because it can be a nativemethod(how?) or a closure
    pub methods: HashMap<StringId, Value>,
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

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!("<class {}>", *self.name))
    }
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, PartialOrd, Clone)]
pub struct Instance {
    pub class: ClassId,
    #[derivative(PartialOrd = "ignore")]
    pub fields: HashMap<String, Value>,
}

impl Instance {
    #[must_use]
    pub fn new(class: Value) -> Self {
        let id = *class.as_class();
        Self {
            class: id,
            fields: HashMap::default(),
        }
    }
}

impl std::fmt::Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!("<{} instance>", *(self.class.name)))
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
    pub items: Vec<Value>,
    pub class: ClassId,
}

impl List {
    #[must_use]
    pub fn new(array_class: Value) -> Self {
        Self {
            items: Vec::new(),
            class: *array_class.as_class(),
        }
    }
}

impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let items = &self.items;
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
        f.pad(&comma_separated)
    }
}

#[derive(Debug, PartialEq, Clone, Derivative)]
#[derivative(PartialOrd)]
pub struct Module {
    pub name: StringId,
    pub path: PathBuf,
    #[derivative(PartialOrd = "ignore")]
    pub globals: HashMap<StringId, Global>,
}

impl Module {
    pub fn new(name: StringId, path: PathBuf) -> Self {
        Self {
            name,
            path,
            globals: HashMap::default(),
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
        Self::Number(f.into())
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

impl From<ListId> for Value {
    fn from(l: ListId) -> Self {
        Self::List(l)
    }
}

impl From<ModuleId> for Value {
    fn from(m: ModuleId) -> Self {
        Self::Module(m)
    }
}

impl Value {
    pub const fn is_falsey(&self) -> bool {
        matches!(self, Self::Bool(false) | Self::Nil)
    }

    pub fn as_closure(&self) -> &ClosureId {
        match self {
            Self::Closure(c) => c,
            _ => unreachable!("Expected Closure, found `{}`", self),
        }
    }

    pub fn as_string(&self) -> &StringId {
        match self {
            Self::String(s) => s,
            _ => unreachable!("Expected String, found `{}`", self),
        }
    }

    pub fn as_native_method(&self) -> &NativeMethodId {
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

    pub fn as_class(&self) -> &ClassId {
        match self {
            Self::Class(c) => c,
            _ => unreachable!("Expected Class, found `{}`", self),
        }
    }

    pub fn as_instance_mut(&mut self) -> &mut InstanceId {
        match self {
            Self::Instance(i) => i,
            _ => unreachable!("Expected Instance, found `{}`", self),
        }
    }

    pub fn as_class_mut(&mut self) -> &mut ClassId {
        match self {
            Self::Class(c) => c,
            _ => unreachable!("Expected Class, found `{}`", self),
        }
    }

    pub fn upvalue_location(&self) -> &UpvalueId {
        match self {
            Self::Upvalue(v) => v,
            _ => unreachable!("Expected upvalue, found `{}`", self),
        }
    }

    pub fn class_name(&self) -> StringId {
        match &self {
            Self::Instance(instance) => instance.class.name,
            Self::List(list) => list.class.name,
            x => unreachable!(
                "Only instances and lists currently have classes. Got `{}`",
                x
            ),
        }
    }

    // pub fn as_module(&self) -> &ModuleId {
    //     match self {
    //         Self::Module(m) => m,
    //         _ => unreachable!("Expected Module, found `{}`", self),
    //     }
    // }
}
