use crate::{
    chunk::Chunk,
    heap::{
        BoundMethodId, ClassId, ClosureId, FunctionId, Heap, InstanceId, ModuleId,
        NativeFunctionId, NativeMethodId, StringId, UpvalueId,
    },
    vm::{Global, VM},
};
use derivative::Derivative;
use derive_more::{From, Neg};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
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
                Number::Float(_) => unreachable!(
                    "Only hashable types are Bool, Nil, Integer, and String, got {self}."
                ),
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
    pub(super) fn pow(self, exp: Self) -> Self {
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

    pub(super) fn floor_div(self, exp: Self) -> Self {
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone)]
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
    pub(super) fn as_open(&self) -> usize {
        match self {
            Self::Open(n) => *n,
            Self::Closed(_) => unreachable!("Only call as_open on a known open upvalue!"),
        }
    }
}

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
            Self::Bool(_) | Self::Nil | Self::Number(Number::Integer(_)) | Self::String(_)
        )
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

    pub(super) fn as_instance_mut(&mut self) -> &mut InstanceId {
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
        let items = &self.items;
        let mut comma_separated = String::new();
        comma_separated.push('{');
        if !items.is_empty() {
            for num in items {
                comma_separated.push_str(&num.to_string());
                comma_separated.push_str(", ");
            }
            // Remove the last ", "
            comma_separated.pop();
            comma_separated.pop();
        }
        comma_separated.push('}');
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
        let items = &self.items;
        let mut comma_separated = String::new();
        comma_separated.push('{');
        if !items.is_empty() {
            for (key, value) in items {
                comma_separated.push_str(&key.to_string());
                comma_separated.push_str(": ");
                comma_separated.push_str(&value.to_string());
                comma_separated.push_str(", ");
            }
            // Remove the last ", "
            comma_separated.pop();
            comma_separated.pop();
        }
        comma_separated.push('}');
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
