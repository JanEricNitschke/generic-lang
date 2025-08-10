//! Runtime representation of generic runtime values.

mod classes;
mod functions;
mod natives;
mod number;

use crate::heap::{
    BigIntId, BoundMethodId, ClassId, ClosureId, FunctionId, Heap, InstanceId, ModuleId,
    NativeFunctionId, NativeMethodId, StringId, UpvalueId,
};
pub use classes::{BoundMethod, Class, Instance};
pub use functions::{Closure, Function, Module, Upvalue};
pub use natives::{
    Dict, List, ListIterator, ModuleContents, NativeClass, NativeFunction, NativeFunctionImpl,
    NativeMethod, NativeMethodImpl, Set,
};
pub use number::{GenericInt, Number};

use num_bigint::BigInt;
use rustc_hash::FxHasher;
use std::hash::{Hash, Hasher};

/// Central enum for the types of runtime values that exist in generic.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    StopIteration,

    Number(Number),

    String(StringId),

    Closure(ClosureId),
    Function(FunctionId),
    Module(ModuleId),
    Upvalue(UpvalueId),

    NativeFunction(NativeFunctionId),
    NativeMethod(NativeMethodId),

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

impl Value {
    pub(super) fn bound_method(receiver: Self, method: Self, heap: &mut Heap) -> Self {
        heap.add_bound_method(BoundMethod { receiver, method })
    }
}

// Conversions
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

// Retrieve the inner value
impl Value {
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

    pub(super) fn as_instance(&self) -> &InstanceId {
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
