//! Runtime representation of generic runtime values.

mod classes;
mod functions;
mod natives;
mod number;
pub mod utils;

use crate::heap::{
    BigIntId, BoundMethodId, ClassId, ClosureId, FunctionId, Heap, InstanceId, ModuleId,
    NativeFunctionId, NativeMethodId, StringId, UpvalueId,
};
pub use classes::{
    BoundMethod, Class, Instance, get_native_class_id, is_exception_subclass, is_subclass_of,
};
pub use functions::{Closure, Function, Module, Upvalue};
pub use natives::{
    Dict, Exception, Generator, GeneratorState, Interpolation, List, ListIterator, ModuleContents,
    NativeClass, NativeFunction, NativeFunctionImpl, NativeMethod, NativeMethodImpl, Range,
    RangeIterator, Set, Template, TemplateIterator, Tuple, TupleIterator,
};
pub use number::{GenericInt, GenericRational, Number};

use unicode_normalization::UnicodeNormalization;

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

#[cfg(test)]
#[test]
fn test_value_size() {
    let sizes = [
        size_of::<bool>(),
        size_of::<Number>(),
        size_of::<StringId>(),
        size_of::<ClosureId>(),
        size_of::<FunctionId>(),
        size_of::<ModuleId>(),
        size_of::<UpvalueId>(),
        size_of::<NativeFunctionId>(),
        size_of::<NativeMethodId>(),
        size_of::<ClassId>(),
        size_of::<InstanceId>(),
        size_of::<BoundMethodId>(),
    ];
    // From Number, because of GenericRational, because of GenericInt being 16
    assert_eq!(sizes.iter().copied().max().unwrap(), 32);
    assert_eq!(std::mem::size_of::<Value>(), 32);
}

// This is fake btw. But it is only used for hash,
// which throws unreachable for all the variants where it doesnt actually hold.
impl Value {
    pub(crate) fn eq(&self, other: &Self, heap: &Heap) -> bool {
        match (self, other) {
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Nil, Self::Nil) | (Self::StopIteration, Self::StopIteration) => true,
            (Self::Number(a), Self::Number(b)) => a.eq(b, heap),
            (Self::String(a), Self::String(b)) => {
                a == b || a.to_value(heap).nfc().eq(b.to_value(heap).nfc())
            }
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

    pub(crate) fn is(&self, other: &Self) -> bool {
        match (self, other) {
            // For immediate/stack values, identity is the same as equality
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Nil, Self::Nil) | (Self::StopIteration, Self::StopIteration) => true,
            (Self::Number(a), Self::Number(b)) => a == b, // Numbers are immediate values

            // For heap-allocated values, compare the IDs (object identity)
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Function(a), Self::Function(b)) => a == b,
            (Self::Module(a), Self::Module(b)) => a == b,
            (Self::Closure(a), Self::Closure(b)) => a == b,
            (Self::NativeFunction(a), Self::NativeFunction(b)) => a == b,
            (Self::NativeMethod(a), Self::NativeMethod(b)) => a == b,
            (Self::Upvalue(a), Self::Upvalue(b)) => a == b,
            (Self::Class(a), Self::Class(b)) => a == b,
            (Self::Instance(a), Self::Instance(b)) => a == b,
            (Self::BoundMethod(a), Self::BoundMethod(b)) => a == b,

            // Different types are never identical
            _ => false,
        }
    }

    pub fn to_string(&self, heap: &Heap) -> String {
        match self {
            Self::Bool(bool) => format!("{bool}"),
            Self::Number(num) => num.to_string(heap),
            Self::Nil => "nil".to_string(),
            Self::StopIteration => "StopIteration".to_string(),
            Self::String(s) => (*s.to_value(heap)).clone(),
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

    pub fn as_tuple<'a>(&self, heap: &'a Heap) -> &'a Tuple {
        match self {
            Self::Instance(inst) => match &inst.to_value(heap).backing {
                Some(NativeClass::Tuple(tuple)) => tuple,
                _ => unreachable!("Expected Tuple, found `{:?}`", self),
            },
            _ => unreachable!("Expected Tuple, found `{:?}`", self),
        }
    }

    pub(super) fn as_tuple_mut<'a>(&mut self, heap: &'a mut Heap) -> &'a mut Tuple {
        match self {
            Self::Instance(inst) => match &mut inst.to_value_mut(heap).backing {
                Some(NativeClass::Tuple(tuple)) => tuple,
                _ => unreachable!("Expected Tuple, found something else."),
            },
            _ => unreachable!("Expected Tuple, found `{:?}`", self),
        }
    }

    pub fn as_tuple_iter_mut<'a>(&mut self, heap: &'a mut Heap) -> &'a mut TupleIterator {
        match self {
            Self::Instance(inst) => match &mut inst.to_value_mut(heap).backing {
                Some(NativeClass::TupleIterator(tuple_iter)) => tuple_iter,
                _ => unreachable!("Expected TupleIterator, found something else."),
            },
            _ => unreachable!("Expected TupleIterator, found `{:?}`", self),
        }
    }

    pub(super) fn as_range<'a>(&self, heap: &'a Heap) -> &'a Range {
        match self {
            Self::Instance(inst) => match &inst.to_value(heap).backing {
                Some(NativeClass::Range(range)) => range,
                _ => unreachable!("Expected Range, found `{:?}`", self),
            },
            _ => unreachable!("Expected Range, found `{:?}`", self),
        }
    }

    pub(super) fn as_range_mut<'a>(&mut self, heap: &'a mut Heap) -> &'a mut Range {
        match self {
            Self::Instance(inst) => match &mut inst.to_value_mut(heap).backing {
                Some(NativeClass::Range(range)) => range,
                _ => unreachable!("Expected Range, found something else."),
            },
            _ => unreachable!("Expected Range, found `{:?}`", self),
        }
    }

    pub(super) fn as_range_iter_mut<'a>(&mut self, heap: &'a mut Heap) -> &'a mut RangeIterator {
        match self {
            Self::Instance(inst) => match &mut inst.to_value_mut(heap).backing {
                Some(NativeClass::RangeIterator(range_iter)) => range_iter,
                _ => unreachable!("Expected RangeIterator, found something else."),
            },
            _ => unreachable!("Expected RangeIterator, found `{:?}`", self),
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

    pub(super) fn as_exception<'a>(&self, heap: &'a Heap) -> &'a Exception {
        match self {
            Self::Instance(inst) => match &inst.to_value(heap).backing {
                Some(NativeClass::Exception(exception)) => exception,
                _ => unreachable!("Expected Exception, found `{:?}`", self),
            },
            _ => unreachable!("Expected Exception, found `{:?}`", self),
        }
    }

    pub(super) fn as_exception_mut<'a>(&mut self, heap: &'a mut Heap) -> &'a mut Exception {
        match self {
            Self::Instance(inst) => match &mut inst.to_value_mut(heap).backing {
                Some(NativeClass::Exception(exception)) => exception,
                _ => unreachable!("Expected Exception, found `{:?}`", self),
            },
            _ => unreachable!("Expected Exception, found `{:?}`", self),
        }
    }

    pub(super) fn as_generator<'a>(&self, heap: &'a Heap) -> &'a Generator {
        match self {
            Self::Instance(inst) => match &inst.to_value(heap).backing {
                Some(NativeClass::Generator(generator)) => generator,
                _ => unreachable!("Expected Generator, found `{:?}`", self),
            },
            _ => unreachable!("Expected Generator, found `{:?}`", self),
        }
    }

    pub(super) fn as_generator_mut<'a>(&mut self, heap: &'a mut Heap) -> &'a mut Generator {
        match self {
            Self::Instance(inst) => match &mut inst.to_value_mut(heap).backing {
                Some(NativeClass::Generator(generator)) => generator,
                _ => unreachable!("Expected Generator, found `{:?}`", self),
            },
            _ => unreachable!("Expected Generator, found `{:?}`", self),
        }
    }

    pub(super) fn as_template<'a>(&self, heap: &'a Heap) -> &'a Template {
        match self {
            Self::Instance(inst) => match &inst.to_value(heap).backing {
                Some(NativeClass::Template(template)) => template,
                _ => unreachable!("Expected Template, found `{:?}`", self),
            },
            _ => unreachable!("Expected Template, found `{:?}`", self),
        }
    }

    pub(super) fn as_template_iter_mut<'a>(
        &mut self,
        heap: &'a mut Heap,
    ) -> &'a mut TemplateIterator {
        match self {
            Self::Instance(inst) => match &mut inst.to_value_mut(heap).backing {
                Some(NativeClass::TemplateIterator(template_iter)) => template_iter,
                _ => unreachable!("Expected TemplateIterator, found something else."),
            },
            _ => unreachable!("Expected TemplateIterator, found `{:?}`", self),
        }
    }

    pub(super) fn as_interpolation<'a>(&self, heap: &'a Heap) -> &'a Interpolation {
        match self {
            Self::Instance(inst) => match &inst.to_value(heap).backing {
                Some(NativeClass::Interpolation(interp)) => interp,
                _ => unreachable!("Expected Interpolation, found `{:?}`", self),
            },
            _ => unreachable!("Expected Interpolation, found `{:?}`", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::heap::Heap;

    fn create_test_heap() -> Heap {
        Heap::new()
    }

    #[test]
    fn test_value_bool() {
        let val_true = Value::Bool(true);
        let val_false = Value::Bool(false);

        assert_eq!(val_true, Value::Bool(true));
        assert_eq!(val_false, Value::Bool(false));
        assert_ne!(val_true, val_false);
    }

    #[test]
    fn test_value_nil() {
        let val_nil = Value::Nil;

        assert_eq!(val_nil, Value::Nil);
    }

    #[test]
    fn test_value_stop_iteration() {
        let val_stop = Value::StopIteration;

        assert_eq!(val_stop, Value::StopIteration);
    }

    #[test]
    fn test_value_number_integer() {
        let num_int = Value::Number(Number::Integer(GenericInt::Small(42)));
        assert_eq!(
            num_int,
            Value::Number(Number::Integer(GenericInt::Small(42)))
        );

        let heap = create_test_heap();
        assert_eq!(num_int.to_string(&heap), "42");
    }

    #[test]
    fn test_value_number_float() {
        let num_float = Value::Number(Number::Float(7.42));
        assert_eq!(num_float, Value::Number(Number::Float(7.42)));

        let heap = create_test_heap();
        assert_eq!(num_float.to_string(&heap), "7.42");
    }

    #[test]
    fn test_value_number_comparison() {
        let num_int = Value::Number(Number::Integer(GenericInt::Small(42)));
        let num_float = Value::Number(Number::Float(7.42));
        assert_ne!(num_int, num_float);

        let zero_int = Value::Number(Number::Integer(GenericInt::Small(0)));
        let zero_float = Value::Number(Number::Float(0.0));
        let heap = create_test_heap();
        assert_eq!(zero_int.to_string(&heap), "0");
        assert_eq!(zero_float.to_string(&heap), "0.0");
    }

    #[test]
    fn test_value_from_bool() {
        let bool_val: Value = true.into();
        assert_eq!(bool_val, Value::Bool(true));

        let bool_val2: Value = false.into();
        assert_eq!(bool_val2, Value::Bool(false));
    }

    #[test]
    fn test_value_from_float() {
        let float_val: Value = 7.42.into();
        assert_eq!(float_val, Value::Number(Number::Float(7.42)));
    }

    #[test]
    fn test_value_from_integer() {
        let int_val: Value = 42i64.into();
        assert_eq!(
            int_val,
            Value::Number(Number::Integer(GenericInt::Small(42)))
        );
    }

    #[test]
    fn test_value_from_number() {
        let number = Number::Integer(GenericInt::Small(100));
        let number_val: Value = number.into();
        assert_eq!(
            number_val,
            Value::Number(Number::Integer(GenericInt::Small(100)))
        );
    }

    #[test]
    fn test_value_equality() {
        let mut heap = create_test_heap();

        // Same values should be equal
        assert!(Value::Bool(true).eq(&Value::Bool(true), &heap));
        assert!(Value::Nil.eq(&Value::Nil, &heap));
        assert!(Value::StopIteration.eq(&Value::StopIteration, &heap));

        let num1 = Value::Number(Number::Integer(GenericInt::Small(42)));
        let num2 = Value::Number(Number::Integer(GenericInt::Small(42)));
        assert!(num1.eq(&num2, &heap));

        // Different values should not be equal
        assert!(!Value::Bool(true).eq(&Value::Bool(false), &heap));
        assert!(!Value::Bool(true).eq(&Value::Nil, &heap));

        // Test string equality
        let str1_val = heap.add_string("hello".to_string());
        let str2_val = heap.add_string("hello".to_string());
        let str3_val = heap.add_string("world".to_string());

        // Same content strings should be equal
        assert!(str1_val.eq(&str2_val, &heap));
        // Different content strings should not be equal
        assert!(!str1_val.eq(&str3_val, &heap));
    }

    #[test]
    fn test_value_as_methods() {
        let mut heap = create_test_heap();

        // Test as_string
        let string_val = heap.add_string("test".to_string());
        let string_id = string_val.as_string();
        assert_eq!(string_id.to_value(&heap), "test");

        // Test as_generic_int (numbers use this instead of as_number)
        let number_val = Value::Number(Number::Integer(GenericInt::Small(42)));
        let generic_int = number_val.as_generic_int();
        assert_eq!(*generic_int, GenericInt::Small(42));
    }

    #[test]
    #[should_panic(expected = "Expected String")]
    fn test_value_as_string_panic() {
        let val = Value::Bool(true);
        val.as_string(); // Should panic
    }

    #[test]
    #[should_panic(expected = "Expected Number")]
    fn test_value_as_generic_int_panic() {
        let val = Value::Bool(true);
        val.as_generic_int(); // Should panic
    }
}
