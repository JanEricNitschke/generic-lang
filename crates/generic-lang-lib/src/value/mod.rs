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
    value_isinstance,
};
pub use functions::{Closure, Function, Module, Upvalue};
pub use natives::{
    Dict, DictIterMode, DictIterator, Exception, Generator, GeneratorState, Interpolation, List,
    ListIterator, ModuleContents, NativeClass, NativeFunction, NativeFunctionImpl, NativeMethod,
    NativeMethodImpl, Range, RangeIterator, Set, SetIterator, Template, TemplateIterator, Tuple,
    TupleIterator,
};
pub use number::{GenericInt, GenericRational, Number};

use paste::paste;
use unicode_normalization::UnicodeNormalization;

/// Central enum for the types of runtime values that exist in generic.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum Value {
    Bool(bool),
    #[default]
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

/// Implement `From<XId> for Value` for variants that hold the ID of the same name.
macro_rules! impl_from_for_value {
    ($($variant:ident),* $(,)?) => {
        paste! {
            $(
                impl From<[<$variant Id>]> for Value {
                    fn from(value: [<$variant Id>]) -> Self {
                        Self::$variant(value)
                    }
                }
            )*
        }
    };
}

impl_from_for_value!(
    String,
    Function,
    Closure,
    NativeFunction,
    NativeMethod,
    Upvalue,
    Class,
    Instance,
    BoundMethod,
    Module,
);

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

impl From<Number> for Value {
    fn from(n: Number) -> Self {
        Self::Number(n)
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

impl From<BigIntId> for Value {
    fn from(b: BigIntId) -> Self {
        Self::Number(Number::Integer(GenericInt::Big(b)))
    }
}

/// Implement `as_x` accessors for variants that directly hold an ID.
macro_rules! impl_as_variant {
    ($($variant:ident),* $(,)?) => {
        paste! {
            $(
                pub(super) fn [<as_ $variant:snake>](&self) -> &[<$variant Id>] {
                    match self {
                        Self::$variant(value) => value,
                        _ => unreachable!(
                            "Expected {}, found `{:?}`",
                            stringify!($variant),
                            self
                        ),
                    }
                }
            )*
        }
    };
}

// Retrieve the inner value
impl Value {
    impl_as_variant!(Closure, String, Function, Class, Instance, Module);

    pub(super) fn as_generic_int(&self) -> &GenericInt {
        match self {
            Self::Number(Number::Integer(n)) => n,
            _ => unreachable!("Expected Number, found `{:?}`", self),
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
}

/// Implement `as_x` accessors for instances backed by a native class.
macro_rules! impl_as_native_class {
    ($($ty:ident),* $(,)?) => {
        paste! {
            $(
                pub(super) fn [<as_ $ty:snake>]<'a>(&self, heap: &'a Heap) -> &'a $ty {
                    match self {
                        Self::Instance(inst) => match &inst.to_value(heap).backing {
                            Some(NativeClass::$ty(value)) => value,
                            _ => unreachable!(
                                "Expected {}, found `{:?}`",
                                stringify!($ty),
                                self
                            ),
                        },
                        _ => unreachable!("Expected {}, found `{:?}`", stringify!($ty), self),
                    }
                }
            )*
        }
    };
}

/// Implement `as_x_mut` accessors for instances backed by a native class.
/// The mutable borrow is on the heap data; the `Value` itself is only read.
macro_rules! impl_as_native_class_mut {
    ($($ty:ident),* $(,)?) => {
        paste! {
            $(
                pub(super) fn [<as_ $ty:snake _mut>]<'a>(&self, heap: &'a mut Heap) -> &'a mut $ty {
                    match self {
                        Self::Instance(inst) => match &mut inst.to_value_mut(heap).backing {
                            Some(NativeClass::$ty(value)) => value,
                            _ => unreachable!(
                                "Expected {}, found `{:?}`",
                                stringify!($ty),
                                self
                            ),
                        },
                        _ => unreachable!("Expected {}, found `{:?}`", stringify!($ty), self),
                    }
                }
            )*
        }
    };
}

impl Value {
    impl_as_native_class!(
        List,
        ListIterator,
        Tuple,
        TupleIterator,
        Range,
        Set,
        SetIterator,
        Dict,
        DictIterator,
        Exception,
        Generator,
        Template,
        TemplateIterator,
        Interpolation,
    );

    impl_as_native_class_mut!(
        List,
        ListIterator,
        Tuple,
        TupleIterator,
        Range,
        RangeIterator,
        Set,
        SetIterator,
        Dict,
        DictIterator,
        Exception,
        Generator,
        TemplateIterator,
    );
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
