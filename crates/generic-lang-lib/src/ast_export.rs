//! Conversion of compiler AST nodes into `ast` module values.
//!
//! Every struct becomes a class with all of its fields, enum unit/struct
//! variants become classes, and newtype variants are transparent unless their
//! payload is a `Vec`.
//!
//! `Literal` is the one value-only node: it converts to a Generic value
//! instead of a class. `Program` is converted manually because its comment map
//! is compiler metadata, not part of the exported tree.

use crate::compiler::ast::{
    AccessMode, BinaryOp, BoolOperator, ExprContext, FunctionKind, Literal, MethodKind, Program,
    UnaryOp,
};
use crate::types::{ConditionType, Location, LoopType, Mutability};
use crate::value::{GenericInt, GenericRational, Instance, Number, Value};
use crate::vm::VM;
use num_bigint::BigInt;

/// One derived type's exported class names.
pub(crate) struct AstClass(pub &'static [&'static str]);

inventory::collect!(AstClass);

/// All classes registered by the `ToAst` derive, plus `Program`.
pub(crate) fn ast_class_names() -> Vec<&'static str> {
    let mut names = inventory::iter::<AstClass>
        .into_iter()
        .flat_map(|class| class.0.iter().copied())
        .collect::<Vec<_>>();
    names.sort_unstable();
    names
}

/// Creates Generic values while exporting an AST node.
pub(crate) struct Converter<'a> {
    vm: &'a mut VM,
}

impl<'a> Converter<'a> {
    pub(crate) fn new(vm: &'a mut VM) -> Self {
        Self { vm }
    }

    fn string(&mut self, value: &str) -> Value {
        self.vm.heap.string_id(&value).into()
    }

    fn list(&mut self, items: Vec<Value>) -> Value {
        self.vm.new_list(items)
    }

    fn tuple(&mut self, items: Vec<Value>) -> Value {
        self.vm.new_tuple(items)
    }

    fn big_int(&mut self, value: &BigInt) -> Value {
        self.vm.heap.add_big_int(value.clone())
    }

    fn rational(&mut self, numerator: &BigInt, denominator: &BigInt) -> Value {
        let numerator = GenericInt::from_bigint(numerator.clone(), &mut self.vm.heap);
        let denominator = GenericInt::from_bigint(denominator.clone(), &mut self.vm.heap);
        let rational = GenericRational::new(numerator, denominator, &mut self.vm.heap)
            .expect("AST rational has a non-zero denominator");
        Value::from(Number::Rational(rational))
    }

    fn instance(&mut self, class_name: &str, fields: Vec<(&'static str, Value)>) -> Value {
        let class = *self
            .vm
            .heap
            .native_classes
            .get(class_name)
            .unwrap_or_else(|| panic!("missing AST class `{class_name}`"));
        let mut instance = Instance::new(class, None);
        for (key, value) in fields {
            instance.fields.insert(key.to_string(), value);
        }
        self.vm.heap.add_instance(instance)
    }

    pub(crate) fn class(&mut self, class_name: &str, fields: Vec<(&'static str, Value)>) -> Value {
        self.instance(class_name, fields)
    }

    fn location(&mut self, location: Location) -> Value {
        let position = |position: usize| {
            Value::from(i64::try_from(position).expect("source positions fit in a 64-bit integer"))
        };
        self.class(
            "Location",
            vec![
                ("start_line", position(*location.start_line)),
                ("start_column", position(*location.start_column)),
                ("start_offset", position(location.index)),
                ("end_line", position(*location.end_line)),
                ("end_column", position(*location.end_column)),
            ],
        )
    }
}

/// Convert an AST value into its Generic representation.
pub(crate) trait ToAst {
    fn to_ast(&self, converter: &mut Converter<'_>) -> Value;
}

impl ToAst for String {
    fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
        converter.string(self)
    }
}

impl ToAst for bool {
    fn to_ast(&self, _converter: &mut Converter<'_>) -> Value {
        Value::from(*self)
    }
}

impl ToAst for i64 {
    fn to_ast(&self, _converter: &mut Converter<'_>) -> Value {
        Value::from(*self)
    }
}

impl ToAst for f64 {
    fn to_ast(&self, _converter: &mut Converter<'_>) -> Value {
        Value::from(*self)
    }
}

impl ToAst for BigInt {
    fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
        converter.big_int(self)
    }
}

impl ToAst for Location {
    fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
        converter.location(*self)
    }
}

impl<T: ToAst> ToAst for Option<T> {
    fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
        self.as_ref()
            .map_or(Value::Nil, |value| value.to_ast(converter))
    }
}

impl<T: ToAst> ToAst for Vec<T> {
    fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
        let values = self.iter().map(|value| value.to_ast(converter)).collect();
        converter.list(values)
    }
}

impl<T: ToAst> ToAst for Box<T> {
    fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
        self.as_ref().to_ast(converter)
    }
}

impl<A: ToAst, B: ToAst> ToAst for (A, B) {
    fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
        let values = vec![self.0.to_ast(converter), self.1.to_ast(converter)];
        converter.tuple(values)
    }
}

macro_rules! impl_display_to_ast {
    ($($ty:ty),* $(,)?) => {
        $(
            impl ToAst for $ty {
                fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
                    converter.string(&self.to_string())
                }
            }
        )*
    };
}

impl_display_to_ast!(
    Mutability,
    ConditionType,
    LoopType,
    FunctionKind,
    MethodKind,
    AccessMode,
    ExprContext,
    UnaryOp,
    BinaryOp,
    BoolOperator,
);

impl ToAst for Literal {
    fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
        match self {
            Self::Nil => Value::Nil,
            Self::True => Value::from(true),
            Self::False => Value::from(false),
            Self::StopIteration => Value::StopIteration,
            Self::Int(value) => Value::from(*value),
            Self::BigInt(value) => converter.big_int(value),
            Self::Float(value) => Value::from(*value),
            Self::String(value) => converter.string(value),
            Self::Rational {
                numerator,
                denominator,
            } => converter.rational(numerator, denominator),
            Self::Tuple(items) => {
                let values = items.iter().map(|item| item.to_ast(converter)).collect();
                converter.tuple(values)
            }
        }
    }
}

inventory::submit! { AstClass(&["Program"]) }

impl ToAst for Program {
    fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
        let declarations = self.declarations.to_ast(converter);
        converter.class("Program", vec![("declarations", declarations)])
    }
}
