//! Conversion of `ast` module values back into compiler AST nodes.

use crate::RunOptions;
use crate::ast_export::{Converter, ToAst};
use crate::compiler::ast::{
    AccessMode, BinaryOp, BoolOperator, Expr, ExprContext, FunctionKind, Literal, MethodKind,
    Program, Stmt, UnaryOp,
};
use crate::compiler::comments::CommentMap;
use crate::compiler::{LocationLessEq, Parser, format_expression_ast, format_program_ast};
use crate::config::AST_IMPORT_MAX_DEPTH;
use crate::heap::Heap;
use crate::types::{ConditionType, Location, LoopType, Mutability};
use crate::value::{GenericInt, Instance, NativeClass, Number, Value};
use crate::vm::VM;

/// Why importing an exported AST value failed.
///
/// The importer has no VM and cannot throw directly. The caller maps these:
///
/// - `Type` -> `TypeError`: missing field, wrong class/value, invalid enum
///   string, or an unrepresentable raw value.
/// - `Recursion` -> `RecursionError`: the AST is cyclic or too deeply nested.
#[derive(Debug)]
pub(crate) enum ImportError {
    Type(String),
    Recursion,
}

/// Reads exported AST instances from the heap.
pub(crate) struct Importer<'a> {
    heap: &'a Heap,
    depth: usize,
}

/// Convert an exported AST value into its compiler representation.
pub(crate) trait FromAst: Sized {
    fn from_ast(importer: &mut Importer<'_>, value: Option<Value>) -> Result<Self, ImportError>;
}

impl<'a> Importer<'a> {
    pub(crate) const fn new(heap: &'a Heap) -> Self {
        Self { heap, depth: 0 }
    }

    pub(crate) fn enter(&mut self) -> Result<(), ImportError> {
        if self.depth >= AST_IMPORT_MAX_DEPTH {
            return Err(ImportError::Recursion);
        }
        self.depth += 1;
        Ok(())
    }

    pub(crate) fn leave(&mut self) {
        self.depth -= 1;
    }

    pub(crate) fn class_name(&self, value: Value) -> Option<String> {
        let Value::Instance(instance) = value else {
            return None;
        };
        Some(
            instance
                .to_value(self.heap)
                .class
                .to_value(self.heap)
                .name
                .to_value(self.heap)
                .clone(),
        )
    }

    pub(crate) fn instance(&self, value: Value, class: &str) -> Result<&'a Instance, ImportError> {
        let Value::Instance(instance) = value else {
            return Err(self.unexpected(class, value));
        };
        let instance = instance.to_value(self.heap);
        if instance.class.to_value(self.heap).name.to_value(self.heap) != class {
            return Err(self.unexpected(class, value));
        }
        Ok(instance)
    }

    // Kept as methods so generated code reads `importer.field(...)` and
    // `importer.required(...)`.
    #[allow(clippy::unused_self)]
    pub(crate) fn field(&self, instance: &Instance, name: &str) -> Option<Value> {
        instance.fields.get(name).copied()
    }

    #[allow(clippy::unused_self)]
    pub(crate) fn required(
        &self,
        value: Option<Value>,
        expected: &str,
    ) -> Result<Value, ImportError> {
        value.ok_or_else(|| ImportError::Type(format!("missing {expected}")))
    }

    pub(crate) fn unexpected(&self, expected: &str, value: Value) -> ImportError {
        let got = match self.class_name(value) {
            Some(class) => class,
            None => value.to_string(self.heap),
        };
        ImportError::Type(format!("expected {expected}, got: {got}"))
    }

    pub(crate) fn string(&self, value: Value) -> Result<String, ImportError> {
        match value {
            Value::String(string) => Ok(string.to_value(self.heap).clone()),
            _ => Err(self.unexpected("string", value)),
        }
    }

    pub(crate) fn boolean(&self, value: Value) -> Result<bool, ImportError> {
        match value {
            Value::Bool(boolean) => Ok(boolean),
            _ => Err(self.unexpected("bool", value)),
        }
    }

    pub(crate) fn enum_name(&self, value: Value) -> Result<String, ImportError> {
        self.string(value)
    }

    pub(crate) fn list_items(&self, value: Value) -> Result<Vec<Value>, ImportError> {
        let Value::Instance(instance) = value else {
            return Err(self.unexpected("List", value));
        };
        match &instance.to_value(self.heap).backing {
            Some(NativeClass::List(list)) => Ok(list.items.clone()),
            _ => Err(self.unexpected("List", value)),
        }
    }

    pub(crate) fn tuple_items(&self, value: Value) -> Result<Vec<Value>, ImportError> {
        let Value::Instance(instance) = value else {
            return Err(self.unexpected("Tuple", value));
        };
        match &instance.to_value(self.heap).backing {
            Some(NativeClass::Tuple(tuple)) => Ok(tuple.items().to_vec()),
            _ => Err(self.unexpected("Tuple", value)),
        }
    }

    fn literal(&mut self, value: Value) -> Result<Literal, ImportError> {
        match value {
            Value::Nil => Ok(Literal::Nil),
            Value::Bool(true) => Ok(Literal::True),
            Value::Bool(false) => Ok(Literal::False),
            Value::StopIteration => Ok(Literal::StopIteration),
            Value::String(string) => Ok(Literal::String(string.to_value(self.heap).clone())),
            Value::Number(Number::Float(number)) => Ok(Literal::Float(number)),
            Value::Number(Number::Integer(GenericInt::Small(number))) => Ok(Literal::Int(number)),
            Value::Number(Number::Integer(GenericInt::Big(number))) => {
                Ok(Literal::BigInt(number.to_value(self.heap).clone()))
            }
            Value::Number(Number::Rational(number)) => Ok(Literal::Rational {
                numerator: number.numerator().to_bigint(self.heap),
                denominator: number.denominator().to_bigint(self.heap),
            }),
            Value::Instance(instance) => {
                let instance = instance.to_value(self.heap);
                let Some(NativeClass::Tuple(tuple)) = &instance.backing else {
                    return Err(self.unexpected("literal", value));
                };
                let mut items = Vec::with_capacity(tuple.items().len());
                for item in tuple.items() {
                    items.push(<Literal as FromAst>::from_ast(self, Some(*item))?);
                }
                Ok(Literal::Tuple(items))
            }
            _ => Err(self.unexpected("literal", value)),
        }
    }
}

impl FromAst for Literal {
    fn from_ast(importer: &mut Importer<'_>, value: Option<Value>) -> Result<Self, ImportError> {
        importer.enter()?;
        let result = (|| -> Result<Self, ImportError> {
            let value = importer.required(value, "literal")?;
            importer.literal(value)
        })();
        importer.leave();
        result
    }
}

impl FromAst for Program {
    fn from_ast(importer: &mut Importer<'_>, value: Option<Value>) -> Result<Self, ImportError> {
        importer.enter()?;
        let result = (|| -> Result<Self, ImportError> {
            let value = importer.required(value, "Program")?;
            let instance = importer.instance(value, "Program")?;
            let declarations = <Vec<Stmt> as FromAst>::from_ast(
                importer,
                importer.field(instance, "declarations"),
            )?;
            Ok(Self {
                declarations,
                comments: CommentMap::default(),
            })
        })();
        importer.leave();
        result
    }
}

impl FromAst for String {
    fn from_ast(importer: &mut Importer<'_>, value: Option<Value>) -> Result<Self, ImportError> {
        let value = importer.required(value, "string")?;
        importer.string(value)
    }
}

impl FromAst for bool {
    fn from_ast(importer: &mut Importer<'_>, value: Option<Value>) -> Result<Self, ImportError> {
        let value = importer.required(value, "bool")?;
        importer.boolean(value)
    }
}

impl FromAst for Location {
    fn from_ast(_importer: &mut Importer<'_>, _value: Option<Value>) -> Result<Self, ImportError> {
        Ok(Self::default())
    }
}

impl<T: FromAst> FromAst for Box<T> {
    fn from_ast(importer: &mut Importer<'_>, value: Option<Value>) -> Result<Self, ImportError> {
        let value = importer.required(value, "value")?;
        Ok(Self::new(T::from_ast(importer, Some(value))?))
    }
}

impl<T: FromAst> FromAst for Option<T> {
    fn from_ast(importer: &mut Importer<'_>, value: Option<Value>) -> Result<Self, ImportError> {
        match value {
            None | Some(Value::Nil) => Ok(None),
            Some(value) => Ok(Some(T::from_ast(importer, Some(value))?)),
        }
    }
}

impl<T: FromAst> FromAst for Vec<T> {
    fn from_ast(importer: &mut Importer<'_>, value: Option<Value>) -> Result<Self, ImportError> {
        let Some(value) = value else {
            return Ok(Self::new());
        };
        if matches!(value, Value::Nil) {
            return Ok(Self::new());
        }
        let mut items = Self::new();
        for item in importer.list_items(value)? {
            items.push(T::from_ast(importer, Some(item))?);
        }
        Ok(items)
    }
}

impl<A: FromAst, B: FromAst> FromAst for (A, B) {
    fn from_ast(importer: &mut Importer<'_>, value: Option<Value>) -> Result<Self, ImportError> {
        let value = importer.required(value, "tuple")?;
        let items = importer.tuple_items(value)?;
        let [first, second] = items.as_slice() else {
            return Err(importer.unexpected("2-element tuple", value));
        };
        Ok((
            A::from_ast(importer, Some(*first))?,
            B::from_ast(importer, Some(*second))?,
        ))
    }
}

macro_rules! impl_enum_from_ast {
    ($($ty:ty),* $(,)?) => {
        $(
            impl FromAst for $ty {
                fn from_ast(
                    importer: &mut Importer<'_>,
                    value: Option<Value>,
                ) -> Result<Self, ImportError> {
                    let value = importer.required(value, stringify!($ty))?;
                    let name = importer.enum_name(value)?;
                    <$ty as std::str::FromStr>::from_str(&name).map_err(|_| {
                        ImportError::Type(format!(
                            "invalid {} value: {name}",
                            stringify!($ty),
                        ))
                    })
                }
            }
        )*
    };
}

impl_enum_from_ast!(
    Mutability,
    ConditionType,
    LoopType,
    FunctionKind,
    MethodKind,
    AccessMode,
    ExprContext,
    UnaryOp,
    BoolOperator,
    BinaryOp,
);

fn import_program(program: &Program) -> Result<Program, ImportError> {
    let mut vm = VM::new(RunOptions::default());
    let value = program.to_ast(&mut Converter::new(&mut vm));
    Program::from_ast(&mut Importer::new(&vm.heap), Some(value))
}

fn import_expression(expression: &Expr) -> Result<Expr, ImportError> {
    let mut vm = VM::new(RunOptions::default());
    let value = expression.to_ast(&mut Converter::new(&mut vm));
    Expr::from_ast(&mut Importer::new(&vm.heap), Some(value))
}

fn check_program_ast(source: &str, optimize: bool) {
    let Ok(mut program) = Parser::parse_source(source) else {
        return;
    };
    if optimize {
        program.optimize();
    }
    let imported = match import_program(&program) {
        Ok(imported) => imported,
        Err(ImportError::Recursion) => return,
        Err(error) => panic!("AST import failed: {error:?}"),
    };
    assert!(
        program.loc_eq(&imported),
        "program AST changed after export/import:\nsource: {source:?}"
    );
}

fn check_expression_ast(source: &str, optimize: bool) {
    let Ok(mut expression) = Parser::parse_expression_source(source) else {
        return;
    };
    if optimize {
        expression.optimize();
    }
    let imported = match import_expression(&expression) {
        Ok(imported) => imported,
        Err(ImportError::Recursion) => return,
        Err(error) => panic!("AST import failed: {error:?}"),
    };
    assert!(
        expression.loc_eq(&imported),
        "expression AST changed after export/import:\nsource: {source:?}"
    );
}

fn check_program_unparse(source: &str) {
    let Ok(program) = Parser::parse_source(source) else {
        return;
    };
    let imported = match import_program(&program) {
        Ok(imported) => imported,
        Err(ImportError::Recursion) => return,
        Err(error) => panic!("AST import failed: {error:?}"),
    };
    let output = format_program_ast(&imported);
    let reparsed = Parser::parse_source(&output).unwrap_or_else(|errors| {
        panic!("unparsed program did not parse: {errors:?}\nsource: {source:?}\noutput: {output:?}")
    });
    assert!(
        program.loc_eq(&reparsed),
        "program AST changed after unparse round trip:\nsource: {source:?}\noutput: {output:?}"
    );
}

fn check_expression_unparse(source: &str) {
    let Ok(expression) = Parser::parse_expression_source(source) else {
        return;
    };
    let imported = match import_expression(&expression) {
        Ok(imported) => imported,
        Err(ImportError::Recursion) => return,
        Err(error) => panic!("AST import failed: {error:?}"),
    };
    let output = format_expression_ast(&imported);
    let reparsed = Parser::parse_expression_source(&output).unwrap_or_else(|errors| {
        panic!(
            "unparsed expression did not parse: {errors:?}\nsource: {source:?}\noutput: {output:?}"
        )
    });
    assert!(
        expression.loc_eq(&reparsed),
        "expression AST changed after unparse round trip:\nsource: {source:?}\noutput: {output:?}"
    );
}

/// Check the internal AST -> exported AST -> internal AST round trip.
pub fn check_ast_roundtrip(source: &str, optimize: bool) {
    check_program_ast(source, optimize);
    check_expression_ast(source, optimize);
}

/// Check the full parse -> export -> import -> format -> reparse round trip.
pub fn check_unparse_roundtrip(source: &str) {
    check_program_unparse(source);
    check_expression_unparse(source);
}

#[cfg(test)]
mod tests {
    use super::*;

    const SOURCES: &[&str] = &[
        "",
        "var x = 1; # comment",
        "class C < B { var x = 1; m(a) { return a; } }",
        "try { x; } catch (A) as e { y; } else { z; }",
        "switch (x) { case nil: y; default: z; }",
        r#"var t = t"a ${1 + 2}b";"#,
        "fun f() { return nil; }",
        "fun f(a, b = 1, c => a + b) { return c; }",
    ];

    #[test]
    fn round_trips() {
        for source in SOURCES {
            for optimize in [false, true] {
                check_ast_roundtrip(source, optimize);
            }
            check_unparse_roundtrip(source);
        }
    }
}
