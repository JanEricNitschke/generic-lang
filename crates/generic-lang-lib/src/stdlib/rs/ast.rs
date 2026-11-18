//! The `ast` stdlib module: parse Generic source and inspect the resulting
//! syntax tree without executing it.
//!
//! The exported schema mirrors the compiler's internal AST as closely as
//! possible: one class per struct and one class per enum variant. Wrappers
//! that only add a source location are flattened into the variant class.

use crate::ast_export::{Converter, ToAst, ast_class_names};
use crate::ast_import::{FromAst, ImportError, Importer};
use crate::compiler::ast::{Expr, Program, Stmt};
use crate::compiler::{Parser, format_expression_ast, format_program_ast, format_statement_ast};
use crate::config::AST_DUMP_MAX_DEPTH;
use crate::heap::{ClassId, InstanceId};
use crate::value::{ModuleContents, ModuleExport, NativeClass, Value, value_isinstance};
use crate::vm::ExceptionKind::{RecursionError, SyntaxError, TypeError};
use crate::vm::VM;
use crate::vm::errors::VmResult;

const BASE_CLASS_NAMES: &[&str] = &["Node", "Location"];

fn string_argument(vm: &mut VM, value: Value, function: &str) -> VmResult<String> {
    let Value::String(string_id) = value else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'{function}' expects a string, got: {}",
                    value.to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    Ok(string_id.to_value(&vm.heap).clone())
}

fn bool_argument(vm: &mut VM, value: Value, function: &str) -> VmResult<bool> {
    let Value::Bool(boolean) = value else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'{function}' expects a bool as its second argument, got: {}",
                    value.to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    Ok(boolean)
}

fn parse_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let source = string_argument(vm, args[0], "parse")?;
    let optimize = match args.get(1) {
        Some(value) => bool_argument(vm, *value, "parse")?,
        None => false,
    };
    let mut program = match Parser::parse_source(&source) {
        Ok(program) => program,
        Err(errors) => return Err(vm.throw(SyntaxError, &errors.join("\n")).unwrap_err()),
    };
    if optimize {
        program.optimize();
    }
    let mut converter = Converter::new(vm);
    Ok(program.to_ast(&mut converter))
}

fn parse_expression_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let source = string_argument(vm, args[0], "parse_expression")?;
    let optimize = match args.get(1) {
        Some(value) => bool_argument(vm, *value, "parse_expression")?,
        None => false,
    };
    let mut expression = match Parser::parse_expression_source(&source) {
        Ok(expression) => expression,
        Err(errors) => return Err(vm.throw(SyntaxError, &errors.join("\n")).unwrap_err()),
    };
    if optimize {
        expression.optimize();
    }
    let mut converter = Converter::new(vm);
    Ok(expression.to_ast(&mut converter))
}

fn dump_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    if !value_isinstance(&vm.heap, args[0], class_id(vm, "Node")) {
        let class_name = match args[0] {
            Value::Instance(instance_id) => instance_id
                .to_value(&vm.heap)
                .class
                .to_value(&vm.heap)
                .name
                .to_value(&vm.heap)
                .clone(),
            value => value.to_string(&vm.heap),
        };
        return Err(vm
            .throw(TypeError, &format!("expected AST, got: {class_name}"))
            .unwrap_err());
    }
    let mut out = String::new();
    Dumper.dump(vm, args[0], &mut out, 0)?;
    Ok(vm.heap.string_id(&out).into())
}

fn unparse_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let value = args[0];
    let Some(class) = Importer::new(&vm.heap).class_name(value) else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "expected Program, Stmt, or Expr, got: {}",
                    value.to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };

    let result = {
        let mut importer = Importer::new(&vm.heap);
        match class.as_str() {
            "Program" => <Program as FromAst>::from_ast(&mut importer, Some(value))
                .map(|program| format_program_ast(&program)),
            "Stmt" => <Stmt as FromAst>::from_ast(&mut importer, Some(value))
                .map(|statement| format_statement_ast(&statement)),
            "Expr" => <Expr as FromAst>::from_ast(&mut importer, Some(value))
                .map(|expression| format_expression_ast(&expression)),
            _ => {
                return Err(vm
                    .throw(
                        TypeError,
                        &format!("expected Program, Stmt, or Expr, got: {class}"),
                    )
                    .unwrap_err());
            }
        }
    };

    match result {
        Ok(source) => {
            let source = source.strip_suffix('\n').unwrap_or(&source);
            Ok(vm.heap.string_id(&source).into())
        }
        Err(ImportError::Type(message)) => Err(vm.throw(TypeError, &message).unwrap_err()),
        Err(ImportError::Recursion) => Err(vm
            .throw(RecursionError, "maximum recursion depth exceeded")
            .unwrap_err()),
    }
}

struct Dumper;

#[derive(Clone, Copy)]
enum SequenceKind {
    List,
    Tuple,
}

impl SequenceKind {
    const fn delimiters(self) -> (char, char) {
        match self {
            Self::List => ('[', ']'),
            Self::Tuple => ('(', ')'),
        }
    }
}

impl Dumper {
    fn indent(out: &mut String, depth: usize) {
        out.push_str(&"  ".repeat(depth));
    }

    fn quote(out: &mut String, value: &str) {
        out.push_str(&serde_json::to_string(value).expect("strings serialize"));
    }

    fn dump(&mut self, vm: &mut VM, value: Value, out: &mut String, depth: usize) -> VmResult<()> {
        if depth > AST_DUMP_MAX_DEPTH {
            return Err(vm
                .throw(RecursionError, "maximum recursion depth exceeded")
                .unwrap_err());
        }
        match value {
            Value::String(string_id) => {
                let string = string_id.to_value(&vm.heap);
                Self::quote(out, string);
            }
            Value::Instance(instance_id) => return self.dump_instance(vm, instance_id, out, depth),
            other => out.push_str(&other.to_string(&vm.heap)),
        }
        Ok(())
    }

    fn dump_instance(
        &mut self,
        vm: &mut VM,
        instance_id: InstanceId,
        out: &mut String,
        depth: usize,
    ) -> VmResult<()> {
        let instance = instance_id.to_value(&vm.heap).clone();
        match &instance.backing {
            Some(NativeClass::List(list)) => {
                self.dump_sequence(vm, &list.items, SequenceKind::List, out, depth)
            }
            Some(NativeClass::Tuple(tuple)) => {
                self.dump_sequence(vm, tuple.items(), SequenceKind::Tuple, out, depth)
            }
            _ if value_isinstance(&vm.heap, Value::Instance(instance_id), class_id(vm, "Node"))
                || instance.class == class_id(vm, "Location") =>
            {
                let class_name = instance
                    .class
                    .to_value(&vm.heap)
                    .name
                    .to_value(&vm.heap)
                    .clone();
                let mut fields = instance.fields.iter().collect::<Vec<_>>();
                fields.sort_by(|left, right| left.0.cmp(right.0));
                self.dump_class(vm, &class_name, &fields, out, depth)
            }
            _ => {
                let value = Value::Instance(instance_id);
                let rendered = vm.value_to_string(&value)?;
                out.push_str(rendered.to_value(&vm.heap));
                Ok(())
            }
        }
    }

    fn dump_sequence(
        &mut self,
        vm: &mut VM,
        items: &[Value],
        kind: SequenceKind,
        out: &mut String,
        depth: usize,
    ) -> VmResult<()> {
        let (open, close) = kind.delimiters();
        out.push(open);
        if items.is_empty() {
            out.push(close);
            return Ok(());
        }
        vm.for_each_rooted_with(
            items.iter().enumerate().map(|(index, item)| (*item, index)),
            |vm, item, index| {
                if index > 0 {
                    out.push(',');
                }
                out.push('\n');
                Self::indent(out, depth + 1);
                self.dump(vm, item, out, depth + 1)?;
                Ok(None)
            },
        )?;
        if matches!(kind, SequenceKind::Tuple) && items.len() == 1 {
            out.push(',');
        }
        out.push(close);
        Ok(())
    }

    fn dump_class(
        &mut self,
        vm: &mut VM,
        class_name: &str,
        fields: &[(&String, &Value)],
        out: &mut String,
        depth: usize,
    ) -> VmResult<()> {
        out.push_str(class_name);
        out.push('(');
        if fields.is_empty() {
            out.push(')');
            return Ok(());
        }
        let mut first = true;
        vm.for_each_rooted_with(
            fields.iter().map(|(key, value)| (**value, *key)),
            |vm, value, key| {
                if !first {
                    out.push(',');
                }
                first = false;
                out.push('\n');
                Self::indent(out, depth + 1);
                out.push_str(key);
                out.push('=');
                self.dump(vm, value, out, depth + 1)?;
                Ok(None)
            },
        )?;
        out.push(')');
        Ok(())
    }
}

fn class_id(vm: &VM, name: &str) -> ClassId {
    *vm.heap
        .native_classes
        .get(name)
        .unwrap_or_else(|| panic!("missing AST class `{name}`"))
}

fn define_classes(vm: &mut VM) {
    for name in BASE_CLASS_NAMES {
        vm.define_plain_class(name, false);
    }
    let node = class_id(vm, "Node");
    for name in ast_class_names() {
        vm.define_plain_class(&name, false);
        let id = class_id(vm, name);
        id.to_value_mut(&mut vm.heap).superclass = Some(node);
    }
}

pub(super) fn register(vm: &mut VM) {
    define_classes(vm);
    vm.register_stdlib_module(&"ast", module());
}

fn module() -> ModuleContents {
    let mut exports = BASE_CLASS_NAMES
        .iter()
        .copied()
        .map(|name| ModuleExport::Class { name })
        .collect::<Vec<_>>();
    exports.extend(
        ast_class_names()
            .into_iter()
            .map(|name| ModuleExport::Class { name }),
    );
    exports.extend([
        ModuleExport::Function {
            name: "parse",
            arity: &[1, 2],
            fun: parse_native,
        },
        ModuleExport::Function {
            name: "parse_expression",
            arity: &[1, 2],
            fun: parse_expression_native,
        },
        ModuleExport::Function {
            name: "dump",
            arity: &[1],
            fun: dump_native,
        },
        ModuleExport::Function {
            name: "unparse",
            arity: &[1],
            fun: unparse_native,
        },
    ]);
    exports
}
