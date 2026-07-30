//! The `json` stdlib module: `loads` (parse a JSON string into generic
//! values) and `dumps` (serialize generic values to a JSON string).
//!
//! Parsing goes through `serde_json`; serialization walks the value
//! tree by hand so integers of any size (JSON numbers are arbitrary
//! precision) and an optional indent are supported without feature
//! flags. Only string escaping is delegated to `serde_json`.

use crate::config::JSON_MAX_DEPTH;
use crate::value::{Dict, GenericInt, ModuleContents, ModuleExport, NativeClass, Number, Value};
use crate::vm::ExceptionKind::{TypeError, ValueError};
use crate::vm::VM;
use crate::vm::errors::VmResult;
use rustc_hash::FxHashSet as HashSet;

use crate::heap::InstanceId;

/// A parsed `serde_json::Value` as a generic value. Runs inside one
/// native call, so allocation never collects and no rooting is needed.
fn to_generic(vm: &mut VM, json: &serde_json::Value) -> VmResult<Value> {
    Ok(match json {
        serde_json::Value::Null => Value::Nil,
        serde_json::Value::Bool(boolean) => Value::Bool(*boolean),
        serde_json::Value::Number(number) => {
            if let Some(integer) = number.as_i64() {
                integer.into()
            } else if let Some(unsigned) = number.as_u64() {
                vm.heap.add_big_int(num_bigint::BigInt::from(unsigned))
            } else {
                // Beyond i64/u64. `arbitrary_precision` keeps the original
                // literal: a plain integer literal is an exact big int,
                // and anything with a fraction dot or exponent is a float
                // (the universal JSON convention), overflowing to infinity
                // rather than failing.
                // serde already validated the literal, so both parses
                // succeed; an out-of-range float parses to infinity, not
                // an error, so no value ever becomes NaN here.
                let literal = number.to_string();
                if literal.contains(['.', 'e', 'E']) {
                    literal
                        .parse::<f64>()
                        .expect("serde validated the float literal")
                        .into()
                } else {
                    let big = literal
                        .parse::<num_bigint::BigInt>()
                        .expect("serde validated the integer literal");
                    vm.heap.add_big_int(big)
                }
            }
        }
        serde_json::Value::String(string) => vm.heap.string_id(string).into(),
        serde_json::Value::Array(items) => {
            let mut list_items = Vec::with_capacity(items.len());
            for item in items {
                list_items.push(to_generic(vm, item)?);
            }
            vm.new_list(list_items)
        }
        serde_json::Value::Object(entries) => {
            let dict = vm.new_dict();
            for (key, value) in entries {
                let key = vm.heap.string_id(key).into();
                let value = to_generic(vm, value)?;
                Dict::add(vm, &dict, key, value)?;
            }
            dict
        }
    })
}

/// Parse a JSON document; the error carries `what` as the source name.
/// Shared by `loads` and `Response.json()`.
pub(super) fn parse_json_source(vm: &mut VM, source: &str) -> VmResult<Value> {
    match serde_json::from_str::<serde_json::Value>(source) {
        Ok(json) => to_generic(vm, &json),
        Err(error) => Err(vm
            .throw(ValueError, &format!("Invalid JSON: {error}"))
            .unwrap_err()),
    }
}

/// `loads(source)` - parse a JSON document.
fn loads_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let Value::String(source_id) = args[0] else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'loads' expects a string, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let source = source_id.to_value(&vm.heap).clone();
    parse_json_source(vm, &source)
}

/// Serialization state: the optional indent and the instances on the
/// current path (for cycle detection).
struct Dumper {
    indent: Option<usize>,
    in_progress: HashSet<InstanceId>,
}

impl Dumper {
    fn newline(&self, out: &mut String, depth: usize) {
        if let Some(indent) = self.indent {
            out.push('\n');
            out.push_str(&" ".repeat(indent * depth));
        }
    }

    /// Serialize `value` into `out` at nesting `depth`.
    fn dump(&mut self, vm: &mut VM, value: Value, out: &mut String, depth: usize) -> VmResult {
        match value {
            Value::Nil => out.push_str("null"),
            Value::Bool(boolean) => out.push_str(if boolean { "true" } else { "false" }),
            Value::Number(Number::Integer(integer)) => {
                out.push_str(&integer.to_string(&vm.heap));
            }
            Value::Number(Number::Float(float)) => {
                if !float.is_finite() {
                    return Self::type_error(vm, "a non-finite float");
                }
                out.push_str(&serde_json::to_string(&float).expect("finite floats serialize"));
            }
            Value::String(string_id) => {
                let string = string_id.to_value(&vm.heap);
                out.push_str(&serde_json::to_string(string).expect("strings serialize"));
            }
            Value::Instance(instance_id) => {
                return self.dump_instance(vm, instance_id, out, depth);
            }
            other => {
                let rendered = other.to_string(&vm.heap);
                return Self::type_error(vm, &format!("`{rendered}`"));
            }
        }
        Ok(None)
    }

    fn dump_instance(
        &mut self,
        vm: &mut VM,
        instance_id: InstanceId,
        out: &mut String,
        depth: usize,
    ) -> VmResult {
        if depth > JSON_MAX_DEPTH {
            return Err(vm
                .throw(ValueError, "Nesting is too deep to be serialized.")
                .unwrap_err());
        }
        if !self.in_progress.insert(instance_id) {
            return Err(vm
                .throw(ValueError, "Circular reference cannot be serialized.")
                .unwrap_err());
        }
        let result = match &instance_id.to_value(&vm.heap).backing {
            Some(NativeClass::List(list)) => {
                let items = list.items.clone();
                self.dump_sequence(vm, &items, out, depth)
            }
            Some(NativeClass::Tuple(tuple)) => {
                let items = tuple.items().to_vec();
                self.dump_sequence(vm, &items, out, depth)
            }
            Some(NativeClass::Dict(dict)) => {
                let entries: Vec<(Value, Value)> = dict
                    .items
                    .iter()
                    .map(|(key, value, _)| (*key, *value))
                    .collect();
                self.dump_object(vm, &entries, out, depth)
            }
            _ => {
                let rendered = instance_id.to_value(&vm.heap).to_string(&vm.heap);
                Self::type_error(vm, &format!("`{rendered}`"))
            }
        };
        self.in_progress.remove(&instance_id);
        result
    }

    fn dump_sequence(
        &mut self,
        vm: &mut VM,
        items: &[Value],
        out: &mut String,
        depth: usize,
    ) -> VmResult {
        out.push('[');
        for (index, item) in items.iter().enumerate() {
            if index > 0 {
                out.push(',');
                if self.indent.is_none() {
                    out.push(' ');
                }
            }
            self.newline(out, depth + 1);
            self.dump(vm, *item, out, depth + 1)?;
        }
        if !items.is_empty() {
            self.newline(out, depth);
        }
        out.push(']');
        Ok(None)
    }

    fn dump_object(
        &mut self,
        vm: &mut VM,
        entries: &[(Value, Value)],
        out: &mut String,
        depth: usize,
    ) -> VmResult {
        out.push('{');
        for (index, (key, value)) in entries.iter().enumerate() {
            if index > 0 {
                out.push(',');
                if self.indent.is_none() {
                    out.push(' ');
                }
            }
            self.newline(out, depth + 1);
            let Value::String(key_id) = key else {
                let rendered = key.to_string(&vm.heap);
                return Err(vm
                    .throw(
                        TypeError,
                        &format!("JSON object keys must be strings, got: `{rendered}`"),
                    )
                    .unwrap_err());
            };
            let key_string = key_id.to_value(&vm.heap);
            out.push_str(&serde_json::to_string(key_string).expect("strings serialize"));
            out.push_str(": ");
            self.dump(vm, *value, out, depth + 1)?;
        }
        if !entries.is_empty() {
            self.newline(out, depth);
        }
        out.push('}');
        Ok(None)
    }

    fn type_error(vm: &mut VM, what: &str) -> VmResult {
        Err(vm
            .throw(TypeError, &format!("{what} is not JSON serializable."))
            .unwrap_err())
    }
}

/// Upper bound on the per-level indent width, keeping `indent * depth` well
/// within `usize` and the emitted padding to a sane size.
const MAX_INDENT: usize = 128;

/// `dumps(value)` / `dumps(value, indent)` - serialize to JSON. Lists
/// and tuples become arrays, dicts (string keys only) become objects.
fn dumps_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let indent = match args.get(1) {
        None => None,
        Some(&Value::Number(Number::Integer(GenericInt::Small(small)))) => {
            match usize::try_from(small) {
                Ok(indent) if indent <= MAX_INDENT => Some(indent),
                Ok(_) => {
                    return Err(vm
                        .throw(TypeError, "'dumps' indent is unreasonably large.")
                        .unwrap_err());
                }
                Err(_) => {
                    return Err(vm
                        .throw(TypeError, "'dumps' indent must not be negative.")
                        .unwrap_err());
                }
            }
        }
        Some(&Value::Number(Number::Integer(GenericInt::Big(_)))) => {
            return Err(vm
                .throw(TypeError, "'dumps' indent is unreasonably large.")
                .unwrap_err());
        }
        Some(x) => {
            return Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "'dumps' expects an integer indent, got: {}",
                        x.to_string(&vm.heap)
                    ),
                )
                .unwrap_err());
        }
    };
    let mut dumper = Dumper {
        indent,
        in_progress: HashSet::default(),
    };
    let mut out = String::new();
    dumper.dump(vm, args[0], &mut out, 0)?;
    Ok(vm.heap.string_id(&out).into())
}

pub(super) fn register(vm: &mut VM) {
    vm.register_stdlib_module(&"json", module());
}

/// Export all the contents of the module with the
/// name they are to be accessed with from generic; functions
/// additionally carry their supported arities.
fn module() -> ModuleContents {
    vec![
        ModuleExport::Function {
            name: "loads",
            arity: &[1],
            fun: loads_native,
        },
        ModuleExport::Function {
            name: "dumps",
            arity: &[1, 2],
            fun: dumps_native,
        },
    ]
}
