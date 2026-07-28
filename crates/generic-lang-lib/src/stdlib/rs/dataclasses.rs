//! The `dataclasses` stdlib module: the `dataclass` class decorator and
//! the `field` default-factory marker.
//!
//! `dataclass` installs native `__init__`, `__str__`, `__eq__`, and
//! `__hash__` methods on the decorated class (methods the class already
//! has, own or inherited, win). The field list is frozen at decoration
//! time into the `__dataclass_fields__` class variable (the class's
//! variables in declaration order, minus `_`-prefixed names, appended
//! to the fields of a dataclass base); like any class variable it is
//! copied down at inheritance, so the methods of a subclass see the
//! frozen list, not later additions. All four methods read it from the
//! receiver's class at call time.

use crate::heap::{ClassId, Heap, InstanceId, StringId};
use crate::natives::VARIADIC_0_PLUS;
use crate::value::{
    Field, Instance, ModuleContents, ModuleExport, NativeClass, NativeMethod, NativeMethodImpl,
    Tuple, Value,
};
use crate::vm::ExceptionKind::{AttributeError, TypeError};
use crate::vm::VM;
use crate::vm::errors::{VmErrorKind, VmResult};
use rustc_hash::FxHasher;
use std::hash::Hasher;

/// Name of the class variable holding the frozen field list.
const FIELDS_VARIABLE: &str = "__dataclass_fields__";

/// Decode a `__dataclass_fields__` snapshot into its field names. The
/// variable is an ordinary class variable, so user code can overwrite it
/// with anything; any shape other than a tuple of strings yields `None`.
fn decode_fields(heap: &Heap, snapshot: Value) -> Option<Vec<StringId>> {
    let Value::Instance(instance_id) = snapshot else {
        return None;
    };
    let Some(NativeClass::Tuple(tuple)) = &instance_id.to_value(heap).backing else {
        return None;
    };
    tuple
        .items()
        .iter()
        .map(|item| match item {
            Value::String(string_id) => Some(*string_id),
            _ => None,
        })
        .collect()
}

/// The `TypeError` for a tampered `__dataclass_fields__`.
fn tampered_fields_error(vm: &mut VM, class_id: ClassId) -> VmErrorKind {
    let name = class_id.to_value(&vm.heap).name.to_value(&vm.heap).clone();
    vm.throw(
        TypeError,
        &format!("'__dataclass_fields__' of '{name}' must be a tuple of field-name strings."),
    )
    .unwrap_err()
}

/// Install the generated methods on a user-defined class and return it.
fn dataclass_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let Value::Class(class_id) = args[0] else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'dataclass' expected a class, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    if class_id.to_value(&vm.heap).is_native() {
        return Err(vm
            .throw(TypeError, "'dataclass' expected a user-defined class.")
            .unwrap_err());
    }

    // Freeze the field list: the fields of a dataclass base (its
    // `__dataclass_fields__`, copied down at inheritance), extended by
    // this class's variables in order. `_`-prefixed names are never
    // fields; a redefined name keeps its inherited position.
    let fields_name = vm.heap.string_id(&FIELDS_VARIABLE);
    let mut field_names: Vec<StringId> = Vec::new();
    if let Some(snapshot) = class_id
        .to_value(&vm.heap)
        .class_variable_value(fields_name)
    {
        let Some(inherited) = decode_fields(&vm.heap, snapshot) else {
            return Err(tampered_fields_error(vm, class_id));
        };
        field_names.extend(inherited);
    }
    for (&name_id, _) in &class_id.to_value(&vm.heap).variables {
        if vm.heap.strings[name_id].starts_with('_') || field_names.contains(&name_id) {
            continue;
        }
        field_names.push(name_id);
    }

    // A field without a default after one with a default could never be
    // filled positionally; reject the order outright.
    let mut saw_default = false;
    for &name_id in &field_names {
        let has_default = class_id
            .to_value(&vm.heap)
            .variables
            .get(&name_id)
            .is_some_and(|variable| variable.default.is_some());
        if has_default {
            saw_default = true;
        } else if saw_default {
            let name = vm.heap.strings[name_id].clone();
            return Err(vm
                .throw(
                    TypeError,
                    &format!("Field `{name}` without a default follows a field with a default."),
                )
                .unwrap_err());
        }
    }

    let items: Vec<Value> = field_names.iter().map(|&name_id| name_id.into()).collect();
    let snapshot = vm.heap.add_instance(Instance::new(
        *vm.heap.native_classes.get("Tuple").unwrap(),
        Some(Tuple::new(items).into()),
    ));
    class_id
        .to_value_mut(&mut vm.heap)
        .set_class_variable_value(fields_name, snapshot);

    let class_name = class_id.to_value(&vm.heap).name;
    let methods: [(&str, &'static [u8], NativeMethodImpl); 4] = [
        ("__init__", &VARIADIC_0_PLUS, dataclass_init_native),
        ("__str__", &[0], dataclass_str_native),
        ("__eq__", &[1], dataclass_eq_native),
        ("__hash__", &[0], dataclass_hash_native),
    ];
    for (name, arity, fun) in methods {
        let name_id = vm.heap.string_id(&name);
        if class_id.to_value(&vm.heap).methods.contains_key(&name_id) {
            continue;
        }
        let method = vm.heap.add_native_method(NativeMethod {
            class: class_name,
            name: name_id,
            arity,
            fun,
            #[cfg(feature = "plugins")]
            plugin_fn: None,
        });
        class_id
            .to_value_mut(&mut vm.heap)
            .methods
            .insert(name_id, method);
    }
    Ok(args[0])
}

/// Wrap a callable as a per-instance default factory.
#[allow(clippy::unnecessary_wraps)]
fn field_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let field_class = *vm.heap.native_classes.get("Field").unwrap();
    let instance = Instance::new(field_class, Some(Field { factory: args[0] }.into()));
    Ok(vm.heap.add_instance(instance))
}

/// Read a dataclass field from an instance, raising `AttributeError`
/// when it is absent (a user-defined `__init__` that skipped it, or a
/// `delattr`).
fn read_field(vm: &mut VM, instance_id: InstanceId, name: &str) -> VmResult<Value> {
    match instance_id.to_value(&vm.heap).fields.get(name).copied() {
        Some(value) => Ok(value),
        None => Err(vm
            .throw(AttributeError, &format!("Undefined property '{name}'."))
            .unwrap_err()),
    }
}

/// The frozen field list of `class_id`, captured once per call; the
/// per-field defaults are still read live when needed.
fn dataclass_fields(vm: &mut VM, class_id: ClassId) -> VmResult<Vec<StringId>> {
    let fields_name = vm.heap.string_id(&FIELDS_VARIABLE);
    let Some(snapshot) = class_id
        .to_value(&vm.heap)
        .class_variable_value(fields_name)
    else {
        let name = class_id.to_value(&vm.heap).name.to_value(&vm.heap).clone();
        return Err(vm
            .throw(TypeError, &format!("'{name}' is not a dataclass."))
            .unwrap_err());
    };
    decode_fields(&vm.heap, snapshot).map_or_else(|| Err(tampered_fields_error(vm, class_id)), Ok)
}

/// A `field(factory)` default builds a fresh value per instance; any
/// other default is shared as-is.
fn resolve_default(vm: &mut VM, default: Value) -> VmResult<Value> {
    let factory = match default {
        Value::Instance(instance_id) => match &instance_id.to_value(&vm.heap).backing {
            Some(NativeClass::Field(field)) => Some(field.factory),
            _ => None,
        },
        _ => None,
    };
    let Some(factory) = factory else {
        return Ok(default);
    };
    vm.stack.push(factory);
    vm.call_value_and_run(0)?;
    Ok(vm.stack.pop().expect("call left no result on the stack"))
}

/// Fill the instance's fields from the positional arguments (in class
/// variable declaration order), falling back to each field's default.
fn dataclass_init_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let instance_id = *receiver.as_instance();
    let class_id = instance_id.to_value(&vm.heap).class;
    let field_names = dataclass_fields(vm, class_id)?;
    if args.len() > field_names.len() {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'{}' takes at most {} arguments, got {}.",
                    class_id.to_value(&vm.heap).name.to_value(&vm.heap),
                    field_names.len(),
                    args.len()
                ),
            )
            .unwrap_err());
    }
    for (index, &name_id) in field_names.iter().enumerate() {
        // The default is re-fetched per step: filling one may re-enter
        // the interpreter.
        let default = class_id
            .to_value(&vm.heap)
            .variables
            .get(&name_id)
            .and_then(|variable| variable.default);
        let value = if index < args.len() {
            args[index]
        } else if let Some(default) = default {
            resolve_default(vm, default)?
        } else {
            let name = vm.heap.strings[name_id].clone();
            return Err(vm
                .throw(
                    TypeError,
                    &format!("Missing argument for field `{name}` without a default."),
                )
                .unwrap_err());
        };
        let name = vm.heap.strings[name_id].clone();
        instance_id
            .to_value_mut(&mut vm.heap)
            .fields
            .insert(name, value);
    }
    Ok(*receiver)
}

/// `ClassName(field=value, ...)` in field declaration order.
fn dataclass_str_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let instance_id = *receiver.as_instance();
    let class_id = instance_id.to_value(&vm.heap).class;
    let class_name = class_id.to_value(&vm.heap).name.to_value(&vm.heap).clone();
    let mut parts = Vec::new();
    for &name_id in &dataclass_fields(vm, class_id)? {
        let name = vm.heap.strings[name_id].clone();
        let value = read_field(vm, instance_id, &name)?;
        let rendered = vm.value_to_string(&value)?;
        parts.push(format!("{name}={}", rendered.to_value(&vm.heap)));
    }
    let result = format!("{class_name}({})", parts.join(", "));
    Ok(vm.heap.string_id(&result).into())
}

/// Instances of the same class are equal iff all fields are equal.
fn dataclass_eq_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let this_id = *receiver.as_instance();
    let Value::Instance(other_id) = args[0] else {
        return Ok(false.into());
    };
    let class_id = this_id.to_value(&vm.heap).class;
    if other_id.to_value(&vm.heap).class != class_id {
        return Ok(false.into());
    }
    for &name_id in &dataclass_fields(vm, class_id)? {
        let name = vm.heap.strings[name_id].clone();
        let this_value = read_field(vm, this_id, &name)?;
        let other_value = read_field(vm, other_id, &name)?;
        if !vm.compare_values_eq(this_value, other_value)? {
            return Ok(false.into());
        }
    }
    Ok(true.into())
}

/// Combine the field hashes in declaration order.
fn dataclass_hash_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let instance_id = *receiver.as_instance();
    let class_id = instance_id.to_value(&vm.heap).class;
    let mut hasher = FxHasher::default();
    for &name_id in &dataclass_fields(vm, class_id)? {
        let name = vm.heap.strings[name_id].clone();
        let value = read_field(vm, instance_id, &name)?;
        hasher.write_u64(vm.compute_hash(value)?);
    }
    Ok(hasher.finish().cast_signed().into())
}

/// Define the `Field` marker class and register the `dataclasses`
/// module. The class is not user-reachable; `field` is the only way to
/// create instances.
/// `str(field_instance)`: the wrapped factory, rendered the way `str`
/// renders it (honoring `__str__`).
fn field_str_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let factory = receiver.as_field(&vm.heap).factory;
    let mut string = String::from("field(");
    string.push_str(vm.value_to_string(&factory)?.to_value(&vm.heap));
    string.push(')');
    Ok(vm.heap.string_id(&string).into())
}

pub(super) fn register(vm: &mut VM) {
    vm.define_native_class(&"Field", false);
    vm.define_native_method(&"Field", &"__str__", &[0], field_str_native);
    vm.register_stdlib_module(&"dataclasses", module());
}

/// Export all the contents of the module with the
/// name they are to be accessed with from generic; functions
/// additionally carry their supported arities.
fn module() -> ModuleContents {
    vec![
        ModuleExport::Function {
            name: "dataclass",
            arity: &[1],
            fun: dataclass_native,
        },
        ModuleExport::Function {
            name: "field",
            arity: &[1],
            fun: field_native,
        },
    ]
}
