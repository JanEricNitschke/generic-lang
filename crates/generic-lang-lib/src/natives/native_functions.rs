//! Module containing free standing rust native functions.

use crate::vm::ExceptionKind::{
    AssertionError, AttributeError, ConstReassignmentError, IoError, TypeError, ValueError,
};
use crate::{
    heap::Heap,
    types::InjectedKind,
    value::{
        BoundMethod, GenericInt, Module, NativeClass, Number, Value, class_of_value,
        is_subclass_of, value_isinstance,
    },
    vm::{Global, VM, errors::VmResult},
};
use num_traits::FromPrimitive;
use rand::RngExt;
use std::io;
use std::path::PathBuf;
use std::thread;
use std::time::Duration;
use std::time::{SystemTime, UNIX_EPOCH};

/// Get the time since the `UNIX_EPOCH` in seconds.
/// Useful for timing durations by calling this twice and subtracting the results.
pub fn clock_native(_vm: &mut VM, _args: &[Value]) -> VmResult<Value> {
    Ok(Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64()
            .into(),
    ))
}

/// Sleep for a non-negative number of seconds.
pub(super) fn sleep_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    match &args[0] {
        Value::Number(Number::Integer(i)) if i.ge_i64(0, &vm.heap) => {
            match i.try_to_u64(&vm.heap) {
                Ok(u) => thread::sleep(Duration::from_secs(u)),
                Err(_) => {
                    return Err(vm
                        .throw(
                            ValueError,
                            &format!(
                                "'sleep' argument too large: `{}`",
                                Value::from(*i).to_string(&vm.heap)
                            ),
                        )
                        .unwrap_err());
                }
            }
        }
        x => {
            return Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "'sleep' expected positive integer argument, got: `{}`",
                        x.to_string(&vm.heap)
                    ),
                )
                .unwrap_err());
        }
    }
    Ok(Value::Nil)
}

/// Error if the argument is falsey.
pub(super) fn assert_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let value = &args[0];
    if vm.is_falsey(*value)? {
        Err(vm
            .throw(
                AssertionError,
                &format!("Assertion on `{}` failed!", value.to_string(&vm.heap)),
            )
            .unwrap_err())
    } else {
        Ok(Value::Nil)
    }
}

// Could also make a zero arg version of this if a prompt is not desired..
/// Read input from the command line after providing a prompt.
pub(super) fn input_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    match &args[0] {
        Value::String(prompt) => {
            println!("{}", vm.heap.strings[*prompt]);
            let mut choice = String::new();
            match io::stdin().read_line(&mut choice) {
                Ok(_) => {
                    let string = Value::String(vm.heap.string_id(&choice.trim()));
                    Ok(string)
                }
                Err(e) => Err(vm
                    .throw(IoError, &format!("'input' could not read line: {e}"))
                    .unwrap_err()),
            }
        }
        x => Err(vm
            .throw(
                ValueError,
                &format!(
                    "'input' expected string argument, got: {}",
                    x.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

/// Turn a value into a float.
/// Works on numbers, bools or sensible strings.
#[allow(clippy::option_if_let_else)]
pub(super) fn to_float_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    match &args[0] {
        Value::String(string_id) => {
            let string = &vm.heap.strings[*string_id];
            let converted: Result<f64, _> = string.parse();
            match converted {
                Ok(result) => Ok(Value::Number(result.into())),
                Err(_) => Err(vm
                    .throw(
                        ValueError,
                        &format!("'float' could not convert string '{string}' to a float."),
                    )
                    .unwrap_err()),
            }
        }
        Value::Number(n) => Ok(Value::Number(n.to_f64(&vm.heap).into())),
        Value::Bool(value) => Ok(Value::Number(f64::from(*value).into())),
        x => Err(vm
            .throw(
                TypeError,
                &format!(
                    "'float' expected string, number or bool argument, got: {}",
                    x.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

/// Convert a value into an integer.
/// Works on numbers, bools or sensible strings.
#[allow(clippy::option_if_let_else)]
pub(super) fn to_int_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    match &args[0] {
        Value::String(string_id) => {
            let string = &vm.heap.strings[*string_id];
            let converted: Result<i64, _> = string.parse();
            match converted {
                Ok(result) => Ok(Value::Number(result.into())),
                Err(_) => Err(vm
                    .throw(
                        ValueError,
                        &format!("'int' could not convert string '{string}' to an integer."),
                    )
                    .unwrap_err()),
            }
        }
        Value::Number(n) => match n {
            Number::Float(f) => match GenericInt::try_from_f64(*f, &mut vm.heap) {
                Ok(i) => Ok(Value::Number(i.into())),
                Err(_) => Err(vm
                    .throw(
                        ValueError,
                        &format!("'int' could not convert float '{f}' to an integer."),
                    )
                    .unwrap_err()),
            },
            Number::Integer(_) => Ok(Value::Number(*n)),
            Number::Rational(rational) => match rational.to_int(&vm.heap) {
                Ok(i) => Ok(Value::Number(i.into())),
                Err(_) => Err(vm
                    .throw(
                        ValueError,
                        &format!("'int' could not convert rational '{rational:?}' to an integer."),
                    )
                    .unwrap_err()),
            },
        },
        Value::Bool(value) => Ok(Value::Number(i64::from(*value).into())),
        x => Err(vm
            .throw(
                TypeError,
                &format!(
                    "'int' expected string, number or bool argument, got: {}",
                    x.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

/// Check if the provided value can be turned into an integer.
#[allow(clippy::option_if_let_else)]
pub(super) fn is_int_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    match &args[0] {
        Value::String(string_id) => {
            let string = &vm.heap.strings[*string_id];
            let converted: Result<i64, _> = string.parse();
            match converted {
                Ok(_) => Ok(Value::Bool(true)),
                Err(_) => Ok(Value::Bool(false)),
            }
        }
        Value::Number(_) | Value::Bool(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

/// Turn the value into a string.
/// Fixed implementations for basic types, instances use the `__str__` method if present.
pub(super) fn to_string_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    Ok(Value::String(vm.value_to_string(&args[0])?))
}

/// Return the class of the value: instances carry their class, and the value
/// types map to their proxy classes (`Bool`, `String`, `Integer`, `Float`,
/// `Rational`). Raises a `TypeError` for values that have no class (nil,
/// functions, classes, modules, …); use `typename` for a string over any value.
pub(super) fn type_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    if let Some(class_id) = class_of_value(&vm.heap, args[0]) {
        Ok(class_id.into())
    } else {
        // `typename_native` always returns a string.
        let name = typename_native(vm, args)?;
        let message = format!(
            "type() is not defined for {}",
            name.as_string().to_value(&vm.heap)
        );
        Err(vm.throw(TypeError, &message).unwrap_err())
    }
}

/// Return the type of the value as a string (defined for every value).
pub(super) fn typename_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let string = match &args[0] {
        Value::Bool(_) => Value::String(vm.heap.string_id(&"<type bool>")),
        Value::BoundMethod(_) => Value::String(vm.heap.string_id(&"<type bound method>")),
        Value::Class(_) => Value::String(vm.heap.string_id(&"<type class>")),
        Value::Closure(_) => Value::String(vm.heap.string_id(&"<type closure>")),
        Value::Function(_) => Value::String(vm.heap.string_id(&"<type function>")),
        Value::Instance(instance) => Value::String(
            vm.heap.string_id(
                &("<type ".to_string()
                    + instance
                        .to_value(&vm.heap)
                        .class
                        .to_value(&vm.heap)
                        .name
                        .to_value(&vm.heap)
                    + ">"),
            ),
        ),
        Value::NativeFunction(_) => Value::String(vm.heap.string_id(&"<type native function>")),
        Value::NativeMethod(_) => Value::String(vm.heap.string_id(&"<type native method>")),
        Value::Nil => Value::String(vm.heap.string_id(&"<type nil>")),
        Value::StopIteration => Value::String(vm.heap.string_id(&"<type StopIteration>")),
        Value::Number(n) => match n {
            Number::Float(_) => Value::String(vm.heap.string_id(&"<type float>")),
            Number::Integer(_) => Value::String(vm.heap.string_id(&"<type int>")),
            Number::Rational(_) => Value::String(vm.heap.string_id(&"<type rational>")),
        },
        Value::String(_) => Value::String(vm.heap.string_id(&"<type string>")),
        Value::Upvalue(_) => Value::String(vm.heap.string_id(&"<type upvalue>")),
        Value::Module(_) => Value::String(vm.heap.string_id(&"<type module>")),
    };
    Ok(string)
}

/// Print the value to stdout.
///
/// Optionally supply a string to be printed at the end of the value.
/// Defaults to `\n`.
pub(super) fn print_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let end = if args.len() == 2 {
        match &args[1] {
            Value::String(string_id) => &string_id.to_value(&vm.heap).clone(),
            x => {
                return Err(vm
                    .throw(
                        TypeError,
                        &format!(
                            "Optional second argument to 'print' has to be a string, got: {}",
                            x.to_string(&vm.heap)
                        ),
                    )
                    .unwrap_err());
            }
        }
    } else {
        "\n"
    };

    // Use the shared value_to_string utility for consistent behavior
    let string_id = vm.value_to_string(&args[0])?;
    print!("{}{end}", string_id.to_value(&vm.heap));

    Ok(Value::Nil)
}

/// Return a random integer between the two arguments.
/// Lower value is inclusive, upper value is exclusive.
pub(super) fn rng_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    match (&args[0], &args[1]) {
        (Value::Number(Number::Integer(min)), Value::Number(Number::Integer(max))) => {
            match (min, max) {
                (GenericInt::Small(min), GenericInt::Small(max)) => {
                    Ok(Value::Number(rand::rng().random_range(*min..*max).into()))
                }
                _ => Err(vm.throw(ValueError, &format!(
                    "'rng' expected small integers (i64) as arguments, got: `{}` and `{}` instead.",
                    min.to_string(&vm.heap),
                    max.to_string(&vm.heap)
                )).unwrap_err()),
            }
        }
        (other_1, other_2) => Err(vm
            .throw(
                TypeError,
                &format!(
                    "'rng' expected two integers as arguments, got: `{}` and `{}` instead.",
                    other_1.to_string(&vm.heap),
                    other_2.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

/// Get an attribute from a value by name.
pub(super) fn getattr_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    match (&args[0], &args[1]) {
        (Value::Module(module_id), Value::String(string_id)) => {
            let maybe_value = module_id
                .to_value(&vm.heap)
                .globals
                .get(string_id)
                .map(|global| global.value);
            if let Some(value) = maybe_value {
                Ok(value)
            } else {
                let message = format!(
                    "Undefined name '{}' in module {}.",
                    string_id.to_value(&vm.heap).clone(),
                    module_id.to_value(&vm.heap).name.to_value(&vm.heap)
                );
                Err(vm.throw(AttributeError, &message).unwrap_err())
            }
        }
        (receiver @ Value::Instance(instance), Value::String(string_id)) => {
            // Mirrors property access: fields, then methods (bound to the
            // instance), then class variables.
            let field = &vm.heap.strings[*string_id];
            if let Some(value_id) = instance.to_value(&vm.heap).fields.get(field) {
                return Ok(*value_id);
            }
            let class = instance.to_value(&vm.heap).class;
            if let Some(method) = class.to_value(&vm.heap).methods.get(string_id).copied() {
                return Ok(vm.heap.add_bound_method(BoundMethod {
                    receiver: *receiver,
                    method,
                }));
            }
            match class.to_value(&vm.heap).class_variable_value(*string_id) {
                Some(value) => Ok(value),
                None => Err(vm
                    .throw(
                        AttributeError,
                        &format!("Undefined property '{}'.", vm.heap.strings[*string_id]),
                    )
                    .unwrap_err()),
            }
        }
        (Value::Class(class), Value::String(string_id)) => {
            match class.to_value(&vm.heap).class_variable_value(*string_id) {
                Some(value) => Ok(value),
                None => Err(vm
                    .throw(
                        AttributeError,
                        &format!("Undefined property '{}'.", vm.heap.strings[*string_id]),
                    )
                    .unwrap_err()),
            }
        }
        (instance @ Value::Instance(_), x) => Err(vm
            .throw(
                TypeError,
                &format!(
                    "`getattr` can only index with string indexes, got: `{}` (instance: `{}`)",
                    x.to_string(&vm.heap),
                    instance.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
        (not_instance, _) => Err(vm
            .throw(
                TypeError,
                &format!(
                    "`getattr` only works on instances, classes, and modules, got `{}`",
                    not_instance.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

/// Set an attribute of a value by name.
pub(super) fn setattr_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let field = if let Value::String(string_id) = args[1] {
        vm.heap.strings[string_id].clone()
    } else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "`setattr` can only index with string indexes, got: `{}` (instance: `{}`)",
                    args[1].to_string(&vm.heap),
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let value = args[2];
    if let Value::Instance(instance) = args[0] {
        instance
            .to_value_mut(&mut vm.heap)
            .fields
            .insert(field, value);
        Ok(Value::Nil)
    } else if let Value::Class(class) = args[0] {
        let name_id = vm.heap.string_id(&field);
        class
            .to_value_mut(&mut vm.heap)
            .set_class_variable_value(name_id, value);
        Ok(Value::Nil)
    } else if let Value::Module(module_id) = args[0] {
        let name_id = vm.heap.string_id(&field);
        if let Some(global) = module_id
            .to_value_mut(&mut vm.heap)
            .globals
            .get_mut(&name_id)
        {
            if !global.mutable {
                return Err(vm
                    .throw(ConstReassignmentError, "Cannot reassign const variable.")
                    .unwrap_err());
            }
            global.value = value;
        } else {
            module_id.to_value_mut(&mut vm.heap).globals.insert(
                name_id,
                Global {
                    value,
                    mutable: true,
                },
            );
        }
        Ok(Value::Nil)
    } else {
        Err(vm
            .throw(
                TypeError,
                &format!(
                    "`setattr` only works on instances, classes, and modules, got `{}`",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err())
    }
}

/// Check if the given attribute exists on an instance (field, method, or
/// class variable), a class (class variables only), or a module (globals).
pub(super) fn hasattr_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    match (&args[0], &args[1]) {
        (Value::Module(module_id), Value::String(string_id)) => Ok(Value::Bool(
            module_id.to_value(&vm.heap).globals.contains_key(string_id),
        )),
        (Value::Instance(instance), Value::String(string_id)) => Ok((instance
            .to_value(&vm.heap)
            .has_field_or_method(*string_id, &vm.heap)
            || instance
                .to_value(&vm.heap)
                .class
                .to_value(&vm.heap)
                .class_variable_value(*string_id)
                .is_some())
        .into()),
        (Value::Class(class), Value::String(string_id)) => Ok(class
            .to_value(&vm.heap)
            .class_variable_value(*string_id)
            .is_some()
            .into()),
        (instance @ Value::Instance(_), x) => Err(vm
            .throw(
                TypeError,
                &format!(
                    "`hasattr` can only index with string indexes, got: `{}` (instance: `{}`)",
                    x.to_string(&vm.heap),
                    instance.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
        (not_instance, _) => Err(vm
            .throw(
                TypeError,
                &format!(
                    "`hasattr` only works on instances, classes, and modules, got `{}`",
                    not_instance.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

/// Delete an attribute on an instance by name.
/// Does NOT work on methods. Errors if the attribute does not exist in the first place.
pub(super) fn delattr_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    if let Value::String(string_id) = args[1] {
        let field = &vm.heap.strings[string_id].clone();
        if let Value::Instance(instance) = args[0] {
            match instance.to_value_mut(&mut vm.heap).fields.remove(field) {
                Some(_) => Ok(Value::Nil),
                None => Err(vm
                    .throw(AttributeError, &format!("Undefined property '{field}'."))
                    .unwrap_err()),
            }
        } else if let Value::Class(class) = args[0] {
            match class
                .to_value_mut(&mut vm.heap)
                .remove_class_variable(string_id)
            {
                Some(_) => Ok(Value::Nil),
                None => Err(vm
                    .throw(AttributeError, &format!("Undefined property '{field}'."))
                    .unwrap_err()),
            }
        } else if let Value::Module(module_id) = args[0] {
            match module_id.to_value(&vm.heap).globals.get(&string_id) {
                Some(global) if !global.mutable => Err(vm
                    .throw(ConstReassignmentError, "Cannot delete const variable.")
                    .unwrap_err()),
                Some(_) => {
                    module_id
                        .to_value_mut(&mut vm.heap)
                        .globals
                        .remove(&string_id);
                    Ok(Value::Nil)
                }
                None => {
                    let message = format!(
                        "Undefined name '{field}' in module {}.",
                        module_id.to_value(&vm.heap).name.to_value(&vm.heap)
                    );
                    Err(vm.throw(AttributeError, &message).unwrap_err())
                }
            }
        } else {
            Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "`delattr` only works on instances, classes, and modules, got `{}`",
                        args[0].to_string(&vm.heap)
                    ),
                )
                .unwrap_err())
        }
    } else {
        Err(vm
            .throw(
                TypeError,
                &format!(
                    "`delattr` can only index with string indexes, got: `{}` (instance: `{}`)",
                    args[1].to_string(&vm.heap),
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err())
    }
}

/// Return the length of an instance.
#[allow(clippy::cast_possible_wrap)]
pub(super) fn len_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    vm.invoke_method_by_name_with_attribute_error(args[0], "__len__")
}

/// Get the next item from an iterator.
pub(super) fn next_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    vm.invoke_method_by_name_with_attribute_error(args[0], "__next__")
}

/// Get the iterator from an iterable.
pub(super) fn iter_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    vm.invoke_method_by_name_with_attribute_error(args[0], "__iter__")
}

/// Check if value is an instance of the given class or any of its subclasses.
/// Similar to Python's isinstance(value, classinfo).
pub(super) fn isinstance_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let value = args[0];
    let class_value = args[1];

    let Value::Class(class_id) = class_value else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'isinstance' expected class as second argument, got: {}",
                    class_value.to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };

    Ok(Value::Bool(value_isinstance(&vm.heap, value, class_id)))
}

/// Check if sub is the same class as super or is a subclass of it.
/// Similar to Python's issubclass(sub, super).
pub(super) fn issubclass_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let sub_value = args[0];
    let super_value = args[1];

    match (sub_value, super_value) {
        (Value::Class(sub_class_id), Value::Class(super_class_id)) => Ok(Value::Bool(
            is_subclass_of(&vm.heap, sub_class_id, super_class_id),
        )),
        (not_class, Value::Class(_)) => Err(vm
            .throw(
                TypeError,
                &format!(
                    "'issubclass' expected class as first argument, got: {}",
                    not_class.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
        (Value::Class(_), not_class) => Err(vm
            .throw(
                TypeError,
                &format!(
                    "'issubclass' expected class as second argument, got: {}",
                    not_class.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
        (not_class1, not_class2) => Err(vm
            .throw(
                TypeError,
                &format!(
                    "'issubclass' expected two classes as arguments, got: {} and {}",
                    not_class1.to_string(&vm.heap),
                    not_class2.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

/// Whether `value` is a dict instance.
fn is_dict(heap: &Heap, value: Value) -> bool {
    matches!(value, Value::Instance(id)
        if matches!(&id.to_value(heap).backing, Some(NativeClass::Dict(_))))
}

/// Read a `(name, value)` list out of a dict with string keys, for
/// injected locals and `Module` initialization. `what` names the
/// expectation in the error message.
fn named_values_from_dict(
    vm: &mut VM,
    dict_value: Value,
    what: &str,
) -> VmResult<Vec<(String, Value)>> {
    let entries: Vec<(Value, Value)> = dict_value
        .as_dict(&vm.heap)
        .items
        .iter()
        .map(|(key, value, _)| (*key, *value))
        .collect();
    let mut named = Vec::with_capacity(entries.len());
    for (key, value) in entries {
        let Value::String(key_id) = key else {
            return Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "{what} keys must be strings, got: {}",
                        key.to_string(&vm.heap)
                    ),
                )
                .unwrap_err());
        };
        named.push((vm.heap.strings[key_id].clone(), value));
    }
    Ok(named)
}

/// Shared implementation of `eval` and `exec`: decode the source string,
/// the optional target module, and the optional locals dict, then hand
/// off to the VM.
fn run_injected_native(vm: &mut VM, args: &[Value], kind: InjectedKind) -> VmResult<Value> {
    let Value::String(source_id) = args[0] else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'{kind}' expects a string as its first argument, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let (module, locals_dict) = match (args.get(1).copied(), args.get(2).copied()) {
        (None, _) => (None, None),
        (Some(Value::Module(module_id)), None) => (Some(module_id), None),
        (Some(second), None) if is_dict(&vm.heap, second) => (None, Some(second)),
        (Some(Value::Module(module_id)), Some(third)) if is_dict(&vm.heap, third) => {
            (Some(module_id), Some(third))
        }
        (Some(Value::Module(_)), Some(third)) => {
            return Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "'{kind}' expects a locals dict as its third argument, got: {}",
                        third.to_string(&vm.heap)
                    ),
                )
                .unwrap_err());
        }
        (Some(second), _) => {
            return Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "'{kind}' expects a module or a locals dict as its second argument, got: {}",
                        second.to_string(&vm.heap)
                    ),
                )
                .unwrap_err());
        }
    };
    let locals = match locals_dict {
        Some(dict) => named_values_from_dict(vm, dict, "locals dict")?,
        None => Vec::new(),
    };
    // The locals become the injected function's parameters, so the
    // 255-parameter limit applies to them.
    if locals.len() > usize::from(u8::MAX) {
        return Err(vm
            .throw(
                ValueError,
                &format!("'{kind}' accepts at most 255 locals, got: {}", locals.len()),
            )
            .unwrap_err());
    }
    let source = source_id.to_value(&vm.heap).clone();
    vm.run_injected_source(&source, module, &locals, kind)
}

/// `eval(source)` / `eval(source, module)` / `eval(source, locals)` /
/// `eval(source, module, locals)`: evaluate one expression and return
/// its value.
pub(super) fn eval_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    run_injected_native(vm, args, InjectedKind::Eval)
}

/// `exec(source)` / `exec(source, module)` / `exec(source, locals)` /
/// `exec(source, module, locals)`: run statements; returns nil.
pub(super) fn exec_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    run_injected_native(vm, args, InjectedKind::Exec)
}

/// `Module.__init__(name)` / `Module.__init__(name, init)`: a fresh
/// module value, its globals optionally initialized from a dict with
/// string keys (the entries are mutable). The module is anonymous: not
/// importable and not cached.
pub(super) fn module_init_native(
    vm: &mut VM,
    _receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let Value::String(name_id) = args[0] else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'Module' expects a string as its first argument, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let init = match args.get(1) {
        Some(&init) if is_dict(&vm.heap, init) => {
            named_values_from_dict(vm, init, "'Module' init dict")?
        }
        Some(x) => {
            return Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "'Module' expects a dict as its second argument, got: {}",
                        x.to_string(&vm.heap)
                    ),
                )
                .unwrap_err());
        }
        None => Vec::new(),
    };
    let path = PathBuf::from(format!("<module:{}>", vm.heap.strings[name_id].clone()));
    let mut module = Module::new(name_id, path, None, name_id, false);
    for (name, value) in init {
        let name_id = vm.heap.string_id(&name);
        module.globals.insert(
            name_id,
            Global {
                value,
                mutable: true,
            },
        );
    }
    Ok(vm.heap.add_module(module))
}

/// `ord(s)` - the Unicode code point of a single-character string.
pub(super) fn ord_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let Value::String(string_id) = args[0] else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'ord' expects a string, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let string = &vm.heap.strings[string_id];
    let mut chars = string.chars();
    match (chars.next(), chars.next()) {
        (Some(character), None) => Ok(i64::from(u32::from(character)).into()),
        _ => Err(vm
            .throw(
                ValueError,
                &format!(
                    "'ord' expects a single character, got a string of length {}",
                    string.chars().count()
                ),
            )
            .unwrap_err()),
    }
}

/// `chr(n)` - the single-character string for a Unicode code point.
pub(super) fn chr_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let Value::Number(Number::Integer(integer)) = args[0] else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'chr' expects an integer, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let code = u32::try_from(integer.to_bigint(&vm.heap))
        .ok()
        .and_then(char::from_u32);
    match code {
        Some(character) => Ok(vm.heap.string_id(&character.to_string()).into()),
        None => Err(vm
            .throw(
                ValueError,
                &format!(
                    "'chr' expects a valid code point, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

/// `hash(x)` - the hash of a hashable value, as an integer.
pub(super) fn hash_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let hash = vm.compute_hash(args[0])?;
    Ok(hash.cast_signed().into())
}

/// `round(x)` - the nearest integer to a number, ties to even.
pub(super) fn round_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    match &args[0] {
        Value::Number(Number::Integer(_)) => Ok(args[0]),
        Value::Number(number) => {
            let rounded = number.to_f64(&vm.heap).round_ties_even();
            match num_bigint::BigInt::from_f64(rounded) {
                Some(big) => match i64::try_from(&big) {
                    Ok(integer) => Ok(integer.into()),
                    Err(_) => Ok(vm.heap.add_big_int(big)),
                },
                None => Err(vm
                    .throw(ValueError, "'round' cannot round a non-finite float")
                    .unwrap_err()),
            }
        }
        _ => Err(vm
            .throw(
                TypeError,
                &format!(
                    "'round' expects a number, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}
