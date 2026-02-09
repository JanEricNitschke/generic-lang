//! Module containing free standing rust native functions.

use crate::{
    value::{GenericInt, Number, Value, get_native_class_id, is_subclass_of},
    vm::{VM, errors::VmResult},
};
use rand::RngExt;
use std::io;
use std::thread;
use std::time::Duration;
use std::time::{SystemTime, UNIX_EPOCH};

/// Get the time since the `UNIX_EPOCH` in seconds.
/// Useful for timing durations by calling this twice and subtracting the results.
pub(super) fn clock_native(_vm: &mut VM, _args: &mut [&mut Value]) -> VmResult<Value> {
    Ok(Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64()
            .into(),
    ))
}

/// Sleep for a non-negative number of seconds.
pub(super) fn sleep_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    match &args[0] {
        Value::Number(Number::Integer(i)) if i.ge_i64(0, &vm.heap) => {
            match i.try_to_u64(&vm.heap) {
                Ok(u) => thread::sleep(Duration::from_secs(u)),
                Err(_) => {
                    return Err(vm
                        .throw_value_error(&format!(
                            "'sleep' argument too large: `{}`",
                            Value::from(*i).to_string(&vm.heap)
                        ))
                        .unwrap_err());
                }
            }
        }
        x => {
            return Err(vm
                .throw_type_error(&format!(
                    "'sleep' expected positive integer argument, got: `{}`",
                    x.to_string(&vm.heap)
                ))
                .unwrap_err());
        }
    }
    Ok(Value::Nil)
}

/// Error if the argument is falsey.
pub(super) fn assert_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    let value = &args[0];
    if vm.is_falsey(**value)? {
        Err(vm
            .throw_assertion_error(&format!(
                "Assertion on `{}` failed!",
                value.to_string(&vm.heap)
            ))
            .unwrap_err())
    } else {
        Ok(Value::Nil)
    }
}

// Could also make a zero arg version of this if a prompt is not desired..
/// Read input from the command line after providing a prompt.
pub(super) fn input_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    match &args[0] {
        Value::String(prompt) => {
            println!("{}", &vm.heap.strings[*prompt]);
            let mut choice = String::new();
            match io::stdin().read_line(&mut choice) {
                Ok(_) => {
                    let string = Value::String(vm.heap.string_id(&choice.trim()));
                    Ok(string)
                }
                Err(e) => Err(vm
                    .throw_io_error(&format!("'input' could not read line: {e}"))
                    .unwrap_err()),
            }
        }
        x => Err(vm
            .throw_value_error(&format!(
                "'input' expected string argument, got: {}",
                x.to_string(&vm.heap)
            ))
            .unwrap_err()),
    }
}

/// Turn a value into a float.
/// Works on numbers, bools or sensible strings.
#[allow(clippy::option_if_let_else)]
pub(super) fn to_float_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    match &args[0] {
        Value::String(string_id) => {
            let string = &vm.heap.strings[*string_id];
            let converted: Result<f64, _> = string.parse();
            match converted {
                Ok(result) => Ok(Value::Number(result.into())),
                Err(_) => Err(vm
                    .throw_value_error(&format!(
                        "'float' could not convert string '{string}' to a float."
                    ))
                    .unwrap_err()),
            }
        }
        Value::Number(n) => Ok(Value::Number(n.to_f64(&vm.heap).into())),
        Value::Bool(value) => Ok(Value::Number(f64::from(*value).into())),
        x => Err(vm
            .throw_type_error(&format!(
                "'float' expected string, number or bool argument, got: {}",
                x.to_string(&vm.heap)
            ))
            .unwrap_err()),
    }
}

/// Convert a value into an integer.
/// Works on numbers, bools or sensible strings.
#[allow(clippy::option_if_let_else)]
pub(super) fn to_int_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    match &args[0] {
        Value::String(string_id) => {
            let string = &vm.heap.strings[*string_id];
            let converted: Result<i64, _> = string.parse();
            match converted {
                Ok(result) => Ok(Value::Number(result.into())),
                Err(_) => Err(vm
                    .throw_value_error(&format!(
                        "'int' could not convert string '{string}' to an integer."
                    ))
                    .unwrap_err()),
            }
        }
        Value::Number(n) => match n {
            Number::Float(f) => match GenericInt::try_from_f64(*f, &mut vm.heap) {
                Ok(i) => Ok(Value::Number(i.into())),
                Err(_) => Err(vm
                    .throw_value_error(&format!(
                        "'int' could not convert float '{f}' to an integer."
                    ))
                    .unwrap_err()),
            },
            Number::Integer(_) => Ok(Value::Number(*n)),
            Number::Rational(rational) => match rational.to_int(&vm.heap) {
                Ok(i) => Ok(Value::Number(i.into())),
                Err(_) => Err(vm
                    .throw_value_error(&format!(
                        "'int' could not convert rational '{rational:?}' to an integer."
                    ))
                    .unwrap_err()),
            },
        },
        Value::Bool(value) => Ok(Value::Number(i64::from(*value).into())),
        x => Err(vm
            .throw_type_error(&format!(
                "'int' expected string, number or bool argument, got: {}",
                x.to_string(&vm.heap)
            ))
            .unwrap_err()),
    }
}

/// Check if the provided value can be turned into an integer.
#[allow(clippy::option_if_let_else)]
pub(super) fn is_int_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
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
pub(super) fn to_string_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    Ok(Value::String(vm.value_to_string(args[0])?))
}

/// Return the type of the value as a string.
pub(super) fn type_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
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
pub(super) fn print_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    let end = if args.len() == 2 {
        match &args[1] {
            Value::String(string_id) => &string_id.to_value(&vm.heap).clone(),
            x => {
                return Err(vm
                    .throw_type_error(&format!(
                        "Optional second argument to 'print' has to be a string, got: {}",
                        x.to_string(&vm.heap)
                    ))
                    .unwrap_err());
            }
        }
    } else {
        "\n"
    };

    // Use the shared value_to_string utility for consistent behavior
    let string_id = vm.value_to_string(args[0])?;
    print!("{}{end}", string_id.to_value(&vm.heap));

    Ok(Value::Nil)
}

/// Return a random integer between the two arguments.
/// Lower value is inclusive, upper value is exclusive.
pub(super) fn rng_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    match (&args[0], &args[1]) {
        (Value::Number(Number::Integer(min)), Value::Number(Number::Integer(max))) => {
            match (min, max) {
                (GenericInt::Small(min), GenericInt::Small(max)) => {
                    Ok(Value::Number(rand::rng().random_range(*min..*max).into()))
                }
                _ => Err(vm.throw_value_error(&format!(
                    "'rng' expected small integers (i64) as arguments, got: `{}` and `{}` instead.",
                    min.to_string(&vm.heap),
                    max.to_string(&vm.heap)
                )).unwrap_err()),
            }
        }
        (other_1, other_2) => Err(vm
            .throw_type_error(&format!(
                "'rng' expected two integers as arguments, got: `{}` and `{}` instead.",
                other_1.to_string(&vm.heap),
                other_2.to_string(&vm.heap)
            ))
            .unwrap_err()),
    }
}

/// Get an attribute from a value by name.
pub(super) fn getattr_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    match (&args[0], &args[1]) {
        (Value::Instance(instance), Value::String(string_id)) => {
            let field = &vm.heap.strings[*string_id];
            match instance.to_value(&vm.heap).fields.get(field) {
                Some(value_id) => Ok(*value_id),
                None => Err(vm
                    .throw_attribute_error(&format!("Undefined property '{}'.", *field))
                    .unwrap_err()),
            }
        }
        (instance @ Value::Instance(_), x) => Err(vm
            .throw_type_error(&format!(
                "`getattr` can only index with string indexes, got: `{}` (instance: `{}`)",
                x.to_string(&vm.heap),
                instance.to_string(&vm.heap)
            ))
            .unwrap_err()),
        (not_instance, _) => Err(vm
            .throw_type_error(&format!(
                "`getattr` only works on instances, got `{}`",
                not_instance.to_string(&vm.heap)
            ))
            .unwrap_err()),
    }
}

/// Set an attribute of a value by name.
pub(super) fn setattr_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    let field = if let &mut Value::String(ref string_id) = args[1] {
        vm.heap.strings[*string_id].clone()
    } else {
        return Err(vm
            .throw_type_error(&format!(
                "`setattr` can only index with string indexes, got: `{}` (instance: `{}`)",
                args[1].to_string(&vm.heap),
                args[0].to_string(&vm.heap)
            ))
            .unwrap_err());
    };
    let value = *args[2];
    if let Value::Instance(instance) = args[0] {
        instance
            .to_value_mut(&mut vm.heap)
            .fields
            .insert(field, value);
        Ok(Value::Nil)
    } else {
        Err(vm
            .throw_type_error(&format!(
                "`setattr` only works on instances, got `{}`",
                args[0].to_string(&vm.heap)
            ))
            .unwrap_err())
    }
}

/// Check if the given attribute exists as a property on the instance.
/// Does NOT check for methods.
pub(super) fn hasattr_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    match (&args[0], &args[1]) {
        (Value::Instance(instance), Value::String(string_id)) => Ok(Value::Bool(
            instance
                .to_value(&vm.heap)
                .fields
                .contains_key(&vm.heap.strings[*string_id]),
        )),
        (instance @ Value::Instance(_), x) => Err(vm
            .throw_type_error(&format!(
                "`hasattr` can only index with string indexes, got: `{}` (instance: `{}`)",
                x.to_string(&vm.heap),
                instance.to_string(&vm.heap)
            ))
            .unwrap_err()),
        (not_instance, _) => Err(vm
            .throw_type_error(&format!(
                "`hasattr` only works on instances, got `{}`",
                not_instance.to_string(&vm.heap)
            ))
            .unwrap_err()),
    }
}

/// Delete an attribute on an instance by name.
/// Does NOT work on methods. Errors if the attribute does not exist in the first place.
pub(super) fn delattr_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    if let &mut Value::String(ref string_id) = args[1] {
        let field = &vm.heap.strings[*string_id].clone();
        if let Value::Instance(instance) = args[0] {
            match instance.to_value_mut(&mut vm.heap).fields.remove(field) {
                Some(_) => Ok(Value::Nil),
                None => Err(vm
                    .throw_attribute_error(&format!("Undefined property '{field}'."))
                    .unwrap_err()),
            }
        } else {
            Err(vm
                .throw_type_error(&format!(
                    "`delattr` only works on instances, got `{}`",
                    args[0].to_string(&vm.heap)
                ))
                .unwrap_err())
        }
    } else {
        Err(vm
            .throw_type_error(&format!(
                "`delattr` can only index with string indexes, got: `{}` (instance: `{}`)",
                args[1].to_string(&vm.heap),
                args[0].to_string(&vm.heap)
            ))
            .unwrap_err())
    }
}

/// Return the length of an instance.
#[allow(clippy::cast_possible_wrap)]
pub(super) fn len_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    vm.invoke_method_by_name_with_attribute_error(*args[0], "__len__")
}

/// Get the next item from an iterator.
pub(super) fn next_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    vm.invoke_method_by_name_with_attribute_error(*args[0], "__next__")
}

/// Get the iterator from an iterable.
pub(super) fn iter_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    vm.invoke_method_by_name_with_attribute_error(*args[0], "__iter__")
}

/// Check if value is an instance of the given class or any of its subclasses.
/// Similar to Python's isinstance(value, classinfo).
pub(super) fn isinstance_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    let value = *args[0];
    let class_value = *args[1];

    let Value::Class(class_id) = class_value else {
        return Err(vm
            .throw_type_error(&format!(
                "'isinstance' expected class as second argument, got: {}",
                class_value.to_string(&vm.heap)
            ))
            .unwrap_err());
    };

    match value {
        Value::Instance(instance) => {
            let instance_class_id = instance.to_value(&vm.heap).class;
            Ok(Value::Bool(is_subclass_of(
                &vm.heap,
                instance_class_id,
                class_id,
            )))
        }
        Value::Bool(_) => {
            // Check if class is Bool proxy class
            Ok(Value::Bool(
                class_id == get_native_class_id(&vm.heap, "Bool"),
            ))
        }
        Value::String(_) => {
            // Check if class is String proxy class
            Ok(Value::Bool(
                class_id == get_native_class_id(&vm.heap, "String"),
            ))
        }
        Value::Number(number) => {
            // Check if class matches the specific number type proxy class
            match number {
                Number::Integer(_) => Ok(Value::Bool(
                    class_id == get_native_class_id(&vm.heap, "Integer"),
                )),
                Number::Float(_) => Ok(Value::Bool(
                    class_id == get_native_class_id(&vm.heap, "Float"),
                )),
                Number::Rational(_) => Ok(Value::Bool(
                    class_id == get_native_class_id(&vm.heap, "Rational"),
                )),
            }
        }
        // For other types (nil, etc.), isinstance should return False
        _ => Ok(Value::Bool(false)),
    }
}

/// Check if sub is the same class as super or is a subclass of it.
/// Similar to Python's issubclass(sub, super).
pub(super) fn issubclass_native(vm: &mut VM, args: &mut [&mut Value]) -> VmResult<Value> {
    let sub_value = *args[0];
    let super_value = *args[1];

    match (sub_value, super_value) {
        (Value::Class(sub_class_id), Value::Class(super_class_id)) => Ok(Value::Bool(
            is_subclass_of(&vm.heap, sub_class_id, super_class_id),
        )),
        (not_class, Value::Class(_)) => Err(vm
            .throw_type_error(&format!(
                "'issubclass' expected class as first argument, got: {}",
                not_class.to_string(&vm.heap)
            ))
            .unwrap_err()),
        (Value::Class(_), not_class) => Err(vm
            .throw_type_error(&format!(
                "'issubclass' expected class as second argument, got: {}",
                not_class.to_string(&vm.heap)
            ))
            .unwrap_err()),
        (not_class1, not_class2) => Err(vm
            .throw_type_error(&format!(
                "'issubclass' expected two classes as arguments, got: {} and {}",
                not_class1.to_string(&vm.heap),
                not_class2.to_string(&vm.heap)
            ))
            .unwrap_err()),
    }
}
