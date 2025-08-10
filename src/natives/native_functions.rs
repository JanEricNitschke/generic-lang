//! Module containing free standing rust native functions.

use crate::value::GenericInt;
use crate::{
    value::{Number, Value},
    vm::VM,
};
use rand::Rng;
use std::io;
use std::thread;
use std::time::Duration;
use std::time::{SystemTime, UNIX_EPOCH};

/// Get the time since the `UNIX_EPOCH` in seconds.
/// Useful for timing durations by calling this twice and subtracting the results.
pub(super) fn clock_native(_vm: &mut VM, _args: &mut [&mut Value]) -> Result<Value, String> {
    Ok(Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64()
            .into(),
    ))
}

/// Sleep for a non-negative number of seconds.
pub(super) fn sleep_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::Number(Number::Integer(i)) if i.ge_i64(0, &vm.heap) => {
            match i.try_to_u64(&vm.heap) {
                Ok(u) => thread::sleep(Duration::from_secs(u)),
                Err(_) => {
                    return Err(format!(
                        "'sleep' argument too large: `{}`",
                        Value::from(*i).to_string(&vm.heap)
                    ));
                }
            }
        }
        x => {
            return Err(format!(
                "'sleep' expected positive integer argument, got: `{}`",
                x.to_string(&vm.heap)
            ));
        }
    }
    Ok(Value::Nil)
}

/// Error if the argument is falsey.
pub(super) fn assert_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    let value = &args[0];
    if vm.is_falsey(**value) {
        Err(format!(
            "Assertion on `{}` failed!",
            value.to_string(&vm.heap)
        ))
    } else {
        Ok(Value::Nil)
    }
}

// Could also make a zero arg version of this if a prompt is not desired..
/// Read input from the command line after providing a prompt.
pub(super) fn input_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::String(prompt) => {
            println!("{}", &vm.heap.strings[*prompt]);
            let mut choice = String::new();
            match io::stdin().read_line(&mut choice) {
                Ok(_) => {
                    let string = Value::String(vm.heap.string_id(&choice.trim()));
                    Ok(string)
                }
                Err(e) => Err(format!("'input' could not read line: {e}")),
            }
        }
        x => Err(format!(
            "'input' expected string argument, got: {}",
            x.to_string(&vm.heap)
        )),
    }
}

/// Turn a value into a float.
/// Works on numbers, bools or sensible strings.
#[allow(clippy::option_if_let_else)]
pub(super) fn to_float_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::String(string_id) => {
            let string = &vm.heap.strings[*string_id];
            let converted: Result<f64, _> = string.parse();
            match converted {
                Ok(result) => Ok(Value::Number(result.into())),
                Err(_) => Err(format!(
                    "'float' could not convert string '{string}' to a float."
                )),
            }
        }
        Value::Number(n) => Ok(Value::Number(n.to_f64(&vm.heap).into())),
        Value::Bool(value) => Ok(Value::Number(f64::from(*value).into())),
        x => Err(format!(
            "'float' expected string, number or bool argument, got: {}",
            x.to_string(&vm.heap)
        )),
    }
}

/// Convert a value into an integer.
/// Works on numbers, bools or sensible strings.
#[allow(clippy::option_if_let_else)]
pub(super) fn to_int_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::String(string_id) => {
            let string = &vm.heap.strings[*string_id];
            let converted: Result<i64, _> = string.parse();
            match converted {
                Ok(result) => Ok(Value::Number(result.into())),
                Err(_) => Err(format!(
                    "'int' could not convert string '{string}' to an integer."
                )),
            }
        }
        Value::Number(n) => match n {
            Number::Float(f) => match GenericInt::try_from_f64(*f, &mut vm.heap) {
                Ok(i) => Ok(Value::Number(i.into())),
                Err(_) => Err(format!(
                    "'int' could not convert float '{f}' to an integer."
                )),
            },
            Number::Integer(_) => Ok(Value::Number(*n)),
            Number::Rational(rational) => match rational.to_int(&vm.heap) {
                Ok(i) => Ok(Value::Number(i.into())),
                Err(_) => Err("'int' could not convert rational to an integer.".to_string()),
            },
        },
        Value::Bool(value) => Ok(Value::Number(i64::from(*value).into())),
        x => Err(format!(
            "'int' expected string, number or bool argument, got: {}",
            x.to_string(&vm.heap)
        )),
    }
}

/// Check if the provided value can be turned into an integer.
#[allow(clippy::option_if_let_else)]
pub(super) fn is_int_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
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
pub(super) fn to_string_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    let value = &args[0];
    let str_id = vm.heap.string_id(&"__str__");

    if let Value::Instance(instance) = value
        && let Some(str_method) = instance
            .to_value(&vm.heap)
            .get_field_or_method(str_id, &vm.heap)
    {
        vm.invoke_and_run_function(str_id, 0, matches!(str_method, Value::NativeMethod(_)));
        let returned_value = vm.stack.pop().expect("Stack underflow in print_native");
        Ok(returned_value)
    } else {
        Ok(Value::String(vm.heap.string_id(&value.to_string(&vm.heap))))
    }
}

/// Return the type of the value as a string.
pub(super) fn type_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
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
/// Optionally supply a string to be printed at the end of the value.
/// Defaults to `\n`.
pub(super) fn print_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    let str_id = vm.heap.string_id(&"__str__");
    let end = if args.len() == 2 {
        match &args[1] {
            Value::String(string_id) => &string_id.to_value(&vm.heap).clone(),
            x => {
                return Err(format!(
                    "Optional second argument to 'print' has to be a string, got: {}",
                    x.to_string(&vm.heap)
                ));
            }
        }
    } else {
        "\n"
    };
    let value = &args[0];
    if let Value::Instance(instance) = **value
        && let Some(str_method) = instance
            .to_value(&vm.heap)
            .get_field_or_method(str_id, &vm.heap)
    {
        vm.invoke_and_run_function(str_id, 0, matches!(str_method, Value::NativeMethod(_)));
        let returned_value = vm.stack.pop().expect("Stack underflow in print_native");
        print!("{}{end}", returned_value.to_string(&vm.heap));
    } else {
        print!("{}{end}", value.to_string(&vm.heap));
    }

    Ok(Value::Nil)
}

/// Return a random integer between the two arguments.
/// Lower value is inclusive, upper value is exclusive.
pub(super) fn rng_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    match (&args[0], &args[1]) {
        (Value::Number(Number::Integer(min)), Value::Number(Number::Integer(max))) => {
            match (min, max) {
                (GenericInt::Small(min), GenericInt::Small(max)) => {
                    Ok(Value::Number(rand::rng().random_range(*min..*max).into()))
                }
                _ => Err(format!(
                    "'rng' expected small integers (i64) as arguments, got: `{}` and `{}` instead.",
                    min.to_string(&vm.heap),
                    max.to_string(&vm.heap)
                )),
            }
        }
        (other_1, other_2) => Err(format!(
            "'rng' expected two integers as arguments, got: `{}` and `{}` instead.",
            other_1.to_string(&vm.heap),
            other_2.to_string(&vm.heap)
        )),
    }
}

/// Get an attribute from a value by name.
pub(super) fn getattr_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    match (&args[0], &args[1]) {
        (Value::Instance(instance), Value::String(string_id)) => {
            let field = &vm.heap.strings[*string_id];
            instance.to_value(&vm.heap).fields.get(field).map_or_else(
                || Err(format!("Undefined property '{}'.", *field)),
                |value_id| Ok(*value_id),
            )
        }
        (instance @ Value::Instance(_), x) => Err(format!(
            "`getattr` can only index with string indexes, got: `{}` (instance: `{}`)",
            x.to_string(&vm.heap),
            instance.to_string(&vm.heap)
        )),
        (not_instance, _) => Err(format!(
            "`getattr` only works on instances, got `{}`",
            not_instance.to_string(&vm.heap)
        )),
    }
}

/// Set an attribute of a value by name.
pub(super) fn setattr_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    let field = if let &mut Value::String(ref string_id) = args[1] {
        vm.heap.strings[*string_id].clone()
    } else {
        return Err(format!(
            "`setattr` can only index with string indexes, got: `{}` (instance: `{}`)",
            args[1].to_string(&vm.heap),
            args[0].to_string(&vm.heap)
        ));
    };
    let value = *args[2];
    if let Value::Instance(instance) = args[0] {
        instance
            .to_value_mut(&mut vm.heap)
            .fields
            .insert(field, value);
        Ok(Value::Nil)
    } else {
        Err(format!(
            "`setattr` only works on instances, got `{}`",
            args[0].to_string(&vm.heap)
        ))
    }
}

/// Check if the given attribute exists as a property on the instance.
/// Does NOT check for methods.
pub(super) fn hasattr_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    match (&args[0], &args[1]) {
        (Value::Instance(instance), Value::String(string_id)) => Ok(Value::Bool(
            instance
                .to_value(&vm.heap)
                .fields
                .contains_key(&vm.heap.strings[*string_id]),
        )),
        (instance @ Value::Instance(_), x) => Err(format!(
            "`hasattr` can only index with string indexes, got: `{}` (instance: `{}`)",
            x.to_string(&vm.heap),
            instance.to_string(&vm.heap)
        )),
        (not_instance, _) => Err(format!(
            "`hasattr` only works on instances, got `{}`",
            not_instance.to_string(&vm.heap)
        )),
    }
}

/// Delete an attribute on an instance by name.
/// Does NOT work on methods. Errors if the attribute does not exist in the first place.
pub(super) fn delattr_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    if let &mut Value::String(ref string_id) = args[1] {
        let field = &vm.heap.strings[*string_id].clone();
        if let Value::Instance(instance) = args[0] {
            match instance.to_value_mut(&mut vm.heap).fields.remove(field) {
                Some(_) => Ok(Value::Nil),
                None => Err(format!("Undefined property '{field}'.")),
            }
        } else {
            Err(format!(
                "`delattr` only works on instances, got `{}`",
                args[0].to_string(&vm.heap)
            ))
        }
    } else {
        Err(format!(
            "`delattr` can only index with string indexes, got: `{}` (instance: `{}`)",
            args[1].to_string(&vm.heap),
            args[0].to_string(&vm.heap)
        ))
    }
}

/// Return the length of a list.
#[allow(clippy::cast_possible_wrap)]
pub(super) fn len_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    let len_method_id = vm.heap.string_id(&"__len__");

    if let Value::Instance(instance) = args[0]
        && let Some(len_method) = instance
            .to_value(&vm.heap)
            .get_field_or_method(len_method_id, &vm.heap)
    {
        vm.invoke_and_run_function(
            len_method_id,
            0,
            matches!(len_method, Value::NativeMethod(_)),
        );
        let result = vm.stack.pop().expect("Stack underflow in len_native");
        return Ok(result);
    }

    Err("Undefined property '__len__'.".into())
}
