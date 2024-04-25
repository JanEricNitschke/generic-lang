use rand::Rng;
use std::io;
use std::thread;
use std::time::Duration;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    heap::Heap,
    value::{ias_f64, ias_u64, Number, Value},
};

pub(super) fn clock_native(_heap: &mut Heap, _args: &mut [&mut Value]) -> Result<Value, String> {
    Ok(Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64()
            .into(),
    ))
}

pub(super) fn sleep_native(_heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::Number(Number::Integer(i)) if i >= &0 => {
            thread::sleep(Duration::from_secs(ias_u64(*i)));
        }
        x => {
            return Err(format!(
                "'sleep' expected positive integer argument, got: `{}`",
                *x
            ));
        }
    };
    Ok(Value::Nil)
}

pub(super) fn assert_native(_heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    let value = &args[0];
    if value.is_falsey() {
        Err(format!("Assertion on `{value}` failed!"))
    } else {
        Ok(Value::Nil)
    }
}

pub(super) fn sqrt_native(_heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::Number(Number::Float(n)) => Ok(n.sqrt().into()),
        Value::Number(Number::Integer(n)) => Ok((ias_f64(*n)).sqrt().into()),
        x => Err(format!("'sqrt' expected numeric argument, got: {}", *x)),
    }
}

pub(super) fn input_native(heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::String(prompt) => {
            println!("{}", &heap.strings[prompt]);
            let mut choice = String::new();
            match io::stdin().read_line(&mut choice) {
                Ok(_) => {
                    let string = Value::String(heap.string_id(&choice.trim()));
                    Ok(string)
                }
                Err(e) => Err(format!("'input' could not read line: {e}")),
            }
        }
        x => Err(format!("'input' expected string argument, got: {x}")),
    }
}

#[allow(clippy::option_if_let_else)]
pub(super) fn to_float_native(heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::String(string_id) => {
            let string = &heap.strings[string_id];
            let converted: Result<f64, _> = string.parse();
            match converted {
                Ok(result) => Ok(Value::Number(result.into())),
                Err(_) => Err(format!(
                    "'float' could not convert string '{string}' to a float."
                )),
            }
        }
        Value::Number(n) => Ok(Value::Number(f64::from(*n).into())),
        Value::Bool(value) => Ok(Value::Number(f64::from(*value).into())),
        x => Err(format!(
            "'float' expected string, number or bool argument, got: {x}"
        )),
    }
}

#[allow(clippy::option_if_let_else)]
pub(super) fn to_int_native(heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::String(string_id) => {
            let string = &heap.strings[string_id];
            let converted: Result<i64, _> = string.parse();
            match converted {
                Ok(result) => Ok(Value::Number(result.into())),
                Err(_) => Err(format!(
                    "'int' could not convert string '{string}' to an integer."
                )),
            }
        }
        Value::Number(n) => Ok(Value::Number(i64::from(*n).into())),
        Value::Bool(value) => Ok(Value::Number(i64::from(*value).into())),
        x => Err(format!(
            "'int' expected string, number or bool argument, got: {x}"
        )),
    }
}

#[allow(clippy::option_if_let_else)]
pub(super) fn is_int_native(heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::String(string_id) => {
            let string = &heap.strings[string_id];
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

pub(super) fn to_string_native(heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    let value = &args[0];
    let string = Value::String(heap.string_id(&value.to_string()));
    Ok(string)
}

pub(super) fn type_native(heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    let string = match &args[0] {
        Value::Bool(_) => Value::String(heap.string_id(&"<type bool>")),
        Value::BoundMethod(_) => Value::String(heap.string_id(&"<type bound method>")),
        Value::Class(_) => Value::String(heap.string_id(&"<type class>")),
        Value::Closure(_) => Value::String(heap.string_id(&"<type closure>")),
        Value::Function(_) => Value::String(heap.string_id(&"<type function>")),
        Value::Instance(instance) => Value::String(
            heap.string_id(&("<type ".to_string() + instance.class.name.as_str() + ">")),
        ),
        Value::NativeFunction(_) => Value::String(heap.string_id(&"<type native function>")),
        Value::NativeMethod(_) => Value::String(heap.string_id(&"<type native method>")),
        Value::Nil => Value::String(heap.string_id(&"<type nil>")),
        Value::Number(n) => match n {
            Number::Float(_) => Value::String(heap.string_id(&"<type float>")),
            Number::Integer(_) => Value::String(heap.string_id(&"<type int>")),
        },
        Value::String(_) => Value::String(heap.string_id(&"<type string>")),
        Value::Upvalue(_) => Value::String(heap.string_id(&"<type upvalue>")),
        Value::List(_) => Value::String(heap.string_id(&"<type list>")),
        Value::Module(_) => Value::String(heap.string_id(&"<type module>")),
    };
    Ok(string)
}

pub(super) fn print_native(heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    let end = if args.len() == 2 {
        match &args[1] {
            Value::String(string_id) => &heap.strings[string_id],
            x => {
                return Err(format!(
                    "Optional second argument to 'print' has to be a string, got: {x}"
                ))
            }
        }
    } else {
        "\n"
    };
    let value = &args[0];
    print!("{value}{end}");
    Ok(Value::Nil)
}

pub(super) fn rng_native(_heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    match (&args[0], &args[1]) {
        (Value::Number(Number::Integer(min)), Value::Number(Number::Integer(max))) => Ok(
            Value::Number(rand::thread_rng().gen_range(*min..*max).into()),
        ),
        (other_1, other_2) => Err(format!(
            "'rng' expected two integers as arguments, got: `{other_1}` and `{other_2}` instead."
        )),
    }
}
