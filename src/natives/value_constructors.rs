//! Module containing native constructor methods for value types.

use crate::value::{GenericInt, Number, Value};
use crate::vm::VM;

/// Bool.__init__(value) - Convert any value to boolean using is_falsey logic
pub(super) fn bool_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    if args.len() != 1 {
        return Err(format!(
            "Bool.__init__() expected 1 argument, got {}",
            args.len()
        ));
    }
    
    let value = args[0].clone();
    let is_falsey = vm.is_falsey(value);
    Ok(Value::Bool(!is_falsey))
}

/// String.__init__(value) - Convert any value to string using to_string_native logic
pub(super) fn string_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    if args.len() != 1 {
        return Err(format!(
            "String.__init__() expected 1 argument, got {}",
            args.len()
        ));
    }
    
    let value = &args[0];
    let str_id = vm.heap.string_id(&"__str__");

    if let Value::Instance(instance) = value
        && let Some(str_method) = instance
            .to_value(&vm.heap)
            .get_field_or_method(str_id, &vm.heap)
    {
        // Push the value onto the stack temporarily so invoke_and_run_function can access it
        vm.stack.push((*value).clone());
        vm.invoke_and_run_function(str_id, 0, matches!(str_method, Value::NativeMethod(_)));
        let returned_value = vm.stack.pop().expect("Stack underflow in string_init_native");
        Ok(returned_value)
    } else {
        Ok(Value::String(vm.heap.string_id(&value.to_string(&vm.heap))))
    }
}

/// Integer.__init__(value) - Convert string or number to integer
pub(super) fn integer_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    if args.len() != 1 {
        return Err(format!(
            "Integer.__init__() expected 1 argument, got {}",
            args.len()
        ));
    }
    
    match &args[0] {
        Value::String(string_id) => {
            let string = &vm.heap.strings[*string_id];
            let converted: Result<i64, _> = string.parse();
            match converted {
                Ok(result) => Ok(Value::Number(result.into())),
                Err(_) => {
                    // Try parsing as BigInt if i64 parsing fails
                    match string.parse::<num_bigint::BigInt>() {
                        Ok(bigint) => {
                            let bigint_value = vm.heap.add_big_int(bigint);
                            Ok(Value::Number((*bigint_value.as_generic_int()).into()))
                        }
                        Err(_) => Err(format!(
                            "Integer.__init__() could not convert string '{}' to an integer.",
                            string
                        )),
                    }
                }
            }
        }
        Value::Number(n) => match n {
            Number::Float(f) => match GenericInt::try_from_f64(*f, &mut vm.heap) {
                Ok(i) => Ok(Value::Number(i.into())),
                Err(_) => Err(format!(
                    "Integer.__init__() could not convert float '{}' to an integer.",
                    f
                )),
            },
            Number::Integer(_) => Ok(Value::Number(*n)),
            Number::Rational(rational) => match rational.to_int(&vm.heap) {
                Ok(i) => Ok(Value::Number(i.into())),
                Err(_) => Err("Integer.__init__() could not convert rational to an integer.".to_string()),
            },
        },
        Value::Bool(value) => Ok(Value::Number(i64::from(*value).into())),
        x => Err(format!(
            "Integer.__init__() expected string, number or bool argument, got: {}",
            x.to_string(&vm.heap)
        )),
    }
}

/// Float.__init__(value) - Convert string or number to float
pub(super) fn float_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    if args.len() != 1 {
        return Err(format!(
            "Float.__init__() expected 1 argument, got {}",
            args.len()
        ));
    }
    
    match &args[0] {
        Value::String(string_id) => {
            let string = &vm.heap.strings[*string_id];
            let converted: Result<f64, _> = string.parse();
            match converted {
                Ok(result) => Ok(Value::Number(result.into())),
                Err(_) => Err(format!(
                    "Float.__init__() could not convert string '{}' to a float.",
                    string
                )),
            }
        }
        Value::Number(n) => Ok(Value::Number(n.to_f64(&vm.heap).into())),
        Value::Bool(value) => Ok(Value::Number(f64::from(*value).into())),
        x => Err(format!(
            "Float.__init__() expected string, number or bool argument, got: {}",
            x.to_string(&vm.heap)
        )),
    }
}

/// Rational.__init__(numerator, denominator) - Create rational from two integers
pub(super) fn rational_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    if args.len() != 2 {
        return Err(format!(
            "Rational.__init__() expected 2 arguments, got {}",
            args.len()
        ));
    }
    
    match (&args[0], &args[1]) {
        (Value::Number(Number::Integer(numerator)), Value::Number(Number::Integer(denominator))) => {
            if denominator.is_zero(&vm.heap) {
                return Err("Rational.__init__() denominator cannot be zero".to_string());
            }
            
            let rational = crate::value::GenericRational::new(*numerator, *denominator, &mut vm.heap)
                .map_err(|e| format!("Rational.__init__() failed to create rational: {}", e))?;
            
            Ok(Value::Number(Number::Rational(rational)))
        }
        (a, b) => Err(format!(
            "Rational.__init__() expected two integer arguments, got: {}, {}",
            a.to_string(&vm.heap),
            b.to_string(&vm.heap)
        )),
    }
}