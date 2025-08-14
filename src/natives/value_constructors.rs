//! Native constructor functions for value types to allow instantiation.

use crate::{
    value::{GenericRational, Number, Value},
    vm::VM,
};
use num_bigint::BigInt;

/// Bool constructor function.
/// Accepts any argument and uses the VM's `is_falsey` to determine true/false.
pub(super) fn bool_init_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    let arg = if args.is_empty() {
        Value::Nil
    } else {
        **args.first().unwrap()
    };
    
    // Use VM's is_falsey logic to determine the bool value
    let is_false = vm.is_falsey(arg);
    Ok(Value::Bool(!is_false))
}

/// String constructor function.
/// Accepts any argument and converts it to string.
pub(super) fn string_init_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    let arg = if args.is_empty() {
        Value::Nil
    } else {
        **args.first().unwrap()
    };
    
    // Use the default to_string for any value
    Ok(Value::String(vm.heap.string_id(&arg.to_string(&vm.heap))))
}

/// Integer constructor function.
/// Accepts a single argument and converts it to integer.
pub(super) fn integer_init_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("Integer() missing required argument".to_string());
    }
    
    let arg = **args.first().unwrap();
    
    match arg {
        Value::String(string_id) => {
            let string = &vm.heap.strings[string_id];
            // Use the same parsing logic as the compiler
            if let Ok(value) = string.parse::<i64>() {
                Ok(Value::from(value))
            } else if let Ok(bigint) = string.parse::<BigInt>() {
                let bigint_id = vm.heap.add_big_int(bigint);
                Ok(Value::from(bigint_id))
            } else {
                Err(format!(
                    "Integer() could not convert string '{}' to an integer",
                    string
                ))
            }
        }
        Value::Number(Number::Integer(i)) => Ok(Value::Number(Number::Integer(i))),
        Value::Number(Number::Float(f)) => {
            // Convert float to integer (truncating)
            #[allow(clippy::cast_possible_truncation)]
            let int_val = f as i64;
            Ok(Value::from(int_val))
        }
        Value::Number(Number::Rational(rational)) => {
            // Convert rational to integer (truncating)
            let int_val = rational.to_f64(&vm.heap) as i64;
            Ok(Value::from(int_val))
        }
        Value::Bool(b) => Ok(Value::from(if b { 1_i64 } else { 0_i64 })),
        _ => Err(format!(
            "Integer() argument must be a string, number, or bool, not '{}'",
            arg.to_string(&vm.heap)
        )),
    }
}

/// Float constructor function.
/// Accepts a single argument and converts it to float.
pub(super) fn float_init_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("Float() missing required argument".to_string());
    }
    
    let arg = **args.first().unwrap();
    
    match arg {
        Value::String(string_id) => {
            let string = &vm.heap.strings[string_id];
            // Use the same parsing logic as the compiler
            match string.parse::<f64>() {
                Ok(value) => Ok(Value::from(value)),
                Err(_) => Err(format!(
                    "Float() could not convert string '{}' to a float",
                    string
                )),
            }
        }
        Value::Number(Number::Float(f)) => Ok(Value::Number(Number::Float(f))),
        Value::Number(n) => Ok(Value::from(n.to_f64(&vm.heap))),
        Value::Bool(b) => Ok(Value::from(if b { 1.0 } else { 0.0 })),
        _ => Err(format!(
            "Float() argument must be a string, number, or bool, not '{}'",
            arg.to_string(&vm.heap)
        )),
    }
}

/// Rational constructor function.
/// Accepts exactly two integer arguments: numerator and denominator.
pub(super) fn rational_init_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err(format!(
            "Rational() takes exactly 2 arguments ({} given)",
            args.len()
        ));
    }
    
    let numerator_arg = **args.first().unwrap();
    let denominator_arg = **args.get(1).unwrap();
    
    // Extract integer values
    let numerator = match numerator_arg {
        Value::Number(Number::Integer(i)) => i,
        _ => return Err(format!(
            "Rational() numerator must be an integer, not '{}'",
            numerator_arg.to_string(&vm.heap)
        )),
    };
    
    let denominator = match denominator_arg {
        Value::Number(Number::Integer(i)) => i,
        _ => return Err(format!(
            "Rational() denominator must be an integer, not '{}'",
            denominator_arg.to_string(&vm.heap)
        )),
    };
    
    // Check for zero denominator by converting to i64 and comparing
    let zero = Value::Number(Number::Integer(0_i64.into()));
    if let Value::Number(Number::Integer(zero_int)) = zero {
        if denominator.eq(&zero_int, &vm.heap) {
            return Err("Rational() denominator cannot be zero".to_string());
        }
    }
    
    // Create the rational value
    let rational = GenericRational::new(numerator, denominator, &mut vm.heap)?;
    Ok(Value::Number(Number::Rational(rational)))
}