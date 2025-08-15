//! Native classes for value type constructors.

use crate::{
    value::{GenericRational, Number, Value},
    vm::VM,
};
use num_bigint::BigInt;

/// Bool.__init__ method.
/// Accepts any argument and uses the VM's `is_falsey` to determine true/false.
/// Returns the boolean value, replacing the receiver on the stack.
pub(super) fn bool_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let arg = if args.is_empty() {
        Value::Nil
    } else {
        **args.first().unwrap()
    };

    // Use VM's is_falsey logic to determine the bool value
    let is_false = vm.is_falsey(arg);
    Ok(Value::Bool(!is_false))
}

/// String.__init__ method.
/// Accepts any argument and converts it to string.
/// Returns the string value, replacing the receiver on the stack.
pub(super) fn string_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let arg = if args.is_empty() {
        Value::Nil
    } else {
        **args.first().unwrap()
    };

    // Use the default to_string for any value
    Ok(Value::String(vm.heap.string_id(&arg.to_string(&vm.heap))))
}

/// Integer.__init__ method.
/// Accepts a single argument and converts it to integer.
/// Returns the integer value, replacing the receiver on the stack.
pub(super) fn integer_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let arg = **args.first().unwrap();

    match arg {
        Value::String(string_id) => {
            let string = &vm.heap.strings[string_id];
            // Use the same parsing logic as the compiler
            if let Ok(value) = string.parse::<i64>() {
                Ok(Value::from(value))
            } else if let Ok(bigint) = string.parse::<BigInt>() {
                Ok(vm.heap.add_big_int(bigint))
            } else {
                Err(format!(
                    "Integer() could not convert string '{string}' to an integer"
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
            #[allow(clippy::cast_possible_truncation)]
            let int_val = rational.to_f64(&vm.heap) as i64;
            Ok(Value::from(int_val))
        }
        Value::Bool(b) => Ok(Value::from(i64::from(b))),
        _ => Err(format!(
            "Integer() argument must be a string, number, or bool, not '{}'",
            arg.to_string(&vm.heap)
        )),
    }
}

/// Float.__init__ method.
/// Accepts a single argument and converts it to float.
/// Returns the float value, replacing the receiver on the stack.
pub(super) fn float_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let arg = **args.first().unwrap();

    match arg {
        Value::String(string_id) => {
            let string = &vm.heap.strings[string_id];
            // Use the same parsing logic as the compiler
            match string.parse::<f64>() {
                Ok(value) => Ok(Value::from(value)),
                Err(_) => Err(format!(
                    "Float() could not convert string '{string}' to a float"
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

/// Rational.__init__ method.
/// Accepts exactly two integer arguments: numerator and denominator.
/// Returns the rational value, replacing the receiver on the stack.
pub(super) fn rational_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let numerator_arg = **args.first().unwrap();
    let denominator_arg = **args.get(1).unwrap();

    // Extract integer values
    let Value::Number(Number::Integer(numerator)) = numerator_arg else {
        return Err(format!(
            "Rational() numerator must be an integer, not '{}'",
            numerator_arg.to_string(&vm.heap)
        ));
    };

    let Value::Number(Number::Integer(denominator)) = denominator_arg else {
        return Err(format!(
            "Rational() denominator must be an integer, not '{}'",
            denominator_arg.to_string(&vm.heap)
        ));
    };

    // Check for zero denominator by converting to i64 and comparing
    let zero = Value::Number(Number::Integer(0_i64.into()));
    if let Value::Number(Number::Integer(zero_int)) = zero
        && denominator.eq(&zero_int, &vm.heap)
    {
        return Err("Rational() denominator cannot be zero".to_string());
    }

    // Create the rational value
    let rational = GenericRational::new(numerator, denominator, &mut vm.heap)?;
    Ok(Value::Number(Number::Rational(rational)))
}
