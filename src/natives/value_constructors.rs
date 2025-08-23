//! Module containing native constructor methods for value types.

use crate::value::utils::{parse_string_to_float, parse_string_to_integer};
use crate::value::{GenericInt, GenericRational, Number, Value};
use crate::vm::VM;
use crate::vm::errors::VmError;

/// Bool.__init__(value) - Convert any value to boolean using `is_falsey` logic
pub(super) fn bool_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmError<Value> {
    let value = *args[0];
    let is_falsey = vm.is_falsey(value)?;
    Ok(Value::Bool(!is_falsey))
}

/// Integer.__init__(value) - Convert string or number to integer
pub(super) fn integer_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmError<Value> {
    match &args[0] {
        Value::String(string_id) => {
            let string = string_id.to_value(&vm.heap).clone();
            match parse_string_to_integer(vm, &string) {
                Ok(value) => Ok(value),
                Err(err) => Err(vm.throw_value_error(&err).unwrap_err()),
            }
        }
        Value::Number(n) => match n {
            Number::Float(f) => match GenericInt::try_from_f64(*f, &mut vm.heap) {
                Ok(i) => Ok(Value::Number(i.into())),
                Err(err) => Err(vm.throw_value_error(&err).unwrap_err()),
            },
            Number::Integer(_) => Ok(Value::Number(*n)),
            Number::Rational(rational) => match rational.to_int(&vm.heap) {
                Ok(i) => Ok(Value::Number(i.into())),
                Err(err) => Err(vm.throw_value_error(&err).unwrap_err()),
            },
        },
        Value::Bool(value) => Ok(Value::Number(i64::from(*value).into())),
        x => Err(vm
            .throw_type_error(&format!(
                "Integer.__init__() expected string, number or bool argument, got: {}",
                x.to_string(&vm.heap)
            ))
            .unwrap_err()),
    }
}

/// Float.__init__(value) - Convert string or number to float
pub(super) fn float_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmError<Value> {
    match &args[0] {
        Value::String(string_id) => {
            let string = string_id.to_value(&vm.heap).clone();
            match parse_string_to_float(&string) {
                Ok(value) => Ok(value),
                Err(err) => Err(vm.throw_value_error(&err).unwrap_err()),
            }
        }
        Value::Number(n) => Ok(Value::Number(n.to_f64(&vm.heap).into())),
        Value::Bool(value) => Ok(Value::Number(f64::from(*value).into())),
        x => Err(vm
            .throw_type_error(&format!(
                "Float.__init__() expected string, number or bool argument, got: {}",
                x.to_string(&vm.heap)
            ))
            .unwrap_err()),
    }
}

/// Rational.__init__(numerator, denominator) - Create rational from two integers
pub(super) fn rational_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmError<Value> {
    match (&args[0], &args[1]) {
        (
            Value::Number(Number::Integer(numerator)),
            Value::Number(Number::Integer(denominator)),
        ) => {
            if denominator.is_zero(&vm.heap) {
                return Err(vm
                    .throw_value_error("Rational.__init__() denominator cannot be zero")
                    .unwrap_err());
            }

            let rational = match GenericRational::new(*numerator, *denominator, &mut vm.heap) {
                Ok(value) => value,
                Err(err) => return Err(vm.throw_value_error(&err).unwrap_err()),
            };

            Ok(Value::Number(Number::Rational(rational)))
        }
        (a, b) => Err(vm
            .throw_type_error(&format!(
                "Rational.__init__() expected two integer arguments, got: {}, {}",
                a.to_string(&vm.heap),
                b.to_string(&vm.heap)
            ))
            .unwrap_err()),
    }
}
