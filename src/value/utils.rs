//! Utility functions for value parsing and conversion.

use crate::value::Value;
use crate::vm::VM;
use num_bigint::BigInt;

/// Result type for integer parsing that can be either a small i64 or a BigInt.
#[derive(Debug, Clone)]
pub enum ParsedInteger {
    Small(i64),
    Big(BigInt),
}

/// Parse a string to integer for the compiler (without VM heap access).
/// Returns a ParsedInteger enum to indicate whether it's a small i64 or a BigInt.
pub fn parse_integer_compiler(string: &str) -> Result<ParsedInteger, String> {
    if let Ok(value) = string.parse::<i64>() {
        Ok(ParsedInteger::Small(value))
    } else {
        match string.parse::<BigInt>() {
            Ok(bigint) => Ok(ParsedInteger::Big(bigint)),
            Err(_) => Err(format!(
                "Could not convert string '{}' to an integer.",
                string
            )),
        }
    }
}

/// Parse a string to float for the compiler.
pub fn parse_float_compiler(string: &str) -> Result<f64, String> {
    string
        .parse()
        .map_err(|_| format!("Could not convert string '{}' to a float.", string))
}

/// Parse a string to integer, supporting BigInt for large numbers.
/// This function uses parse_integer_compiler internally and is shared between value constructors.
pub fn parse_string_to_integer(vm: &mut VM, string: &str) -> Result<Value, String> {
    match parse_integer_compiler(string)? {
        ParsedInteger::Small(value) => Ok(Value::Number(value.into())),
        ParsedInteger::Big(bigint) => {
            let bigint_value = vm.heap.add_big_int(bigint);
            Ok(Value::Number((*bigint_value.as_generic_int()).into()))
        }
    }
}

/// Parse a string to float.
/// This function is shared between value constructors and the compiler.
pub fn parse_string_to_float(string: &str) -> Result<Value, String> {
    let converted: Result<f64, _> = string.parse();
    match converted {
        Ok(result) => Ok(Value::Number(result.into())),
        Err(_) => Err(format!("Could not convert string '{}' to a float.", string)),
    }
}

/// Convert a value to string, handling instances with __str__ methods.
/// This function is shared between value constructors, to_string_native, and print_native.
pub fn value_to_string(vm: &mut VM, value: &Value) -> Result<Value, String> {
    let str_id = vm.heap.string_id(&"__str__");

    if let Value::Instance(instance) = value
        && let Some(str_method) = instance
            .to_value(&vm.heap)
            .get_field_or_method(str_id, &vm.heap)
    {
        // Push the value onto the stack temporarily so invoke_and_run_function can access it
        vm.stack.push(*value);
        vm.invoke_and_run_function(str_id, 0, matches!(str_method, Value::NativeMethod(_)));
        let returned_value = vm.stack.pop().expect("Stack underflow in value_to_string");
        Ok(returned_value)
    } else {
        Ok(Value::String(vm.heap.string_id(&value.to_string(&vm.heap))))
    }
}
