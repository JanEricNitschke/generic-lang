//! Utility functions for value parsing and conversion.

use crate::value::Value;
use crate::vm::VM;
use num_bigint::BigInt;

/// Result type for integer parsing that can be either a small i64 or a `BigInt`.
#[derive(Debug, Clone)]
pub enum ParsedInteger {
    Small(i64),
    Big(BigInt),
}

/// Parse a string to integer for the compiler (without VM heap access).
/// Returns a `ParsedInteger` enum to indicate whether it's a small i64 or a `BigInt`.
pub fn parse_integer_compiler(string: &str) -> Result<ParsedInteger, String> {
    if let Ok(value) = string.parse::<i64>() {
        Ok(ParsedInteger::Small(value))
    } else {
        match string.parse::<BigInt>() {
            Ok(bigint) => Ok(ParsedInteger::Big(bigint)),
            Err(_) => Err(format!(
                "Could not convert string '{string}' to an integer."
            )),
        }
    }
}

/// Parse a string to float for the compiler.
pub fn parse_float_compiler(string: &str) -> Result<f64, String> {
    string
        .parse()
        .map_err(|_| format!("Could not convert string '{string}' to a float."))
}

/// Parse a string to integer, supporting `BigInt` for large numbers.
/// This function uses `parse_integer_compiler` internally and is shared between value constructors.
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
/// This function uses `parse_float_compiler` internally and is shared between value constructors.
pub fn parse_string_to_float(string: &str) -> Result<Value, String> {
    let float_value = parse_float_compiler(string)?;
    Ok(Value::Number(float_value.into()))
}
