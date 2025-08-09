//! Native methods for Range class

use crate::{
    value::Value,
    vm::VM,
};

/// Check if a range contains a value.
pub fn range_contains_native(_vm: &mut VM, this: &mut Value, args: &mut [&mut Value]) -> Result<Value, String> {
    let arg = &args[0];
    let range = this.as_range(&_vm.heap);
    
    let value = match arg {
        Value::Number(crate::value::Number::Integer(n)) => {
            match n {
                crate::value::GenericInt::Small(val) => *val,
                _ => return Ok(false.into()),
            }
        }
        _ => return Ok(false.into()),
    };

    Ok(range.contains(value).into())
}

/// Get the length of a range.
pub fn range_len_native(_vm: &mut VM, this: &mut Value, _args: &mut [&mut Value]) -> Result<Value, String> {
    let range = this.as_range(&_vm.heap);
    Ok((range.len() as i64).into())
}

/// Create an iterator for the range.
pub fn range_iter_native(_vm: &mut VM, this: &mut Value, _args: &mut [&mut Value]) -> Result<Value, String> {
    // For now, let's return the range itself as its own iterator
    // In a more complete implementation, we'd create a separate iterator class
    Ok(*this)
}

/// Get the next value from a range iterator.
/// This is a simple implementation where the range acts as its own iterator.
pub fn range_next_native(vm: &mut VM, this: &mut Value, _args: &mut [&mut Value]) -> Result<Value, String> {
    let range = this.as_range_mut(&mut vm.heap);
    
    if range.start > range.end || (range.start == range.end && !range.inclusive) {
        return Ok(Value::StopIteration);
    }
    
    let current = range.start;
    range.start += 1;
    
    Ok(current.into())
}