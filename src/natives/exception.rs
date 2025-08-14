//! Native methods for Exception class.

use crate::{
    value::{Exception, NativeClass, Value},
    vm::VM,
};

/// Initialize an Exception with a message and current stack trace.
pub(super) fn exception_init_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let message = match &args[0] {
        Value::String(string_id) => *string_id,
        _ => return Err("Exception message must be a string".to_string()),
    };

    let stack_trace = vm.capture_stack_trace();

    let exception = Exception::new(message, vm.heap.string_id(&stack_trace));

    if let Value::Instance(instance) = receiver {
        instance.to_value_mut(&mut vm.heap).backing = Some(NativeClass::Exception(exception));
    }

    // Return the modified instance
    Ok(*receiver)
}

/// Get the message of the Exception.
pub(super) fn exception_message_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    Ok(Value::String(receiver.as_exception(&vm.heap).message()))
}
