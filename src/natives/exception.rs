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

/// Get the stack trace of the Exception.
pub(super) fn exception_stack_trace_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    Ok(Value::String(receiver.as_exception(&vm.heap).stack_trace()))
}

/// Get the the full string representation of the Exception.
/// 
/// This will be inherited by all exceptions and properly display their name.
pub(super) fn exception_str_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let exception = receiver.as_exception(&vm.heap);
    let mut result = format!(
        "{}: {}\n",
        receiver
            .as_instance()
            .to_value(&vm.heap)
            .class
            .to_value(&vm.heap)
            .name
            .to_value(&vm.heap),
        exception.message().to_value(&vm.heap)
    );
    result.push_str(exception.stack_trace().to_value(&vm.heap));
    Ok(vm.heap.string_id(&result).into())
}
