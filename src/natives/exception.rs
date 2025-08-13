//! Native methods for Exception class.

use crate::{
    value::{NativeClass, Value},
    vm::{VM, errors::VmError},
};

/// Initialize an Exception with a message and current stack trace.
pub(super) fn exception_init_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmError<Value> {
    let message = if args.is_empty() {
        None
    } else {
        match &args[0] {
            Value::String(string_id) => Some(*string_id),
            _ => {
                return Err(vm
                    .throw_type_error("Exception message must be a string")
                    .unwrap_err());
            }
        }
    };

    // Use the utility function to create the exception data with stack trace
    let exception_data = vm.create_exception_data(message);

    if let Value::Instance(instance) = receiver {
        instance.to_value_mut(&mut vm.heap).backing = Some(NativeClass::Exception(exception_data));
    }

    // Return the modified instance
    Ok(*receiver)
}

/// Get the message of the Exception.
pub(super) fn exception_message_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> VmError<Value> {
    match receiver.as_exception(&vm.heap).message() {
        Some(message) => Ok(Value::String(message)),
        None => Ok(Value::Nil),
    }
}

/// Get the stack trace of the Exception.
pub(super) fn exception_stack_trace_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> VmError<Value> {
    Ok(Value::String(receiver.as_exception(&vm.heap).stack_trace()))
}

/// Get the the full string representation of the Exception.
///
/// This will be inherited by all exceptions and properly display their name.
pub(super) fn exception_str_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> VmError<Value> {
    let exception = receiver.as_exception(&vm.heap);
    let class_name = receiver
        .as_instance()
        .to_value(&vm.heap)
        .class
        .to_value(&vm.heap)
        .name
        .to_value(&vm.heap);
    let mut result = match exception.message() {
        Some(message) => format!("{class_name}: {}\n", message.to_value(&vm.heap)),
        None => format!("{class_name}\n"),
    };
    result.push_str(exception.stack_trace().to_value(&vm.heap));
    Ok(vm.heap.string_id(&result).into())
}
