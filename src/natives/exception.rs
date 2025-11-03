//! Native methods for Exception class.

use crate::{
    value::{Exception, NativeClass, Value},
    vm::{VM, errors::VmResult},
};

/// Initialize an Exception with a message and current stack trace.
pub(super) fn exception_init_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmResult<Value> {
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
    let exception_data = Exception::new(message);

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
) -> VmResult<Value> {
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
) -> VmResult<Value> {
    match receiver.as_exception(&vm.heap).stack_trace() {
        Some(stack_trace) => Ok(Value::String(stack_trace)),
        None => Ok(Value::Nil),
    }
}

/// Get the the full string representation of the Exception.
///
/// This will be inherited by all exceptions and properly display their name.
pub(super) fn exception_str_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> VmResult<Value> {
    let exception = receiver.as_exception(&vm.heap);
    let class_name = receiver
        .as_instance()
        .to_value(&vm.heap)
        .class
        .to_value(&vm.heap)
        .name
        .to_value(&vm.heap);
    let mut result = match exception.message() {
        Some(message) => format!("{class_name}: {}", message.to_value(&vm.heap)),
        None => class_name.clone(),
    };
    if let Some(stack_trace) = exception.stack_trace() {
        result.push('\n');
        result.push_str(stack_trace.to_value(&vm.heap));
    }
    Ok(vm.heap.string_id(&result).into())
}
