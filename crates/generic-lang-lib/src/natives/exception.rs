//! Native methods for Exception class.

use crate::vm::ExceptionKind::TypeError;
use crate::{
    value::{Exception, NativeClass, Value},
    vm::{VM, errors::VmResult},
};

/// Initialize an Exception with a message and current stack trace.
pub(super) fn exception_init_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let message = if args.is_empty() {
        None
    } else {
        match &args[0] {
            Value::String(string_id) => Some(*string_id),
            _ => {
                return Err(vm
                    .throw(TypeError, "Exception message must be a string")
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
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    match receiver.as_exception(&vm.heap).message() {
        Some(message) => Ok(Value::String(message)),
        None => Ok(Value::Nil),
    }
}

/// Get the stack trace of the Exception.
pub(super) fn exception_stack_trace_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    match receiver.as_exception(&vm.heap).stack_trace() {
        Some(stack_trace) => Ok(Value::String(stack_trace)),
        None => Ok(Value::Nil),
    }
}

/// Get the string representation of the Exception: its message, or an empty
/// string if it has none — like Python's `BaseException.__str__`. The class
/// name and stack trace are added by the fatal-error display in `unwind`,
/// not by `__str__`.
pub(super) fn exception_str_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    let result = match receiver.as_exception(&vm.heap).message() {
        Some(message) => message,
        None => vm.heap.string_id(&""),
    };
    Ok(result.into())
}
