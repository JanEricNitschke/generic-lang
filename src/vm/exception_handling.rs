use super::{InterpretResult, VmError, VmResult};
use crate::chunk::CodeOffset;
use crate::value::Value;
use crate::vm::VM;
/// An exception handlers
///
/// Holds the index of the frame where the exception handler is located
/// and the instruction pointer (ip) where the exception handler starts.
#[derive(Debug)]
pub(super) struct ExceptionHandler {
    pub(super) frames_to_keep: usize,
    pub(super) ip: usize,
    pub(super) stack_length: usize,
}

impl VM {
    pub(super) fn register_exception_handler(
        &mut self,
        frames_to_keep: usize,
        ip: usize,
        stack_length: usize,
    ) {
        self.exception_handlers.push(ExceptionHandler {
            frames_to_keep,
            ip,
            stack_length,
        });
    }

    pub(super) fn pop_exception_handler(&mut self) -> Option<ExceptionHandler> {
        self.exception_handlers.pop()
    }

    pub(super) fn unwind(&mut self, exception: Value) -> VmResult {
        if !matches!(exception, Value::Instance(_)) {
            runtime_error!(
                self,
                "Can only throw instances, got: {}",
                exception.to_string(&self.heap)
            );
            return Err(VmError::Hard);
        }
        if let Some(handler) = self.pop_exception_handler() {
            self.callstack.truncate(handler.frames_to_keep, &self.heap);
            self.callstack.current_mut().ip = handler.ip;
            self.stack.truncate(handler.stack_length);
            self.stack.push(exception);
            Ok(())
        } else {
            // Keep the exception on the stack for differentiation
            self.stack.push(exception);
            Err(VmError::Exception)
        }
    }

    /// Legacy wrapper for unwind that returns Option<InterpretResult>
    pub(super) fn unwind_legacy(&mut self, exception: Value) -> Option<InterpretResult> {
        match self.unwind(exception) {
            Ok(()) => None,
            Err(VmError::Hard) => Some(InterpretResult::RuntimeError),
            Err(VmError::Exception) => {
                // For legacy compatibility, print the error and return RuntimeError
                let exception = self.stack.last().expect("Exception should be on stack");
                runtime_error!(
                    self,
                    "Uncaught exception: {}",
                    exception.to_string(&self.heap)
                );
                Some(InterpretResult::RuntimeError)
            }
        }
    }

    pub(super) fn reraise_exception(&mut self) -> VmResult {
        match self.stack.pop().expect("Stack underflow in OP_RERAISE") {
            Value::Nil => Ok(()),
            exception => self.unwind(exception),
        }
    }

    /// Legacy wrapper for reraise_exception that returns Option<InterpretResult>
    pub(super) fn reraise_exception_legacy(&mut self) -> Option<InterpretResult> {
        match self.reraise_exception() {
            Ok(()) => None,
            Err(VmError::Hard) => Some(InterpretResult::RuntimeError),
            Err(VmError::Exception) => {
                // For legacy compatibility, print the error and return RuntimeError
                let exception = self.stack.last().expect("Exception should be on stack");
                runtime_error!(
                    self,
                    "Uncaught exception: {}",
                    exception.to_string(&self.heap)
                );
                Some(InterpretResult::RuntimeError)
            }
        }
    }

    ///Layout is Stack Top: [`exception_class_to_catch`, `exception_value_raised`]
    pub(super) fn compare_exception(&mut self) -> Option<InterpretResult> {
        let class_to_catch = self
            .stack
            .pop()
            .expect("Stack underflow in OP_COMPARE_EXCEPTION");
        let exception_value = self
            .peek(0)
            .expect("Stack underflow in OP_COMPARE_EXCEPTION");
        if let Value::Class(class_id) = class_to_catch {
            let exception_instance = exception_value.as_instance();
            self.stack.push(Value::Bool(
                exception_instance.to_value(&self.heap).class == class_id,
            ));
            None
        } else {
            runtime_error!(
                self,
                "Exception to catch must be a class, got: {}",
                class_to_catch.to_string(&self.heap)
            );
            Some(InterpretResult::RuntimeError)
        }
    }
}
