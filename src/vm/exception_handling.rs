use super::RuntimeError;
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

    pub(super) fn unwind(&mut self, exception: Value) -> Result<(), RuntimeError> {
        if !matches!(exception, Value::Instance(_)) {
            runtime_error!(
                self,
                "Can only throw instances, got: {}",
                exception.to_string(&self.heap)
            );
            return Err(RuntimeError::new(format!(
                "Can only throw instances, got: {}",
                exception.to_string(&self.heap)
            )));
        }
        if let Some(handler) = self.pop_exception_handler() {
            self.callstack.truncate(handler.frames_to_keep, &self.heap);
            self.callstack.current_mut().ip = handler.ip;
            self.stack.truncate(handler.stack_length);
            self.stack.push(exception);
            Ok(())
        } else {
            runtime_error!(
                self,
                "{}",
                self.value_to_string(&exception).to_string(&self.heap)
            );
            Err(RuntimeError::new(
                self.value_to_string(&exception).to_string(&self.heap),
            ))
        }
    }

    pub(super) fn reraise_exception(&mut self) -> Result<(), RuntimeError> {
        match self.stack.pop().expect("Stack underflow in OP_RERAISE") {
            Value::Nil => Ok(()),
            exception => self.unwind(exception),
        }
    }

    ///Layout is Stack Top: [`exception_class_to_catch`, `exception_value_raised`]
    pub(super) fn compare_exception(&mut self) -> Result<(), RuntimeError> {
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
            Ok(())
        } else {
            runtime_error!(
                self,
                "Exception to catch must be a class, got: {}",
                class_to_catch.to_string(&self.heap)
            );
            Err(RuntimeError::new(format!(
                "Exception to catch must be a class, got: {}",
                class_to_catch.to_string(&self.heap)
            )))
        }
    }
}
