use super::InterpretResult;
use crate::chunk::CodeOffset;
use crate::value::{Value, is_exception_subclass, is_subclass_of};
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

    pub(super) fn unwind(&mut self, exception: Value) -> Option<InterpretResult> {
        if !matches!(exception, Value::Instance(_)) {
            runtime_error!(
                self,
                "Can only throw instances, got: {}",
                exception.to_string(&self.heap)
            );
            return Some(InterpretResult::RuntimeError);
        }

        // Check that the exception is an instance of Exception or its subclasses
        let exception_class_id = exception.as_instance().to_value(&self.heap).class;

        // Only allow throwing instances of Exception or its subclasses
        if !is_exception_subclass(&self.heap, exception_class_id) {
            runtime_error!(
                self,
                "Can only throw instances of Exception or its subclasses, got instance of: {}",
                exception_class_id
                    .to_value(&self.heap)
                    .name
                    .to_value(&self.heap)
            );
            return Some(InterpretResult::RuntimeError);
        }

        if let Some(handler) = self.pop_exception_handler() {
            self.callstack.truncate(handler.frames_to_keep, &self.heap);
            self.callstack.current_mut().ip = handler.ip;
            self.stack.truncate(handler.stack_length);
            self.stack.push(exception);
            None
        } else {
            runtime_error!(
                self,
                "{}",
                self.value_to_string(&exception).to_string(&self.heap)
            );
            Some(InterpretResult::RuntimeError)
        }
    }

    pub(super) fn reraise_exception(&mut self) -> Option<InterpretResult> {
        match self.stack.pop().expect("Stack underflow in OP_RERAISE") {
            Value::Nil => None,
            exception => self.unwind(exception),
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
            // Check that the class to catch is a subclass of Exception
            if !is_exception_subclass(&self.heap, class_id) {
                runtime_error!(
                    self,
                    "Can only catch Exception or its subclasses, got: {}",
                    class_id.to_value(&self.heap).name.to_value(&self.heap)
                );
                return Some(InterpretResult::RuntimeError);
            }

            let exception_class_id = exception_value.as_instance().to_value(&self.heap).class;
            // Check if the exception class is the same as or a subclass of the catch class
            self.stack.push(Value::Bool(is_subclass_of(
                &self.heap,
                exception_class_id,
                class_id,
            )));
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
