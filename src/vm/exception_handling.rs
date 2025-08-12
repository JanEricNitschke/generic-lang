use super::InterpretResult;
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

    /// Create an exception instance and push it onto the stack.
    /// The caller should then call unwind() directly.
    pub(super) fn create_exception_instance(&mut self, exception_name: &str, message: &str) -> bool {
        let exception_class_id = self.heap.string_id(&exception_name);
        
        // Get the exception class from builtins
        if let Some(exception_global) = self.builtins.get(&exception_class_id) {
            if let Value::Class(class_id) = exception_global.value {
                // Create an instance of the exception class
                let mut instance = crate::value::Instance::new(Value::Class(class_id), None);
                
                // Set the message field
                let message_value = self.heap.string_id(&message);
                instance.fields.insert("message".to_string(), message_value.into());
                
                let instance_value = self.heap.add_instance(instance);
                
                // Push the exception instance onto the stack
                self.stack.push(instance_value);
                return true;
            }
        }
        
        false // Failed to create exception
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
        if let Some(handler) = self.pop_exception_handler() {
            self.callstack.truncate(handler.frames_to_keep, &self.heap);
            self.callstack.current_mut().ip = handler.ip;
            self.stack.truncate(handler.stack_length);
            self.stack.push(exception);
            None
        } else {
            // Extract the message from the exception instance if it has one
            let message = if let Value::Instance(instance_id) = exception {
                let instance = instance_id.to_value(&self.heap);
                if let Some(message_value) = instance.fields.get("message") {
                    message_value.to_string(&self.heap)
                } else {
                    exception.to_string(&self.heap)
                }
            } else {
                exception.to_string(&self.heap)
            };
            
            runtime_error!(
                self,
                "{}",
                message
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
