use super::InterpretResult;
use crate::chunk::CodeOffset;
use crate::value::{
    Exception, Instance, NativeClass, Value, is_exception_subclass, is_subclass_of,
};
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
        self.handling_exception = true;
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
            let exception_str = self
                .value_to_string(&exception)
                .unwrap_or_else(|_| Value::String(self.heap.string_id(&"<unprintable exception>")));
            runtime_error!(self, "{}", exception_str.to_string(&self.heap));
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

    /// Create a new exception instance of the specified type with a message.
    ///
    /// This helper method creates exception instances that can be thrown and caught.
    /// The exception will include the current stack trace.
    ///
    /// # Panics
    ///
    /// Panics if the exception type is not found instead of falling back to a general exception.
    pub(super) fn create_exception(&mut self, exception_type: &str, message: &str) -> Value {
        // First try to get from native classes
        let exception_class = if let Some(class) = self.heap.native_classes.get(exception_type) {
            *class
        } else {
            // Try to get from builtins
            let exception_type_id = self.heap.string_id(&exception_type.to_string());
            if let Some(builtin) = self.builtins.get(&exception_type_id) {
                builtin.value
            } else {
                // Panic instead of falling back to Exception
                panic!(
                    "Exception type '{}' not found in native classes or builtins",
                    exception_type
                );
            }
        };

        self.create_exception_with_class(exception_class, message)
    }

    /// Create a new exception instance with a specific class and message.
    ///
    /// This utility function handles the common logic for creating exceptions with stack traces.
    pub(super) fn create_exception_with_class(
        &mut self,
        exception_class: Value,
        message: &str,
    ) -> Value {
        let exception_data = self.create_exception_data(message);

        let instance = Instance::new(
            exception_class,
            Some(NativeClass::Exception(exception_data)),
        );

        self.heap.add_instance(instance)
    }

    /// Create exception data with stack trace.
    /// 
    /// This utility function extracts the common logic for creating exception data
    /// to avoid duplication between exception creation and __init__ method.
    pub fn create_exception_data(&mut self, message: &str) -> Exception {
        let message_id = self.heap.string_id(&message.to_string());
        let stack_trace = self.capture_stack_trace();
        let stack_trace_id = self.heap.string_id(&stack_trace);
        
        Exception::new(message_id, stack_trace_id)
    }

    /// Create and throw a TypeError with the given message.
    pub(super) fn throw_type_error(&mut self, message: &str) -> Option<InterpretResult> {
        let exception = self.create_exception("TypeError", message);
        self.unwind(exception)
    }

    /// Create and throw a ValueError with the given message.
    pub(super) fn throw_value_error(&mut self, message: &str) -> Option<InterpretResult> {
        let exception = self.create_exception("ValueError", message);
        self.unwind(exception)
    }

    /// Create and throw a NameError with the given message.
    pub(super) fn throw_name_error(&mut self, message: &str) -> Option<InterpretResult> {
        let exception = self.create_exception("NameError", message);
        self.unwind(exception)
    }

    /// Create and throw a ConstReassignmentError with the given message.
    pub(super) fn throw_const_reassignment_error(
        &mut self,
        message: &str,
    ) -> Option<InterpretResult> {
        let exception = self.create_exception("ConstReassignmentError", message);
        self.unwind(exception)
    }

    /// Create and throw an AttributeError with the given message.
    pub(super) fn throw_attribute_error(&mut self, message: &str) -> Option<InterpretResult> {
        let exception = self.create_exception("AttributeError", message);
        self.unwind(exception)
    }

    /// Create and throw an ImportError with the given message.
    pub(super) fn throw_import_error(&mut self, message: &str) -> Option<InterpretResult> {
        let exception = self.create_exception("ImportError", message);
        self.unwind(exception)
    }

    /// Create and throw an ArithmeticError with the given message.
    #[allow(dead_code)]
    pub(super) fn throw_arithmetic_error(&mut self, message: &str) -> Option<InterpretResult> {
        let exception = self.create_exception("ArithmeticError", message);
        self.unwind(exception)
    }

    /// Create and throw an IndexError with the given message.
    #[allow(dead_code)]
    pub(super) fn throw_index_error(&mut self, message: &str) -> Option<InterpretResult> {
        let exception = self.create_exception("IndexError", message);
        self.unwind(exception)
    }

    /// Create and throw a RuntimeError with the given message.
    pub(super) fn throw_runtime_error(&mut self, message: &str) -> Option<InterpretResult> {
        let exception = self.create_exception("Exception", message);
        self.unwind(exception)
    }
}
