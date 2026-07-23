use crate::heap::{ClassId, StringId};
use crate::value::{
    Exception, Instance, NativeClass, Value, is_exception_subclass, is_subclass_of,
};
use crate::vm::VM;
use crate::vm::errors::{ExceptionRaisedKind, RuntimeErrorKind, VmErrorKind, VmResult};
use strum_macros::{EnumIter, IntoStaticStr};

use self::ExceptionKind::TypeError;
/// The kinds of exceptions the VM can throw. Each variant is named exactly
/// like the builtin exception class it maps to (the mapping goes through
/// the name - see `every_exception_kind_has_a_builtin_class`).
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter, IntoStaticStr)]
pub enum ExceptionKind {
    /// The base exception class.
    Exception = 1,
    TypeError,
    ValueError,
    NameError,
    ConstReassignmentError,
    AttributeError,
    ImportError,
    AssertionError,
    IoError,
    KeyError,
    IndexError,
    RuntimeError,
}

/// A snapshot of the three lengths that define a callstack region: the
/// callstack depth, the value-stack height, and the module count.
///
/// Captured when a region is entered (or a handler registered) and consumed
/// by [`VM::unwind_region`], which cuts the VM back to it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct RegionSnapshot {
    pub frames: usize,
    pub stack: usize,
    pub modules: usize,
}

/// An exception handler.
///
/// Holds the region to unwind to when the handler catches - absolute
/// positions in the live VM - and the instruction pointer (chunk-relative)
/// where the catch block starts. For the generator-relative form saved
/// across suspensions see [`SuspendedExceptionHandler`].
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ExceptionHandler {
    pub region: RegionSnapshot,
    pub ip: usize,
}

/// An exception handler saved inside a suspended generator.
///
/// A generator can be resumed at a different callstack depth, value-stack
/// height, and module count than the ones it was suspended at, so the
/// absolute indices of an [`ExceptionHandler`] would go stale across a
/// suspension. This form anchors them to the generator frame instead:
///
/// - `frames_above_base`: frames relative to the callstack *below* the
///   generator frame (always 1 today - the generator frame itself).
/// - `stack_length`: relative to the generator frame's `stack_base`.
/// - `modules_above_base`: relative to the module count at suspension
///   (normally 0 - imports cannot yield, so a resume is module-balanced).
/// - `ip`: chunk-relative, identical in both forms.
///
/// The fields are private: the only way in or out is
/// [`ExceptionHandler::suspend`] / [`Self::resume`], so a suspended handler
/// can never be used as an absolute one by accident.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SuspendedExceptionHandler {
    frames_above_base: usize,
    ip: usize,
    stack_length: usize,
    modules_above_base: usize,
}

impl ExceptionHandler {
    /// Rebase from VM-absolute to generator-relative form on suspension.
    ///
    /// The bases describe the VM at the moment the generator frame was
    /// popped: `frame_base`/`module_base` are the remaining callstack and
    /// module counts, `stack_base` is the generator frame's `stack_base`.
    pub(crate) fn suspend(
        self,
        frame_base: usize,
        stack_base: usize,
        module_base: usize,
    ) -> SuspendedExceptionHandler {
        debug_assert!(
            self.region.frames > frame_base,
            "suspending a handler that does not belong to the generator frame"
        );
        debug_assert!(
            self.region.stack >= stack_base && self.region.modules >= module_base,
            "suspending a handler registered below the generator frame"
        );
        SuspendedExceptionHandler {
            frames_above_base: self.region.frames - frame_base,
            ip: self.ip,
            stack_length: self.region.stack - stack_base,
            modules_above_base: self.region.modules - module_base,
        }
    }
}

impl SuspendedExceptionHandler {
    /// Rebase back to VM-absolute form while the generator is re-planted.
    ///
    /// The bases describe the VM of the new resume: `frame_base`/
    /// `module_base` are the callstack and module counts *below* the
    /// generator frame, `stack_base` is the frame's rebased `stack_base`.
    pub(crate) fn resume(
        self,
        frame_base: usize,
        stack_base: usize,
        module_base: usize,
    ) -> ExceptionHandler {
        ExceptionHandler {
            region: RegionSnapshot {
                frames: self.frames_above_base + frame_base,
                stack: self.stack_length + stack_base,
                modules: self.modules_above_base + module_base,
            },
            ip: self.ip,
        }
    }
}

impl VM {
    /// The current callstack depth, value-stack height, and module count -
    /// the state a later [`VM::unwind_region`] restores.
    pub(crate) fn current_region(&self) -> RegionSnapshot {
        RegionSnapshot {
            frames: self.callstack.len(),
            stack: self.stack.len(),
            modules: self.modules.len(),
        }
    }

    /// Cut the value stack, callstack, and module stack back to `region`,
    /// closing upvalues over the removed stack slots exactly like a normal
    /// return would - a closure that escaped the region must keep seeing
    /// the captured values, and a stale open upvalue would index out of
    /// bounds (or read a reused slot) later.
    pub(crate) fn unwind_region(&mut self, region: RegionSnapshot) {
        self.close_upvalue(region.stack);
        self.stack.truncate(region.stack);
        self.callstack.truncate(region.frames, &self.heap);
        self.modules.truncate(region.modules);
    }

    /// Register a handler for the current region whose catch block starts
    /// at the chunk-relative `ip`.
    pub(super) fn register_exception_handler(&mut self, ip: usize) {
        self.exception_handlers.push(ExceptionHandler {
            region: self.current_region(),
            ip,
        });
    }

    pub(super) fn pop_exception_handler(&mut self) -> Option<ExceptionHandler> {
        self.exception_handlers.pop()
    }

    /// Drain the handlers belonging to a just-popped generator frame and
    /// rebase them to their suspended (generator-relative) form.
    ///
    /// Must be called right after the generator frame was popped from the
    /// callstack; `stack_base` is that frame's `stack_base`. All handlers
    /// above the remaining callstack belong to the generator frame: any
    /// deeper frame must have returned before the `yield`, and the compiler
    /// pops handlers before returns.
    pub(crate) fn suspend_handlers_above(
        &mut self,
        stack_base: usize,
    ) -> Vec<SuspendedExceptionHandler> {
        let frame_base = self.callstack.len();
        let module_base = self.modules.len();
        let split = self
            .exception_handlers
            .iter()
            .position(|handler| handler.region.frames > frame_base)
            .unwrap_or(self.exception_handlers.len());
        self.exception_handlers
            .drain(split..)
            .map(|handler| handler.suspend(frame_base, stack_base, module_base))
            .collect()
    }

    /// Splice a suspended generator's handlers back onto the live handler
    /// stack, rebased to the current VM.
    ///
    /// `frame_base` is the callstack depth *below* the generator frame and
    /// `stack_base` the frame's rebased `stack_base` for this resume.
    pub(crate) fn resume_suspended_handlers(
        &mut self,
        handlers: Vec<SuspendedExceptionHandler>,
        frame_base: usize,
        stack_base: usize,
    ) {
        let module_base = self.modules.len();
        self.exception_handlers.extend(
            handlers
                .into_iter()
                .map(|handler| handler.resume(frame_base, stack_base, module_base)),
        );
    }

    /// Mark the value on the stack top as a thrown, in-flight exception.
    ///
    /// Validates it and attaches the stack trace - captured HERE, at the
    /// throw site, while the throwing frames are still intact. No unwinding
    /// happens: the exception stays on the stack (rooted for the GC) until a
    /// dispatch loop resolves it against a handler, or a native along the
    /// propagation path pops it and handles it.
    pub(crate) fn raise_pending_from_stack(&mut self) -> VmResult {
        let exception = *self.stack.last().expect("Raise with empty stack");
        if !matches!(exception, Value::Instance(_)) {
            self.stack.pop();
            return self.throw(
                TypeError,
                &format!(
                    "Can only throw instances, got: {}",
                    exception.to_string(&self.heap)
                ),
            );
        }

        // Only allow throwing instances of Exception or its subclasses
        let exception_class_id = exception.as_instance().to_value(&self.heap).class;
        if !is_exception_subclass(&self.heap, exception_class_id) {
            self.stack.pop();
            return self.throw(
                TypeError,
                &format!(
                    "Can only throw instances of Exception or its subclasses, got instance of: {}",
                    exception_class_id
                        .to_value(&self.heap)
                        .name
                        .to_value(&self.heap)
                ),
            );
        }

        let stack_trace = self.capture_stack_trace();
        let stack_trace_id = self.heap.string_id(&stack_trace);
        let exception_data = exception.as_exception_mut(&mut self.heap);
        exception_data.stack_trace.get_or_insert(stack_trace_id);

        Err(VmErrorKind::Exception(ExceptionRaisedKind))
    }

    /// Route the pending exception (on the stack top) to the innermost
    /// handler, IF that handler lies within the callstack region rooted at
    /// `call_depth`. This is the only place control ever transfers: the
    /// handler is popped, modules/callstack/stack are truncated to its
    /// snapshot, the ip is set to the catch block, and the exception is
    /// re-delivered on the stack for `OP_COMPARE_EXCEPTION`.
    ///
    /// Returns `false` if the innermost handler (if any) belongs to an outer
    /// region: the pending exception then escapes to whoever entered the
    /// region - a native caller, which may pop and handle it or propagate.
    /// Pass `0` for `call_depth` to accept any handler (the top-level loop).
    pub(crate) fn resolve_pending_exception(&mut self, call_depth: usize) -> bool {
        match self.exception_handlers.last() {
            Some(handler) if handler.region.frames >= call_depth => {
                let handler = self
                    .pop_exception_handler()
                    .expect("Handler vanished during resolution");
                let exception = self.stack.pop().expect("Pending exception missing");
                self.unwind_region(handler.region);
                self.callstack.current_mut().ip = handler.ip;
                self.stack.push(exception);
                true
            }
            _ => false,
        }
    }

    /// Report a pending exception that no handler anywhere catches, and
    /// convert it into a fatal runtime error.
    ///
    /// Composes the display as `ClassName: {str(e)}` (bare class name for
    /// an empty str) plus the stored stack trace. `__str__` returns only the
    /// message, and user overrides run here too - if str(e) raises, its
    /// pending exception is discarded and `<exception str() failed>` is
    /// printed instead.
    pub(super) fn report_uncaught_exception(&mut self) -> RuntimeErrorKind {
        let exception = *self
            .stack
            .last()
            .expect("Uncaught exception missing from the stack");

        let message = match self.value_to_string(&exception) {
            Ok(id) => id.to_value(&self.heap).clone(),
            Err(VmErrorKind::Exception(_)) => {
                // Discard the pending exception the failing `__str__` left.
                self.stack.pop();
                "<exception str() failed>".to_string()
            }
            Err(VmErrorKind::Runtime(_)) => "<exception str() failed>".to_string(),
        };

        let class_name = exception
            .as_instance()
            .to_value(&self.heap)
            .class
            .to_value(&self.heap)
            .name
            .to_value(&self.heap);
        let mut display = if message.is_empty() {
            class_name.clone()
        } else {
            format!("{class_name}: {message}")
        };
        if let Some(stack_trace) = exception.as_exception(&self.heap).stack_trace() {
            display.push('\n');
            display.push_str(stack_trace.to_value(&self.heap));
        }
        runtime_error!("{}", display);
        self.stack.pop();
        RuntimeErrorKind
    }

    pub(super) fn reraise_exception(&mut self) -> VmResult {
        match self.stack.last().expect("Stack underflow in OP_RERAISE") {
            Value::Nil => {
                self.stack.pop();
                Ok(None)
            }
            _ => self.raise_pending_from_stack(),
        }
    }

    ///Layout is Stack Top: [`exception_class_to_catch`, `exception_value_raised`]
    pub(super) fn compare_exception(&mut self) -> VmResult {
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
                return self.throw(
                    TypeError,
                    &format!(
                        "Can only catch Exception or its subclasses, got: {}",
                        class_id.to_value(&self.heap).name.to_value(&self.heap)
                    ),
                );
            }

            let exception_class_id = exception_value.as_instance().to_value(&self.heap).class;
            // Check if the exception class is the same as or a subclass of the catch class
            self.stack.push(Value::Bool(is_subclass_of(
                &self.heap,
                exception_class_id,
                class_id,
            )));
            Ok(None)
        } else {
            self.throw(
                TypeError,
                &format!(
                    "Exception to catch must be a class, got: {}",
                    class_to_catch.to_string(&self.heap)
                ),
            )
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
        let exception_class =
            if let Some(class) = self.heap.native_classes.get(exception_type) {
                *class
            } else {
                // Try to get from builtins
                let exception_type_id = self.heap.string_id(&exception_type.to_string());
                *self.builtins
                .get(&exception_type_id)
                .unwrap_or_else(|| {
                    panic!(
                        "Exception type '{exception_type}' not found in native classes or builtins"
                    )
                })
                .value.as_class()
            };

        let message_id = self.heap.string_id(&message);
        self.create_exception_with_class(exception_class, Some(message_id))
    }

    /// Create a new exception instance with a specific class and message.
    ///
    /// This utility function handles the common logic for creating exceptions with stack traces.
    pub(super) fn create_exception_with_class(
        &mut self,
        exception_class: ClassId,
        message_id: Option<StringId>,
    ) -> Value {
        let exception_data = Exception::new(message_id);

        let instance = Instance::new(
            exception_class,
            Some(NativeClass::Exception(exception_data)),
        );

        self.heap.add_instance(instance)
    }

    /// Create and throw an exception of the given kind with the given message.
    ///
    /// The exception is left pending on the stack top (see
    /// [`Self::raise_pending_from_stack`]); no unwinding happens here.
    /// Callers propagate the returned error with `?`, or pop the exception
    /// to handle it.
    pub(crate) fn throw(&mut self, kind: ExceptionKind, message: &str) -> VmResult {
        let exception = self.create_exception(kind.into(), message);
        self.stack.push(exception);
        self.raise_pending_from_stack()
    }

    /// Validate that `class` is a class deriving from `Exception`, throwing
    /// a `TypeError` otherwise.
    ///
    /// Used by `generator.raise(...)`: validation runs in the caller's
    /// context, so an unusable argument never touches the generator.
    pub(crate) fn validate_exception_class(&mut self, class: Value) -> VmResult<ClassId> {
        let Value::Class(class_id) = class else {
            return Err(self
                .throw(
                    TypeError,
                    &format!(
                        "Exception to throw must be a class, got: {}",
                        class.to_string(&self.heap)
                    ),
                )
                .unwrap_err());
        };
        if !is_exception_subclass(&self.heap, class_id) {
            return Err(self
                .throw(
                    TypeError,
                    &format!(
                        "Can only throw Exception or its subclasses, got: {}",
                        class_id.to_value(&self.heap).name.to_value(&self.heap)
                    ),
                )
                .unwrap_err());
        }
        Ok(class_id)
    }

    /// Build a new instance of the (already validated) exception class,
    /// running its `__init__` (with no arguments) to completion.
    ///
    /// A throwing `__init__` escapes as a pending exception with its trace
    /// anchored at the current position - `generator.raise(...)` relies on
    /// this to deliver constructor errors at the suspension point.
    pub(crate) fn instantiate_exception(&mut self, class_id: ClassId) -> VmResult<Value> {
        let init_method_id = self.heap.builtin_constants().init_string;
        class_id
            .to_value(&self.heap)
            .methods
            .get(&init_method_id)
            .expect("Exception classes always define __init__");
        let instance = self
            .heap
            .add_instance(Instance::new(class_id, Some(NativeClass::new("Exception"))));
        self.stack.push(instance);
        self.invoke_and_run_function(init_method_id, 0)?;
        Ok(self
            .stack
            .pop()
            .expect("Stack underflow building a raised exception"))
    }
}

#[cfg(test)]
mod tests {
    use super::{ExceptionKind, VM};
    use crate::value::is_exception_subclass;
    use strum::IntoEnumIterator;

    /// Every variant resolves to a builtin exception class: `throw` looks
    /// the class up by the variant's name, and `create_exception` panics
    /// when it is missing (e.g. after a rename in `exceptions.gen` or in
    /// the enum). Pin that for every kind instead of relying on whichever
    /// `.gen` tests happen to throw it - and check the resolved class
    /// actually derives from `Exception`, which the lookup alone does not
    /// guarantee.
    #[test]
    fn every_exception_kind_has_a_builtin_class() {
        let mut vm = VM::new();

        for kind in ExceptionKind::iter() {
            let exception = vm.create_exception(kind.into(), "sync");
            let class_id = exception.as_instance().to_value(&vm.heap).class;
            assert!(
                is_exception_subclass(&vm.heap, class_id),
                "{kind:?} must derive from Exception"
            );
        }
    }
}
