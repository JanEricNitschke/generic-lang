//! The vm module contains the main struct for the virtual machine and heart of the interpreter.
//!
//! The VM orchestrates the scanning to tokens, parsing of the tokens and creation of bytecode,
//! as well as the actual execution of the bytecode.

#[macro_use]
mod runtime_error;
mod dunder;
pub mod errors;
mod garbage_collection;
mod import;
mod setup;
mod stack;

#[macro_use]
mod arithmetics;
#[macro_use]
mod run_instruction;
mod bytecode;
mod callstack;
mod exception_handling;
mod functions;
mod native_containers;
mod state;
mod variables;

use arithmetics::IntoResultValue;
use callstack::CallStack;
use errors::RuntimeError;
use errors::{Return, RuntimeErrorKind, VmErrorKind};
use exception_handling::ExceptionHandler;

use rustc_hash::FxHashMap as HashMap;
use std::collections::VecDeque;
use std::path::PathBuf;

#[cfg(feature = "trace_execution")]
use crate::chunk::InstructionDisassembler;
use crate::config::GENERIC_BUILTINS_DIR;
use crate::natives;
use crate::vm::errors::VmError;
use crate::{
    chunk::{CodeOffset, OpCode},
    compiler::Compiler,
    heap::{Heap, ModuleId, StringId, UpvalueId},
    scanner::Scanner,
    stdlib,
    types::{EqualityMode, JumpCondition, NumberEncoding, RangeType},
    value::{Class, Closure, Function, ModuleContents, Number, Upvalue, Value},
};
use std::fmt::Write;

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl From<RuntimeError> for InterpretResult {
    fn from(result: RuntimeError) -> Self {
        match result {
            Ok(()) => Self::Ok,
            Err(_) => Self::RuntimeError,
        }
    }
}

/// Wrapper around a global value to store whether it is mutable or not.
#[derive(Debug, Clone, Copy)] // , PartialEq, Eq, PartialOrd
pub struct Global {
    pub(super) value: Value,
    mutable: bool,
}

impl Global {
    #[allow(dead_code)]
    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "Value: {}, mutable: {}",
            self.value.to_string(heap),
            self.mutable
        )
    }
}

impl std::fmt::Display for Global {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Value: {:?}, mutable: {}", self.value, self.mutable)
    }
}

/// The main struct for the virtual machine and heart of the interpreter.
///
/// Contains the heap, stack, callstack, open upvalues, modules, builtins, and stdlib.
pub struct VM {
    pub(super) heap: Heap,
    pub(super) stack: Vec<Value>,
    callstack: CallStack,
    exception_handlers: Vec<ExceptionHandler>,
    open_upvalues: VecDeque<UpvalueId>,
    // Could also keep a cache of the last module or its globals for performance
    modules: Vec<ModuleId>,
    // This could also just be passed to the interperet function.
    path: PathBuf,
    builtins: HashMap<StringId, Global>,
    stdlib: HashMap<StringId, ModuleContents>,
    pub(crate) handling_exception: bool,
    pub(crate) encountered_hard_exception: bool,
}

// Core functionality for running a script.
impl VM {
    #[must_use]
    pub(super) fn new(path: PathBuf) -> Self {
        Self {
            heap: Heap::new(),
            stack: Vec::with_capacity(crate::config::STACK_MAX),
            callstack: CallStack::new(),
            exception_handlers: Vec::new(),
            open_upvalues: VecDeque::new(),
            modules: Vec::new(),
            path,
            builtins: HashMap::default(),
            stdlib: HashMap::default(),
            handling_exception: false,
            encountered_hard_exception: false,
        }
    }

    /// Main interpret step for an input of bytes.
    ///
    /// Works by compiling the source to bytecode and then running it.
    /// Even the main script is compiled as a function.
    pub(super) fn interpret(&mut self, source: &[u8]) -> InterpretResult {
        // Load native functions and classes first
        natives::define(self);

        // Load generic builtins from .gen files after natives but before main script setup
        self.load_generic_builtins();

        // Register stdlib modules
        stdlib::register(self);

        let result = if let Some(function) = self.compile(
            source,
            "<script>",
            #[cfg(any(feature = "print_code", feature = "debug_scanner"))]
            false,
        ) {
            let function_id = self.heap.add_function(function);

            let closure = Closure::new(*function_id.as_function(), true, None, &self.heap);

            self.add_closure_to_modules(&closure, self.path.clone(), None, None, false);

            let value_id = self.heap.add_closure(closure);
            self.stack_push(value_id);
            self.execute_call(value_id, 0)
                .expect("Internal Error: Executing call of the main script failed.");

            self.run().into()
        } else {
            InterpretResult::CompileError
        };

        if result == InterpretResult::Ok {
            assert_eq!(self.stack.len(), 0);
        }
        result
    }

    fn compile(
        &mut self,
        source: &[u8],
        name: &str,
        #[cfg(any(feature = "print_code", feature = "debug_scanner"))] is_builtin: bool,
    ) -> Option<Function> {
        let scanner = Scanner::new(
            source,
            #[cfg(feature = "debug_scanner")]
            is_builtin,
        );
        let compiler = Compiler::new(
            scanner,
            &mut self.heap,
            name,
            #[cfg(feature = "print_code")]
            is_builtin,
        );
        compiler.compile()
    }

    /// Load and execute generic builtin files from the embedded `src/builtins` directory.
    ///
    /// Each builtin file is compiled and executed to completion. Then its globals
    /// (excluding `__name__`) are copied to the VM builtins and the module is
    /// popped from the modules stack.
    ///
    /// Panics if builtin loading fails, as this indicates an internal error.
    fn load_generic_builtins(&mut self) {
        // Iterate over embedded builtin files
        GENERIC_BUILTINS_DIR
            .files()
            .filter(|file| file.path().extension().is_some_and(|ext| ext == "gen"))
            .for_each(|file| {
                self.load_builtin_file_from_embedded(
                    file.path().to_string_lossy().as_ref(),
                    file.contents(),
                );
            });
    }

    /// Load and execute a single builtin file from embedded content in the current VM.
    fn load_builtin_file_from_embedded(&mut self, file_name: &str, source: &[u8]) {
        let name = format!("<builtin:{file_name}>");

        let function = self
            .compile(
                source,
                &name,
                #[cfg(any(feature = "print_code", feature = "debug_scanner"))]
                true,
            )
            .expect("Failed to compile builtin file");

        let function_id = self.heap.add_function(function);
        let closure = Closure::new(*function_id.as_function(), true, None, &self.heap);

        // Create a dummy path for the builtin file - this is only used for module tracking
        let builtin_path = PathBuf::from(format!("builtins/{file_name}"));
        self.add_closure_to_modules(&closure, builtin_path.clone(), None, None, false);

        let value_id = self.heap.add_closure(closure);
        self.stack_push(value_id);
        self.execute_call(value_id, 0)
            .expect("Internal Error: Executing call of builtin module failed.");

        // Execute the builtin to completion
        self.run().unwrap_or_else(|_| {
            panic!("Failed to execute builtin file {}.", builtin_path.display())
        });

        // Copy globals from the completed module to builtins (excluding __name__)
        let module_globals = std::mem::take(self.globals());
        // Extend builtins with all globals from the module
        self.builtins.extend(module_globals);
        // Remove __name__ from builtins
        self.builtins
            .remove(&self.heap.builtin_constants().script_name);

        // Clean up the builtin module
        self.modules.pop();
    }

    /// Infinite loop over the bytecode.
    ///
    /// Returns when a return instruction is hit at the top level.
    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    fn run(&mut self) -> RuntimeError {
        loop {
            if matches!(
                run_instruction!(self),
                Err(VmErrorKind::Runtime(RuntimeErrorKind))
            ) {
                return Err(RuntimeErrorKind);
            }
        }
    }

    /// Capture the current stack trace directly as a string.
    pub(super) fn capture_stack_trace(&self) -> String {
        let mut out = String::with_capacity(64 + self.callstack.len() * 40);

        out.push_str("Stacktrace (most recent call last):");

        for frame in self.callstack.iter() {
            let line = frame
                .closure(&self.heap)
                .function
                .to_value(&self.heap)
                .chunk
                .get_line(CodeOffset(frame.ip.saturating_sub(1)));

            let name = frame
                .closure(&self.heap)
                .function
                .to_value(&self.heap)
                .name
                .to_value(&self.heap);

            write!(out, "\n  [line {}] in {}", *line, name).unwrap();
        }

        out
    }
}

// Remaining opcode handlers
impl VM {
    fn jump_conditional(&mut self, condition: JumpCondition) -> VmError {
        let offset = self.read_16bit_number();
        // condition = IfTrue -> jump_if_true
        // -> ! (is_falsey())
        // condition = IfFalse -> jump_if_false
        // -> is_falsey
        let is_falsey = self.is_falsey(*self.peek(0).expect("Stack underflow in JUMP"))?;

        if is_falsey ^ bool::from(condition) {
            self.callstack.current_mut().ip += offset;
        }
        Ok(())
    }

    /// Pop from stack and jump conditionally based on the popped value.
    ///
    /// Similar to `jump_conditional` but pops the condition value from the stack
    /// before checking if it should jump. This combines the common pattern of
    /// conditional jump followed by pop.
    fn pop_jump_conditional(&mut self, condition: JumpCondition) -> VmError {
        let offset = self.read_16bit_number();
        let condition_value = self.stack.pop().expect("Stack underflow in POP_JUMP_IF");

        // Same logic as jump_conditional but with the popped condition
        if self.is_falsey(condition_value)? ^ bool::from(condition) {
            self.callstack.current_mut().ip += offset;
        }
        Ok(())
    }

    /// Jump if the top of stack matches the condition, leaving the value on the stack.
    /// Otherwise (condition doesn't match), pop the value from the stack.
    ///
    /// This is useful for and/or operators where we want to preserve
    /// the operand value when short-circuiting.
    fn jump_if_or_pop(&mut self, condition: JumpCondition) -> VmError {
        let offset = self.read_16bit_number();
        let condition_value = *self.peek(0).expect("Stack underflow in JUMP_IF_OR_POP");

        if self.is_falsey(condition_value)? ^ bool::from(condition) {
            // Condition matches, jump and leave value on stack
            self.callstack.current_mut().ip += offset;
        } else {
            // Condition doesn't match, pop it
            self.stack.pop().expect("Stack underflow in JUMP_IF_OR_POP");
        }
        Ok(())
    }

    /// Logical not the top value on the stack.
    ///
    /// Treats `nil` and `false` as falsey and everything else as truthy.
    ///
    /// # Panics
    ///
    /// If the stack is empty. This is an internal error and should never happen.
    pub(super) fn not_(&mut self) -> VmError {
        let value = self.stack.pop().expect("Stack underflow in OP_NOT");
        let result = self.is_falsey(value)?;
        self.stack_push(result.into());
        Ok(())
    }

    /// Check if the top two values on the stack are equal.
    ///
    /// Uses the specified `mode` to determine if equality should be negated.
    ///
    /// # Panics
    ///
    /// If the stack does not have two values. This is an internal error and should never happen.
    fn equal(&mut self, mode: EqualityMode) -> VmError {
        let eq_id = self.heap.string_id(&"__eq__");
        let left_id = self.peek(1).expect("Stack underflow in OP_EQUAL (left)");

        // Check if left value is an instance with __eq__ method
        if let Value::Instance(instance) = left_id
            && let Some(eq_method) = instance
                .to_value(&self.heap)
                .get_field_or_method(eq_id, &self.heap)
        {
            // If the left value is an instance, use its __eq__ method
            // Values are already on stack in correct order for method call
            self.invoke_and_run_function(eq_id, 1, matches!(eq_method, Value::NativeMethod(_)))?;
            let result = *self.peek(0).expect("Stack underflow in OP_EQUAL");
            return if let Value::Bool(bool) = result {
                if mode == EqualityMode::NotEqual {
                    self.stack.pop().expect("Stack underflow in OP_EQUAL");
                    self.stack_push((!bool).into());
                }
                Ok(())
            } else {
                self.throw_type_error(&format!(
                    "Return value of `__eq__` has to be bool, got {}",
                    result.to_string(&self.heap)
                ))
            };
        }

        // No custom __eq__ method, fall back to heap equality
        let right_id = self
            .stack
            .pop()
            .expect("stack underflow in OP_EQUAL (right)");
        let left_id = self
            .stack
            .pop()
            .expect("stack underflow in OP_EQUAL (left)");

        let result = match mode {
            EqualityMode::Equal => left_id.eq(&right_id, &self.heap),
            EqualityMode::NotEqual => !left_id.eq(&right_id, &self.heap),
        };
        self.stack_push(result.into());
        Ok(())
    }
}
