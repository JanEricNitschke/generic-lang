//! The vm module contains the main struct for the virtual machine and heart of the interpreter.
//!
//! The VM orchestrates the scanning to tokens, parsing of the tokens and creation of bytecode,
//! as well as the actual execution of the bytecode.

#[macro_use]
mod runtime_error;
mod dunder;
mod errors;
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

use arithmetics::{BinaryOpResult, IntoResultValue};
use callstack::CallStack;
use exception_handling::ExceptionHandler;

use rustc_hash::FxHashMap as HashMap;
use std::collections::VecDeque;
use std::path::PathBuf;

#[cfg(feature = "trace_execution")]
use crate::chunk::InstructionDisassembler;
use crate::natives;
use crate::{
    chunk::{CodeOffset, OpCode},
    compiler::Compiler,
    heap::{Heap, ModuleId, StringId, UpvalueId},
    scanner::Scanner,
    stdlib,
    types::JumpCondition,
    value::{Class, Closure, Function, ModuleContents, Number, Upvalue, Value},
};
use std::fmt::Write;
use errors::{RuntimeError, RuntimeErrorKind, ExceptionRaisedKind, VmError};

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
            Ok(()) => InterpretResult::Ok,
            Err(_) => InterpretResult::RuntimeError,
        }
    }
}

// Helper function to create a RuntimeError
impl VM {
    fn runtime_error(&self) -> RuntimeErrorKind {
        RuntimeErrorKind
    }

    fn exception_raised(&self) -> ExceptionRaisedKind {
        ExceptionRaisedKind
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

        let result = if let Some(function) = self.compile(source, "<script>") {
            let function_id = self.heap.add_function(function);

            let closure = Closure::new(*function_id.as_function(), true, None, &self.heap);

            self.add_closure_to_modules(&closure, self.path.clone(), None, None, false);

            let value_id = self.heap.add_closure(closure);
            self.stack_push(value_id);
            self.execute_call(value_id, 0);

            self.run()
        } else {
            InterpretResult::CompileError
        };

        if result == InterpretResult::Ok {
            assert_eq!(self.stack.len(), 0);
        }
        result
    }

    fn compile(&mut self, source: &[u8], name: &str) -> Option<Function> {
        let scanner = Scanner::new(source);
        let compiler = Compiler::new(scanner, &mut self.heap, name);
        compiler.compile()
    }

    /// Load and execute generic builtin files from the `src/builtins` directory.
    ///
    /// Each builtin file is compiled and executed to completion. Then its globals
    /// (excluding `__name__`) are copied to the VM builtins and the module is
    /// popped from the modules stack.
    ///
    /// Panics if builtin loading fails, as this indicates an internal error.
    fn load_generic_builtins(&mut self) {
        // Use path relative to the rust file, consistent with stdlib handling
        let builtins_dir = std::path::Path::new(file!())
            .parent()
            .unwrap() // drop mod.rs
            .parent()
            .unwrap() // drop vm/
            .join("builtins");

        std::fs::read_dir(&builtins_dir)
            .expect("Failed to read builtins directory")
            .map(|entry| entry.expect("Failed to read directory entry").path())
            .filter(|path| path.extension().and_then(|s| s.to_str()) == Some("gen"))
            .for_each(|path| self.load_builtin_file(&path));
    }

    /// Load and execute a single builtin file in the current VM.
    fn load_builtin_file(&mut self, path: &std::path::Path) {
        let source = std::fs::read(path).expect("Failed to read builtin file");

        let name = format!("<builtin:{}>", path.file_name().unwrap().to_string_lossy());

        let function = self
            .compile(&source, &name)
            .expect("Failed to compile builtin file");

        let function_id = self.heap.add_function(function);
        let closure = Closure::new(*function_id.as_function(), true, None, &self.heap);

        self.add_closure_to_modules(&closure, path.to_path_buf(), None, None, false);

        let value_id = self.heap.add_closure(closure);
        self.stack_push(value_id);
        self.execute_call(value_id, 0);

        // Execute the builtin to completion
        let result = self.run();

        // Copy globals from the completed module to builtins (excluding __name__)
        if result == InterpretResult::Ok {
            let module_globals = std::mem::take(self.globals());
            // Extend builtins with all globals from the module
            self.builtins.extend(module_globals);
            // Remove __name__ from builtins
            self.builtins
                .remove(&self.heap.builtin_constants().script_name);
        } else {
            panic!(
                "Failed to execute builtin file {}: {result:?}",
                path.display()
            );
        }

        // Clean up the builtin module
        self.modules.pop();
    }

    /// Infinite loop over the bytecode.
    ///
    /// Returns when a return instruction is hit at the top level.
    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    fn run(&mut self) -> InterpretResult {
        loop {
            run_instruction!(self);
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
    fn jump_conditional(&mut self, condition: JumpCondition) {
        let offset = self.read_16bit_number();
        // condition = IfTrue -> jump_if_true
        // -> ! (is_falsey())
        // condition = IfFalse -> jump_if_false
        // -> is_falsey
        let is_falsey = self.is_falsey(*self.peek(0).expect("Stack underflow in JUMP"));

        if is_falsey ^ bool::from(condition) {
            self.callstack.current_mut().ip += offset;
        }
    }

    /// Pop from stack and jump conditionally based on the popped value.
    ///
    /// Similar to `jump_conditional` but pops the condition value from the stack
    /// before checking if it should jump. This combines the common pattern of
    /// conditional jump followed by pop.
    fn pop_jump_conditional(&mut self, condition: JumpCondition) {
        let offset = self.read_16bit_number();
        let condition_value = self.stack.pop().expect("Stack underflow in POP_JUMP_IF");

        // Same logic as jump_conditional but with the popped condition
        if self.is_falsey(condition_value) ^ bool::from(condition) {
            self.callstack.current_mut().ip += offset;
        }
    }

    /// Jump if the top of stack matches the condition, leaving the value on the stack.
    /// Otherwise (condition doesn't match), pop the value from the stack.
    ///
    /// This is useful for and/or operators where we want to preserve
    /// the operand value when short-circuiting.
    fn jump_if_or_pop(&mut self, condition: JumpCondition) {
        let offset = self.read_16bit_number();
        let condition_value = *self.peek(0).expect("Stack underflow in JUMP_IF_OR_POP");

        if self.is_falsey(condition_value) ^ bool::from(condition) {
            // Condition matches, jump and leave value on stack
            self.callstack.current_mut().ip += offset;
        } else {
            // Condition doesn't match, pop it
            self.stack.pop().expect("Stack underflow in JUMP_IF_OR_POP");
        }
    }

    /// Logical not the top value on the stack.
    ///
    /// Treats `nil` and `false` as falsey and everything else as truthy.
    ///
    /// # Panics
    ///
    /// If the stack is empty. This is an internal error and should never happen.
    pub(super) fn not_(&mut self) {
        let value = self.stack.pop().expect("Stack underflow in OP_NOT");
        let result = self.is_falsey(value);
        self.stack_push(result.into());
    }

    /// Check if the top two values on the stack are equal.
    ///
    /// If `negate` is true then the result is negated.
    ///
    /// # Panics
    ///
    /// If the stack does not have two values. This is an internal error and should never happen.
    fn equal(&mut self, negate: bool) -> Option<InterpretResult> {
        let eq_id = self.heap.string_id(&"__eq__");
        let left_id = self.peek(1).expect("Stack underflow in OP EQUAL (left)");

        // Check if left value is an instance with __eq__ method
        if let Value::Instance(instance) = left_id
            && instance
                .to_value(&self.heap)
                .has_field_or_method(eq_id, &self.heap)
        {
            // If the left value is an instance, use its __eq__ method
            // Values are already on stack in correct order for method call
            return if self.invoke(eq_id, 1) {
                None // Continue execution - method will handle result
            } else {
                Some(InterpretResult::RuntimeError)
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

        let result = left_id.eq(&right_id, &self.heap) != negate;
        self.stack_push(result.into());
        None
    }
}
