//! The vm module contains the main struct for the virtual machine and heart of the interpreter.
//!
//! The VM orchestrates the scanning to tokens, parsing of the tokens and creation of bytecode,
//! as well as the actual execution of the bytecode.

#[macro_use]
mod runtime_error;
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
mod state;
mod variables;

mod functions;

use arithmetics::{BinaryOpResult, IntoResultValue};
use callstack::CallStack;

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
    value::{
        Class, Closure, Dict, Function, Instance, List, ModuleContents, Number, Set, Upvalue, Value,
    },
};

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
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
    open_upvalues: VecDeque<UpvalueId>,
    // Could also keep a cache of the last module or its globals for performance
    modules: Vec<ModuleId>,
    // This could also just be passed to the interperet function.
    path: PathBuf,
    builtins: HashMap<StringId, Global>,
    stdlib: HashMap<StringId, ModuleContents>,
}

// Core functionality for running a script.
impl VM {
    #[must_use]
    pub(super) fn new(path: PathBuf) -> Self {
        Self {
            heap: Heap::new(),
            callstack: CallStack::new(),
            stack: Vec::with_capacity(crate::config::STACK_MAX),
            open_upvalues: VecDeque::new(),
            modules: Vec::new(),
            path,
            builtins: HashMap::default(),
            stdlib: HashMap::default(),
        }
    }

    /// Main interpret step for an input of bytes.
    ///
    /// Works by compiling the source to bytecode and then running it.
    /// Even the main script is compiled as a function.
    pub(super) fn interpret(&mut self, source: &[u8]) -> InterpretResult {
        let result = if let Some(function) = self.compile(source, "<script>") {
            let function_id = self.heap.add_function(function);

            let closure = Closure::new(*function_id.as_function(), true, None, &self.heap);

            self.add_closure_to_modules(&closure, self.path.clone(), None, None, false);

            let value_id = self.heap.add_closure(closure);
            self.stack_push(value_id);
            self.execute_call(value_id, 0);

            // Need to have the first module loaded before defining natives
            // Probably not actually needed in that order anymore as they are
            // now defined in the builtins.
            natives::define(self);
            stdlib::register(self);

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

    /// Infinite loop over the bytecode.
    ///
    /// Returns when a return instruction is hit at the top level.
    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    fn run(&mut self) -> InterpretResult {
        loop {
            run_instruction!(self);
        }
    }
}

// Remaining opcode handlers
impl VM {
    fn jump_conditional(&mut self, if_true: bool) {
        let offset = self.read_16bit_number();
        // if_true = True -> jump_if_true
        // -> ! (is_falsey())
        // if_true - is_falsey() ->:
        // true ^ false = true
        // true ^ true = false
        // if_true = False -> jump_if_false
        // -> is_falsey
        // if_true - is_falsey() ->:
        // false ^ true = true
        // false ^ false = false
        if self
            .stack
            .last()
            .expect("stack underflow in OP_JUMP_IF_FALSE")
            .is_falsey()
            ^ if_true
        {
            self.callstack.current_mut().ip += offset;
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
        let value = self
            .stack
            .pop()
            .expect("Stack underflow in OP_NOT.")
            .is_falsey();
        self.stack_push(value.into());
    }

    /// Check if the top two values on the stack are equal.
    ///
    /// If `negate` is true then the result is negated.
    ///
    /// # Panics
    ///
    /// If the stack does not have two values. This is an internal error and should never happen.
    fn equal(&mut self, negate: bool) {
        let left_id = self
            .stack
            .pop()
            .expect("stack underflow in OP_EQUAL (first)");
        let right_id = self
            .stack
            .pop()
            .expect("stack underflow in OP_EQUAL (second)");
        let result = left_id.eq(&right_id, &self.heap) != negate;
        self.stack_push(result.into());
    }
}
