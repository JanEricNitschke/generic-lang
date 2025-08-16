//! The compiler module contains the compiler for the Generic language.
//!
//! It compiles tokens from the scanner into bytecode for the VM to execute.

mod back;
mod error;
mod front;
mod rules;
mod variables;

use rustc_hash::FxHashMap as HashMap;

use shrinkwraprs::Shrinkwrap;

use crate::{
    chunk::{Chunk, CodeOffset, ConstantLongIndex},
    compiler::rules::{Rules, make_rules},
    heap::{Heap, StringId},
    scanner::{Scanner, Token, TokenKind},
    types::Line,
    value::Function,
};

#[derive(Shrinkwrap, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Default, Debug)]
#[shrinkwrap(mutable)]
struct ScopeDepth(i32);

/// Represents a local variable in the current scope.
///
/// Store their depth in order to properly release them when a scope ends,
/// whether they are mutable, and whether they are captured by a closure.
/// Also contains the token they were created from. This is usually an identifier.
/// Synthetic tokens exist for `super`, `this`, and other hidden variables like
/// the iterator in foreach loops.
#[derive(Debug)]
struct Local<'scanner> {
    name: Token<'scanner>,
    depth: ScopeDepth,
    mutable: bool,
    is_captured: bool,
}

/// Characterizes the types of functions.
///
/// - Function is a normal function.
/// - Initializer is a constructor and is special because it returns `this` on exit without
///   a return statement or on a bare `return`. Returns with values are not allowed.
/// - Method is a method on a class. It is special because the local slot 0 is always `this`.
/// - Script is the top-level code in a file (main script or imported modules).
///   Does not allow `return` statements.
#[derive(Copy, Clone, PartialEq, Eq)]
enum FunctionType {
    Function,
    Initializer,
    Method,
    Script,
}

/// Struct to handle the state of a loop.
///
/// Needed for nested loops to properly assign jump targets for continue and break.
#[derive(Clone, Debug)]
struct LoopState {
    /// The depth of the scope when the loop was entered.
    /// Needed to free all locals declared inside the loop.
    depth: ScopeDepth,
    /// The bytecode index of the start of the loop.
    /// Used to jump to the start at the end of an iteration or using `continue`.
    start: CodeOffset,
    /// Bytecode indices of `break` statements inside the loop.
    /// Needed to patch all of them when the compilation of the loop body is finished.
    /// Required as there is no way to know how long the loop body will be when the `break` is compiled.
    break_jumps: Vec<CodeOffset>,
    /// The label of the loop, if any.
    /// Used to identify the loop in `break` and `continue` statements.
    label: Option<String>,
}

impl LoopState {
    #[must_use]
    const fn new(depth: ScopeDepth, start: CodeOffset, label: Option<String>) -> Self {
        Self {
            depth,
            start,
            break_jumps: Vec::new(),
            label,
        }
    }
}

/// Struct to handle the state of an upvalue.
///
/// Tracks the position of the upvalue and whether is is captured directly from
/// a local variable or from the upvalue of an enclosing closure.
#[derive(Clone, Debug)]
struct Upvalue {
    index: u8,
    is_local: bool,
}

/// Nestable part of the compiler state
///
/// This struct is used to keep track of the state of the compiler that can be nested
/// when compiling nested functions.
struct NestableState<'scanner> {
    current_function: Function,
    function_type: FunctionType,

    locals: Vec<Local<'scanner>>,
    globals_by_name: HashMap<StringId, ConstantLongIndex>,
    upvalues: Vec<Upvalue>,
    scope_depth: ScopeDepth,
    loop_state: Vec<LoopState>,
}

impl NestableState<'_> {
    #[must_use]
    fn new(function_name: StringId, function_type: FunctionType) -> Self {
        NestableState {
            current_function: Function::new(0, function_name),
            function_type,
            locals: vec![Local {
                name: Token {
                    kind: TokenKind::Identifier,
                    lexeme: if function_type == FunctionType::Method
                        || function_type == FunctionType::Initializer
                    {
                        b"this"
                    } else {
                        &[]
                    },
                    line: Line(0),
                },
                depth: ScopeDepth::default(),
                mutable: false,
                is_captured: false,
            }],
            upvalues: Vec::new(),
            globals_by_name: HashMap::default(),
            scope_depth: ScopeDepth::default(),
            loop_state: Vec::new(),
        }
    }
}

/// Keep track of the state of a class declaration
///
/// Similar to `LoopState`, this is needed for nested class declarations.
/// Currently only tracks whether the class has a superclass.
struct ClassState {
    pub has_superclass: bool,
}

impl ClassState {
    #[must_use]
    const fn new() -> Self {
        Self {
            has_superclass: false,
        }
    }
}

/// Main compiler struct that turns tokens into bytecode.
pub struct Compiler<'scanner, 'heap> {
    /// The VM heap. Already present here to store compiled functions.
    /// and cache strings.
    heap: &'heap mut Heap,

    /// Rules used for parsing expressions by precedence climbing.
    rules: Rules<'scanner, 'heap>,

    /// The scanner that provides tokens to the compiler.
    scanner: Scanner<'scanner>,
    previous: Option<Token<'scanner>>,
    current: Option<Token<'scanner>>,

    had_error: bool,
    panic_mode: bool,

    nestable_state: Vec<NestableState<'scanner>>,
    class_state: Vec<ClassState>,
}

impl<'scanner, 'heap> Compiler<'scanner, 'heap> {
    #[must_use]
    pub(super) fn new(scanner: Scanner<'scanner>, heap: &'heap mut Heap, name: &str) -> Self {
        let function_name = heap.string_id(&name);

        Compiler {
            heap,
            scanner,
            previous: None,
            current: None,
            had_error: false,
            panic_mode: false,
            rules: make_rules(),
            nestable_state: vec![NestableState::new(function_name, FunctionType::Script)],
            class_state: vec![],
        }
    }

    /// Compile the tokens provided by the scanner into a function.
    ///
    /// This is the main compilation loop of generic.
    pub(super) fn compile(mut self) -> Option<Function> {
        self.advance();

        while !self.match_(TokenKind::Eof) {
            self.declaration();
        }

        self.end(false.into());
        if self.had_error {
            None
        } else {
            Some(self.nestable_state.pop().unwrap().current_function)
        }
    }

    // Nesting related function are here to not have to export the `NestableState` struct.
    fn start_nesting<S>(&mut self, function_name: &S, function_type: FunctionType)
    where
        S: ToString,
    {
        let function_name = self.heap.string_id(function_name);
        self.nestable_state
            .push(NestableState::new(function_name, function_type));
    }

    fn end_nesting(&mut self) -> NestableState<'_> {
        self.nestable_state.pop().unwrap()
    }

    fn nested<F, S>(
        &mut self,
        function_name: &S,
        function_type: FunctionType,
        f: F,
    ) -> NestableState<'_>
    where
        S: ToString,
        F: Fn(&mut Self),
    {
        self.start_nesting(function_name, function_type);
        f(self);
        self.end_nesting()
    }

    fn has_enclosing(&self) -> bool {
        self.nestable_state.len() > 1
    }

    /// Call a function from within the enclosing scope.
    ///
    /// Mainly used for recursively resolving upvalues.
    fn in_enclosing<F, R>(&mut self, f: F) -> R
    where
        F: Fn(&mut Self) -> R,
    {
        assert!(self.has_enclosing());
        let state = self.nestable_state.pop().unwrap();
        let result = f(self);
        self.nestable_state.push(state);
        result
    }

    fn current_function(&self) -> &Function {
        &self.nestable_state.last().unwrap().current_function
    }

    fn current_function_mut(&mut self) -> &mut Function {
        &mut self.nestable_state.last_mut().unwrap().current_function
    }

    fn loop_state(&self) -> &Vec<LoopState> {
        &self.nestable_state.last().unwrap().loop_state
    }

    fn loop_state_mut(&mut self) -> &mut Vec<LoopState> {
        &mut self.nestable_state.last_mut().unwrap().loop_state
    }

    fn last_loop_state(&self) -> Option<&LoopState> {
        self.loop_state().last()
    }

    fn last_loop_state_mut(&mut self) -> Option<&mut LoopState> {
        self.loop_state_mut().last_mut()
    }

    fn loop_state_by_label(&self, label: Option<&str>) -> Option<&LoopState> {
        label.map_or_else(
            || self.last_loop_state(),
            |label| {
                self.loop_state()
                    .iter()
                    .rev()
                    .find(|state| state.label.as_deref() == Some(label))
            },
        )
    }

    fn loop_state_by_label_mut(&mut self, label: Option<&str>) -> Option<&mut LoopState> {
        match label {
            None => self.last_loop_state_mut(),
            Some(label) => self
                .loop_state_mut()
                .iter_mut()
                .rev()
                .find(|state| state.label.as_deref() == Some(label)),
        }
    }

    fn locals(&self) -> &Vec<Local<'scanner>> {
        &self.nestable_state.last().unwrap().locals
    }

    fn locals_mut(&mut self) -> &mut Vec<Local<'scanner>> {
        &mut self.nestable_state.last_mut().unwrap().locals
    }

    fn function_type(&self) -> FunctionType {
        self.nestable_state.last().unwrap().function_type
    }

    fn scope_depth(&self) -> ScopeDepth {
        self.nestable_state.last().unwrap().scope_depth
    }

    fn scope_depth_mut(&mut self) -> &mut ScopeDepth {
        &mut self.nestable_state.last_mut().unwrap().scope_depth
    }

    fn globals_by_name(&self) -> &HashMap<StringId, ConstantLongIndex> {
        &self.nestable_state.last().unwrap().globals_by_name
    }

    fn globals_by_name_mut(&mut self) -> &mut HashMap<StringId, ConstantLongIndex> {
        &mut self.nestable_state.last_mut().unwrap().globals_by_name
    }

    fn upvalues(&self) -> &Vec<Upvalue> {
        &self.nestable_state.last().unwrap().upvalues
    }

    fn upvalues_mut(&mut self) -> &mut Vec<Upvalue> {
        &mut self.nestable_state.last_mut().unwrap().upvalues
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.current_function_mut().chunk
    }

    fn current_chunk_len(&mut self) -> usize {
        self.current_chunk().code().len()
    }

    fn current_class(&self) -> Option<&ClassState> {
        self.class_state.last()
    }

    fn current_class_mut(&mut self) -> Option<&mut ClassState> {
        self.class_state.last_mut()
    }
}
