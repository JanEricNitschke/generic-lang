//! Backend of the compiler. This module is responsible for emitting bytecode.
//!
//! The walk visits nodes in source order, so string interning, constant
//! table population, local slot assignment, and jump structure follow
//! the source. The frame stack (`Frame`) carries each function's locals,
//! scope depths, upvalues, and loop state along the way.

use rustc_hash::FxHashMap as HashMap;
use shrinkwraprs::Shrinkwrap;

use super::ast::{
    AccessMode, Arg, BinaryOp, BoolOperator, ClassDecl, ClassMember, ClassVar, Expr, ExprContext,
    ExprKind, FstringPart, FunKind, FunctionDef, InjectedBody, InjectedProgram, LambdaBody,
    LambdaLit, Literal, Method, ParamList, Program, StmtKind, SwitchCase, TryStmt, Tstring,
    UnaryOp,
};
use crate::{
    chunk::{Chunk, CodeOffset, ConstantIndex, ConstantLongIndex, OpCode},
    config::{LAMBDA_NAME, SPREAD_BITMAP_BYTES},
    heap::{Heap, StringId},
    types::{
        ConditionType, Location, LoopType, Mutability, NumberEncoding, OpcodeLocation, ReturnMode,
        render_snippet_rows,
    },
    utils::get_file_stem,
    value::{Function, GenericInt, Instance, Number, Tuple, Value},
};

/// Scope nesting depth: 0 is global scope, locals carry `-1` while
/// declared but not yet initialized.
#[derive(Shrinkwrap, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Default, Debug)]
#[shrinkwrap(mutable)]
pub(super) struct ScopeDepth(pub(super) i32);

/// Represents a local variable in the current scope.
///
/// Store their depth in order to properly release them when a scope ends,
/// whether they are mutable, and whether they are captured by a closure.
/// Also contains the name they were created from. This is usually from an identifier.
/// Synthetic names exist for `super`, `this`, and other hidden variables like
/// the iterator in foreach loops.
#[derive(Debug)]
pub(super) struct Local {
    pub(super) name: String,
    pub(super) depth: ScopeDepth,
    pub(super) mutability: Mutability,
    pub(super) is_captured: bool,
}

/// Struct to handle the state of a loop.
///
/// Needed for nested loops to properly assign jump targets for continue and break.
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
    /// The loop's head keyword (`while`/`until`/`for`/`foreach`):
    /// the `continue` jump target and the origin of the loop's own
    /// jump-size error spans.
    head_location: Location,
}

impl LoopState {
    #[must_use]
    const fn new(
        depth: ScopeDepth,
        start: CodeOffset,
        label: Option<String>,
        head_location: Location,
    ) -> Self {
        Self {
            depth,
            start,
            break_jumps: Vec::new(),
            label,
            head_location,
        }
    }
}

/// Struct to handle the state of an upvalue.
///
/// Tracks the position of the upvalue and whether is is captured directly from
/// a local variable or from the upvalue of an enclosing closure.
#[derive(Clone, Debug)]
pub(super) struct Upvalue {
    pub(super) index: u8,
    pub(super) is_local: bool,
}

/// Nestable part of the compiler state
///
/// This struct is used to keep track of the state of the compiler that can be nested
/// when compiling nested functions.
pub(super) struct Frame {
    pub(super) function: Function,
    pub(super) kind: FunKind,
    pub(super) locals: Vec<Local>,
    pub(super) globals_by_name: HashMap<StringId, ConstantLongIndex>,
    pub(super) upvalues: Vec<Upvalue>,
    pub(super) scope_depth: ScopeDepth,
    loop_state: Vec<LoopState>,
}

impl Frame {
    fn new(function_name: StringId, kind: FunKind) -> Self {
        Self {
            function: Function::new(0, function_name),
            kind,
            locals: vec![Local {
                name: String::from(if matches!(kind, FunKind::Method | FunKind::Initializer) {
                    "this"
                } else {
                    ""
                }),
                depth: ScopeDepth::default(),
                mutability: Mutability::Immutable,
                is_captured: false,
            }],
            globals_by_name: HashMap::default(),
            upvalues: Vec::new(),
            scope_depth: ScopeDepth::default(),
            loop_state: Vec::new(),
        }
    }
}

pub(crate) struct Codegen<'heap, 'src> {
    /// The VM heap. Already present here to store compiled functions.
    /// and cache strings.
    pub(super) heap: &'heap mut Heap,
    /// The compiled source, used to render the snippet rows under a
    /// codegen error.
    source: &'src str,
    pub(super) frames: Vec<Frame>,
    had_error: bool,
    /// Like the parser's panic mode: the first error in a declaration
    /// region wins; subsequent ones are suppressed until the next
    /// declaration boundary.
    panic_mode: bool,
    /// Collected compile errors; printed to stderr by [`Self::compile`],
    /// returned to the caller by the collecting entry points (eval/exec
    /// wrap them in a catchable `SyntaxError`).
    errors: Vec<String>,
    /// The nil-guard jumps of the null-safe chain currently being
    /// compiled; the chain's root patches them (see [`Self::gen_expr`]).
    nullsafe_chain_jumps: Vec<CodeOffset>,
    /// Whether the source is an embedded builtin; gates the `print_code`
    /// output.
    #[cfg(feature = "print_code")]
    is_builtin: bool,
}

impl<'heap, 'src> Codegen<'heap, 'src> {
    #[must_use]
    pub(crate) fn new(
        heap: &'heap mut Heap,
        source: &'src str,
        #[cfg(feature = "print_code")] is_builtin: bool,
    ) -> Self {
        Self {
            heap,
            source,
            frames: Vec::new(),
            had_error: false,
            panic_mode: false,
            errors: Vec::new(),
            nullsafe_chain_jumps: Vec::new(),
            #[cfg(feature = "print_code")]
            is_builtin,
        }
    }

    /// Compile a parsed program into a script `Function`.
    pub(crate) fn compile(
        &mut self,
        program: &Program,
        name: &str,
    ) -> Result<Function, Vec<String>> {
        let frame = self.script_frame(name);
        self.frames.push(frame);

        self.gen_declarations(&program.declarations);

        self.end(ReturnMode::Normal, OpcodeLocation::default());

        self.finish()
    }

    /// Compile injected (`eval`/`exec`) source: a plain function whose
    /// leading local slots are pre-declared and filled by the caller like
    /// arguments. `Eval` compiles a single expression the function returns
    /// raw; `Exec` compiles plain declarations.
    pub(crate) fn compile_injected(
        &mut self,
        program: &InjectedProgram,
        name: &str,
    ) -> Result<Function, Vec<String>> {
        let frame = self.script_frame(name);
        self.frames.push(frame);

        // `mark_initialized` no-ops at scope depth 0; these slots are
        // filled by the caller like arguments, so they are live from
        // the first instruction.
        for local in &program.locals {
            self.locals_mut().push(Local {
                name: local.clone(),
                depth: ScopeDepth::default(),
                mutability: Mutability::Mutable,
                is_captured: false,
            });
        }
        self.current_function_mut().arity = program.locals.len();
        self.current_function_mut().required_params = program.locals.len();

        match program.body {
            InjectedBody::Eval(ref expr) => {
                self.gen_expr(expr);
                self.end(ReturnMode::Raw, OpcodeLocation::default());
            }
            InjectedBody::Exec(ref declarations) => {
                self.gen_declarations(declarations);
                self.end(ReturnMode::Normal, OpcodeLocation::default());
            }
        }

        self.finish()
    }

    /// Common tail for both entries: fail with the collected errors, or
    /// return the finished script function.
    fn finish(&mut self) -> Result<Function, Vec<String>> {
        if self.had_error {
            Err(std::mem::take(&mut self.errors))
        } else {
            Ok(self.frames.pop().unwrap().function)
        }
    }

    /// The outermost frame for a script-like compilation (the top-level
    /// file, an imported module, or injected source): an empty slot-0
    /// local and global scope.
    fn script_frame(&mut self, name: &str) -> Frame {
        Frame::new(self.heap.string_id(&name), FunKind::Script)
    }

    pub(super) fn error(&mut self, msg: &str) {
        // Bare fallback for sites that cannot name a source token
        // (practically unreachable limits); no snippet rows.
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        self.errors.push(msg.to_string());
        self.had_error = true;
    }

    /// Report a compile error at `location`, naming `token` - the same
    /// format the parser produces (`[line N] Error at 'tok': msg` plus
    /// the underlined source snippet), so the two error sources are
    /// indistinguishable downstream.
    pub(super) fn error_at(&mut self, location: Location, token: &str, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        let mut message = format!("[line {}] Error at '{token}': {msg}", *location.start_line);
        message.push('\n');
        render_snippet_rows(&mut message, self.source, &OpcodeLocation::new(location));
        self.errors.push(message);
        self.had_error = true;
    }
}

// Nesting related function are here to not have to export the `Frame` struct.
impl Codegen<'_, '_> {
    fn push_frame(&mut self, name: &str, kind: FunKind) {
        let function_name = self.heap.string_id(&name);
        self.frames.push(Frame::new(function_name, kind));
    }

    fn nested<F>(&mut self, name: &str, kind: FunKind, f: F) -> Frame
    where
        F: FnOnce(&mut Self),
    {
        self.push_frame(name, kind);
        f(self);
        self.frames.pop().unwrap()
    }

    pub(super) fn has_enclosing(&self) -> bool {
        self.frames.len() > 1
    }

    /// Call a function from within the enclosing scope.
    ///
    /// Mainly used for recursively resolving upvalues.
    pub(super) fn in_enclosing<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        assert!(
            self.has_enclosing(),
            "in_enclosing requires an enclosing frame"
        );
        let frame = self.frames.pop().unwrap();
        let result = f(self);
        self.frames.push(frame);
        result
    }

    pub(super) fn current_function_mut(&mut self) -> &mut Function {
        &mut self.frames.last_mut().unwrap().function
    }

    fn loop_state(&self) -> &Vec<LoopState> {
        &self.frames.last().unwrap().loop_state
    }

    fn loop_state_mut(&mut self) -> &mut Vec<LoopState> {
        &mut self.frames.last_mut().unwrap().loop_state
    }

    fn last_loop_state(&self) -> Option<&LoopState> {
        self.loop_state().last()
    }

    fn last_loop_state_mut(&mut self) -> Option<&mut LoopState> {
        self.loop_state_mut().last_mut()
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

    fn loop_depth_by_label(&self, label: Option<&str>) -> Option<ScopeDepth> {
        match label {
            None => self.last_loop_state().map(|state| state.depth),
            Some(label) => self
                .loop_state()
                .iter()
                .rev()
                .find(|state| state.label.as_deref() == Some(label))
                .map(|state| state.depth),
        }
    }

    pub(super) fn locals(&self) -> &Vec<Local> {
        &self.frames.last().unwrap().locals
    }

    pub(super) fn locals_mut(&mut self) -> &mut Vec<Local> {
        &mut self.frames.last_mut().unwrap().locals
    }

    fn function_kind(&self) -> FunKind {
        self.frames.last().unwrap().kind
    }

    pub(super) fn scope_depth(&self) -> ScopeDepth {
        self.frames.last().unwrap().scope_depth
    }

    pub(super) fn scope_depth_mut(&mut self) -> &mut ScopeDepth {
        &mut self.frames.last_mut().unwrap().scope_depth
    }

    pub(super) fn globals_by_name(&self) -> &HashMap<StringId, ConstantLongIndex> {
        &self.frames.last().unwrap().globals_by_name
    }

    pub(super) fn globals_by_name_mut(&mut self) -> &mut HashMap<StringId, ConstantLongIndex> {
        &mut self.frames.last_mut().unwrap().globals_by_name
    }

    pub(super) fn upvalues(&self) -> &Vec<Upvalue> {
        &self.frames.last().unwrap().upvalues
    }

    pub(super) fn upvalues_mut(&mut self) -> &mut Vec<Upvalue> {
        &mut self.frames.last_mut().unwrap().upvalues
    }

    fn current_chunk(&self) -> &Chunk {
        &self.frames.last().unwrap().function.chunk
    }

    pub(super) fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.frames.last_mut().unwrap().function.chunk
    }

    fn current_chunk_len(&self) -> usize {
        self.current_chunk().code().len()
    }
}

// Emit helpers.
impl Codegen<'_, '_> {
    pub(super) fn emit_byte<T>(&mut self, byte: T, location: OpcodeLocation)
    where
        T: Into<u8>,
    {
        self.current_chunk_mut().write(byte, location);
    }

    pub(super) fn emit_24bit_number(&mut self, number: usize, location: OpcodeLocation) -> bool {
        self.current_chunk_mut()
            .write_24bit_number(number, location)
    }

    fn emit_bytes<T1, T2>(&mut self, byte1: T1, byte2: T2, location: OpcodeLocation)
    where
        T1: Into<u8>,
        T2: Into<u8>,
    {
        self.current_chunk_mut().write(byte1, location);
        self.current_chunk_mut().write(byte2, location);
    }

    fn emit_return(&mut self, location: OpcodeLocation) {
        if self.function_kind() == FunKind::Initializer {
            self.emit_bytes(OpCode::GetLocal, 0, location);
        } else if self.function_kind() == FunKind::Generator {
            self.emit_byte(OpCode::StopIteration, location);
        } else {
            self.emit_byte(OpCode::Nil, location);
        }
        self.emit_byte(OpCode::Return, location);
    }

    fn end(&mut self, return_mode: ReturnMode, location: OpcodeLocation) {
        match return_mode {
            ReturnMode::Raw => self.emit_byte(OpCode::Return, location),
            ReturnMode::Normal => self.emit_return(location),
        }

        #[cfg(feature = "print_code")]
        self.print_code_info();
    }

    fn emit_constant(&mut self, value: Value, location: OpcodeLocation) {
        match value {
            Value::Number(Number::Integer(GenericInt::Small(1))) => {
                self.emit_byte(OpCode::LoadOne, location);
            }
            Value::Number(Number::Integer(GenericInt::Small(2))) => {
                self.emit_byte(OpCode::LoadTwo, location);
            }
            Value::Number(Number::Integer(GenericInt::Small(-1))) => {
                self.emit_byte(OpCode::LoadMinusOne, location);
            }
            Value::Number(Number::Integer(GenericInt::Small(0))) => {
                self.emit_byte(OpCode::LoadZero, location);
            }
            Value::Number(Number::Float(0.0)) => self.emit_byte(OpCode::LoadZerof, location),
            Value::Number(Number::Float(1.0)) => self.emit_byte(OpCode::LoadOnef, location),
            _ => {
                if !self.current_chunk_mut().write_constant(value, location) {
                    self.error("Too many constants in one chunk.");
                }
            }
        }
    }

    fn emit_jump(&mut self, instruction: OpCode, location: OpcodeLocation) -> CodeOffset {
        self.emit_byte(instruction, location);
        let retval = CodeOffset(self.current_chunk().code().len() - 1);
        self.emit_byte(0xff, location);
        self.emit_byte(0xff, location);
        retval
    }

    #[allow(clippy::cast_possible_truncation)]
    fn patch_jump(&mut self, jump_offset: CodeOffset, error_location: Location, error_token: &str) {
        let jump_length = self.current_chunk().code().len() - *jump_offset - 3; // 3: length of the jump instruction + its arg

        if jump_length > usize::from(u16::MAX) {
            self.error_at(error_location, error_token, "Too much code to jump over.");
        }

        self.current_chunk_mut()
            .patch(CodeOffset(*jump_offset + 1), (jump_length >> 8) as u8);
        self.current_chunk_mut()
            .patch(CodeOffset(*jump_offset + 2), jump_length as u8);
    }

    fn patch_break_jumps(&mut self, close_location: Location) {
        while let Some(break_jump) = self
            .last_loop_state_mut()
            .as_mut()
            .unwrap()
            .break_jumps
            .pop()
        {
            self.patch_jump(break_jump, close_location, "break");
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    fn emit_loop(
        &mut self,
        loop_start: CodeOffset,
        location: OpcodeLocation,
        error_location: Location,
        error_token: &str,
    ) {
        let offset = self.current_chunk().code().len() - *loop_start + 3; // 3: length of the loop instruction + its arg

        self.emit_byte(OpCode::Loop, location);
        if offset > usize::from(u16::MAX) {
            self.error_at(error_location, error_token, "Loop body too large.");
        }

        self.emit_byte((offset >> 8) as u8, location);
        self.emit_byte(offset as u8, location);
    }

    pub(super) fn emit_number(
        &mut self,
        n: usize,
        encoding: NumberEncoding,
        location: OpcodeLocation,
    ) -> bool {
        match encoding {
            NumberEncoding::Long => self.emit_24bit_number(n, location),
            NumberEncoding::Short => {
                if let Ok(n) = u8::try_from(n) {
                    self.emit_byte(n, location);
                    true
                } else {
                    false
                }
            }
        }
    }
}

// `print_code` support: after a function (or the script) is finished,
// print its chunk - and the verbose compiler state when requested.
// Embedded builtins are silent unless `print_code_builtin` is set.
#[cfg(feature = "print_code")]
impl Codegen<'_, '_> {
    fn print_code_info(&self) {
        if !self.had_error && (cfg!(feature = "print_code_builtin") || !self.is_builtin) {
            println!("{}", self.current_chunk().to_string(self.heap));

            #[cfg(feature = "print_code_verbose")]
            self.print_verbose_compiler_info();
        }
    }

    #[cfg(feature = "print_code_verbose")]
    fn print_verbose_compiler_info(&self) {
        println!("== VERBOSE DEBUG INFO ==");
        println!("Function Type: {:?}", self.function_kind());
        println!("Scope Depth: {}", *self.scope_depth());

        self.print_locals();
        self.print_upvalues();

        println!("== END VERBOSE DEBUG INFO ==");
        println!();
    }

    #[cfg(feature = "print_code_verbose")]
    fn print_locals(&self) {
        let locals = self.locals();
        if locals.is_empty() {
            println!("No locals");
            return;
        }
        println!("Locals ({} total):", locals.len());

        let max_name_width = locals
            .iter()
            .map(|local| local.name.len())
            .max()
            .unwrap_or(0);

        for (i, local) in locals.iter().enumerate() {
            println!(
                "  [{:3}] '{:width_name$}' (depth: {:3}, mutability: {:10}, captured: {})",
                i,
                local.name,
                *local.depth,
                format!("{:?}", local.mutability),
                local.is_captured,
                width_name = max_name_width,
            );
        }
    }

    #[cfg(feature = "print_code_verbose")]
    fn print_upvalues(&self) {
        let upvalues = self.upvalues();
        if upvalues.is_empty() {
            println!("No upvalues");
            return;
        }
        println!("Upvalues ({} total):", upvalues.len());

        // Calculate maximum widths for alignment
        for (i, upvalue) in upvalues.iter().enumerate() {
            println!(
                "  [{:3}] index: {:3}, is_local: {}",
                i, upvalue.index, upvalue.is_local
            );
        }
    }
}

// Statements.
impl Codegen<'_, '_> {
    /// Generate a run of declarations/statements, resetting the panic mode
    /// after each: the first error within one declaration suppresses
    /// the rest *of that declaration* only.
    fn gen_declarations(&mut self, stmts: &[StmtKind]) {
        for stmt in stmts {
            self.gen_stmt(stmt);
            self.panic_mode = false;
        }
    }

    fn gen_var_decl(
        &mut self,
        name: &str,
        name_location: Location,
        mutability: Mutability,
        initializer: Option<&Expr>,
    ) {
        let global = self.declare_or_intern(name, mutability, name_location);
        if let Some(initializer) = initializer {
            self.gen_expr(initializer);
        } else {
            self.emit_byte(OpCode::Nil, OpcodeLocation::new(name_location));
        }
        self.define_variable(global, mutability, OpcodeLocation::new(name_location));
    }

    /// A `var`/`const` declaration that is always local (a `for` loop
    /// variable lives in the loop's scope, which is never depth 0).
    fn gen_var_decl_local(
        &mut self,
        name: &str,
        name_location: Location,
        mutability: Mutability,
        initializer: Option<&Expr>,
    ) {
        self.declare_variable(name, mutability, name_location);
        if let Some(initializer) = initializer {
            self.gen_expr(initializer);
        } else {
            self.emit_byte(OpCode::Nil, OpcodeLocation::new(name_location));
        }
        self.define_variable(None, mutability, OpcodeLocation::default());
    }

    /// Declare a function's parameters and defaults in the current frame.
    ///
    /// Defaults are evaluated once, in the enclosing scope at definition
    /// time; their values are left on the stack for the following
    /// `OP_CLOSURE` to capture into the closure.
    fn gen_parameters(&mut self, parameters: &ParamList) {
        let ParamList { params, rest } = parameters;
        self.current_function_mut().arity = params.len();
        self.current_function_mut().required_params = params
            .iter()
            .filter(|param| param.default.is_none())
            .count();

        for param in params {
            // Parameters are declared locals: a duplicate parameter name is
            // a scope error, reported at the parameter's name token.
            self.declare_variable(&param.name, Mutability::Mutable, param.location);
            if let Some(default) = &param.default {
                self.in_enclosing(|codegen| codegen.gen_expr(default));
            }
            self.define_variable(None, Mutability::Mutable, OpcodeLocation::default());
        }
        if let Some(rest) = rest {
            self.current_function_mut().is_variadic = true;
            self.declare_variable(&rest.name, Mutability::Mutable, rest.location);
            self.define_variable(None, Mutability::Mutable, OpcodeLocation::default());
        }
    }

    /// Compile the definition in a fresh frame, then emit `Closure` plus
    /// its upvalue descriptor in the enclosing chunk. `kind` comes from
    /// the declaration site (a [`FunctionKind`] or [`MethodKind`]
    /// widened to [`FunKind`]).
    fn gen_function_def(&mut self, def: &FunctionDef, kind: FunKind) {
        let fn_location = OpcodeLocation::new(def.fn_location);

        let frame = self.nested(&def.name, kind, |codegen| {
            codegen.begin_scope();
            codegen.gen_parameters(&def.parameters);

            // Declarations (and only they) have a pending name local in the
            // enclosing scope to initialize once the parameter list is done.
            if kind.initializes_enclosing_name() {
                codegen.in_enclosing(Self::mark_initialized);
            }

            if kind == FunKind::Generator {
                codegen.emit_byte(OpCode::ReturnGenerator, fn_location);
            }
            codegen.gen_declarations(&def.body);
            codegen.end(ReturnMode::Normal, fn_location);
        });

        let nested_function = frame.function;
        let nested_upvalues = frame.upvalues;

        self.emit_closure(
            nested_function,
            nested_upvalues,
            fn_location,
            def.fn_location,
            &def.name,
        );
    }

    /// A lambda literal: like [`Self::gen_function_def`], but the frame
    /// binds no name and the body may be a bare expression (implicit
    /// `return`).
    fn gen_lambda(&mut self, lambda: &LambdaLit) {
        let fn_location = OpcodeLocation::new(lambda.fn_location);

        let frame = self.nested(LAMBDA_NAME, FunKind::Lambda, |codegen| {
            codegen.begin_scope();
            codegen.gen_parameters(&lambda.parameters);

            match &lambda.body {
                LambdaBody::Block(stmts) => {
                    codegen.gen_declarations(stmts);
                    codegen.end(ReturnMode::Normal, fn_location);
                }
                LambdaBody::Expression(expr) => {
                    codegen.gen_expr(expr);
                    codegen.end(ReturnMode::Raw, fn_location);
                }
            }
        });

        let nested_function = frame.function;
        let nested_upvalues = frame.upvalues;

        self.emit_closure(
            nested_function,
            nested_upvalues,
            fn_location,
            lambda.fn_location,
            "->",
        );
    }

    /// Emit the `Closure` instruction for a function compiled in a fresh
    /// frame, with its constant-table overflow error at the function's
    /// name (or the `->` for a lambda).
    fn emit_closure(
        &mut self,
        function: Function,
        upvalues: Vec<Upvalue>,
        location: OpcodeLocation,
        error_location: Location,
        error_token: &str,
    ) {
        self.emit_byte(OpCode::Closure, location);
        let function_value = self.heap.add_function(function);
        let value_id_byte = u8::try_from(self.current_chunk_mut().make_constant(function_value).0)
            .unwrap_or_else(|_| {
                self.error_at(
                    error_location,
                    error_token,
                    "Too many constants in one chunk.",
                );
                0
            });
        self.emit_byte(value_id_byte, location);

        for upvalue in upvalues {
            self.emit_bytes(upvalue.is_local, upvalue.index, location);
        }
    }

    /// A declaration introduced by one or more decorators; the target may
    /// be a function, a generator, or a class. One uniform scheme for all
    /// of them:
    ///
    /// 1. The decorator values are evaluated in source order and tracked
    ///    as anonymous locals, so everything the following declaration
    ///    declares (the `super` local, a local name) stays aligned with
    ///    the physical stack.
    /// 2. The plain declaration runs and binds its name as usual.
    /// 3. The chain is applied to loaded copies, innermost first, and the
    ///    result is written back over the binding
    ///    (`Name = d1(d2(Name))`).
    /// 4. At top level the temporaries are popped afterwards; in a scope
    ///    they die with it.
    ///
    /// Stack progression for `@d1 @d2 fun Name`, at local scope (at top
    /// level `Name` lives in the globals table instead of a slot):
    ///
    /// ```text
    /// [..]                                  start
    /// [.., d1, d2]                          decorator temporaries
    /// [.., d1, d2, Name]                    declaration; Name's slot
    /// [.., d1, d2, Name, d1, d2, Name]      loaded copies
    /// [.., d1, d2, Name, d1, d2(Name)]      innermost call
    /// [.., d1, d2, Name, d1(d2(Name))]      outermost call
    /// [.., d1, d2, result, result]          SetLocal peeks, writes Name's slot
    /// [.., d1, d2, result]                  Pop discards the leftover copy
    /// [..]                                  (top level only) temporaries popped
    /// ```
    ///
    /// Undecorated declarations skip all of this and compile plain.
    fn gen_decorated(
        &mut self,
        decorators: &[Expr],
        name: &str,
        location: Location,
        build: impl FnOnce(&mut Self),
    ) {
        if decorators.is_empty() {
            build(self);
            return;
        }

        let temporaries_base = self.locals().len();
        for decorator in decorators {
            self.gen_expr(decorator);
        }
        for _ in decorators {
            // `@` cannot start an identifier, so user code can never
            // resolve these; they are only addressed by slot.
            self.add_local(String::from("@decorator"), Mutability::Immutable);
            self.mark_initialized();
        }

        // Compile the plain declaration and bind its name.
        build(self);

        for offset in 0..decorators.len() {
            self.emit_local_get(temporaries_base + offset, location);
        }
        self.emit_named_read(name, location);
        // One `CALL 1` per decorator, innermost (closest to the definition)
        // first.
        for decorator in decorators.iter().rev() {
            self.emit_bytes(OpCode::Call, 1, OpcodeLocation::new(decorator.location));
        }
        self.emit_variable_set_and_pop(name, location);

        if *self.scope_depth() == 0 {
            for _ in decorators {
                self.emit_byte(OpCode::Pop, OpcodeLocation::new(location));
            }
            self.locals_mut().truncate(temporaries_base);
        }
    }

    fn gen_class(&mut self, decl: &ClassDecl) {
        self.gen_decorated(
            &decl.decorators,
            &decl.name,
            decl.name_location,
            |codegen| {
                codegen.gen_class_inner(decl);
            },
        );
    }

    /// The undecorated class body: declare and define the class, wire the
    /// superclass, class variables, and methods.
    fn gen_class_inner(&mut self, decl: &ClassDecl) {
        let name_constant = self.identifier_constant(&decl.name);
        self.declare_variable(&decl.name, Mutability::Mutable, decl.name_location);
        let class_name_index = ConstantIndex::try_from(name_constant).unwrap_or_else(|_| {
            self.error_at(
                decl.name_location,
                &decl.name,
                "Too many constants when declaring class.",
            );
            ConstantIndex(0)
        });
        self.emit_bytes(
            OpCode::Class,
            class_name_index,
            OpcodeLocation::new(decl.name_location),
        );
        self.define_variable(
            Some(name_constant),
            Mutability::Mutable,
            OpcodeLocation::new(decl.name_location),
        );

        if let Some((superclass, less_location)) = &decl.superclass {
            // The superclass expression is a plain identifier read.
            self.gen_expr(superclass);

            self.begin_scope();
            self.add_local(String::from("super"), Mutability::Mutable);
            self.define_variable(None, Mutability::Mutable, OpcodeLocation::default());

            self.emit_named_read(&decl.name, decl.name_location);
            self.emit_byte(
                OpCode::Inherit,
                OpcodeLocation {
                    preceding: Some(*less_location),
                    source: superclass.location,
                    following: None,
                },
            );
        }

        self.emit_named_read(&decl.name, decl.name_location);

        for member in &decl.members {
            match member {
                ClassMember::Variable(class_var) => self.gen_class_variable(class_var),
                ClassMember::Method(method) => self.gen_method(method),
            }
        }

        let close = OpcodeLocation::new(decl.name_location);
        self.emit_byte(OpCode::Pop, close);

        if decl.superclass.is_some() {
            self.end_scope(OpcodeLocation::default());
        }
    }

    /// A method member: intern its name, compile the function literal, then
    /// attach it to the class with `OP_METHOD`.
    fn gen_method(&mut self, method: &Method) {
        let name_constant = self.identifier_constant(&method.name);
        self.gen_function_def(&method.fn_lit, method.kind.into());
        let method_name_index = ConstantIndex::try_from(name_constant).unwrap_or_else(|_| {
            self.error_at(
                method.close_location,
                "}",
                "Too many constants when declaring method.",
            );
            ConstantIndex(0)
        });
        self.emit_bytes(
            OpCode::Method,
            method_name_index,
            OpcodeLocation::new(method.close_location),
        );
    }

    #[allow(clippy::too_many_lines)] // one arm per statement kind
    fn gen_stmt(&mut self, stmt: &StmtKind) {
        match stmt {
            StmtKind::VarDecl {
                name,
                mutability,
                initializer,
                name_location,
                ..
            } => self.gen_var_decl(name, *name_location, *mutability, initializer.as_deref()),
            StmtKind::FunctionDecl {
                name,
                kind,
                fn_lit,
                decorators,
            } => self.gen_decorated(decorators, name, fn_lit.fn_location, |codegen| {
                let global =
                    codegen.declare_or_intern(name, Mutability::Mutable, fn_lit.fn_location);
                codegen.gen_function_def(fn_lit, (*kind).into());
                codegen.define_variable(
                    global,
                    Mutability::Mutable,
                    OpcodeLocation::new(fn_lit.fn_location),
                );
            }),
            StmtKind::Class(decl) => self.gen_class(decl),
            StmtKind::Try(stmt) => self.gen_try(stmt),
            StmtKind::Throw {
                throw_location,
                expr,
            } => {
                self.gen_expr(expr);
                self.emit_byte(
                    OpCode::Throw,
                    OpcodeLocation {
                        preceding: Some(*throw_location),
                        source: expr.location,
                        following: None,
                    },
                );
            }
            StmtKind::Conditional {
                condition_type,
                condition,
                then_block,
                else_block,
                then_close_location,
                else_close_location,
                keyword_location,
            } => {
                // Structure is:
                //
                // if (condition) {
                //     "A": then block
                // } else {
                //     "B": else block
                // }
                //  "C" // Continue here after the conditional statement.
                self.gen_expr(condition);
                let condition_location = OpcodeLocation::new(condition.location);
                // If the condition is such that we do NOT want the "then" block,
                // we jump over it and continue at B. The new PopJumpIf opcodes
                // automatically pop the condition value.
                let then_jump = self.emit_jump(
                    match condition_type {
                        ConditionType::If => OpCode::PopJumpIfFalse,
                        ConditionType::Unless => OpCode::PopJumpIfTrue,
                    },
                    condition_location,
                );
                self.begin_scope();
                self.gen_declarations(then_block);
                self.end_scope(OpcodeLocation::new(*then_close_location));

                // And afterwards always jump over the "else" block to C.
                let else_jump = self.emit_jump(OpCode::Jump, condition_location);

                // If we do the jump over the "then" block, we need to patch it
                // to continue at "B" the "else" block. Which is here.
                self.patch_jump(
                    then_jump,
                    keyword_location.merge_ordered(then_close_location),
                    match condition_type {
                        ConditionType::If => "if",
                        ConditionType::Unless => "unless",
                    },
                );

                // If we have an "else" block, we parse it now.
                if let Some(else_block) = else_block {
                    self.begin_scope();
                    self.gen_declarations(else_block);
                    self.end_scope(OpcodeLocation::new(*else_close_location));
                }

                // And finally we patch the jump over the "else" block to continue at "C".
                let else_target = if else_block.is_some() {
                    else_close_location
                } else {
                    then_close_location
                };
                self.patch_jump(
                    else_jump,
                    keyword_location.merge_ordered(else_target),
                    match condition_type {
                        ConditionType::If => "if",
                        ConditionType::Unless => "unless",
                    },
                );
            }
            StmtKind::Block { stmts } => {
                self.begin_scope();
                self.gen_declarations(stmts);
                self.end_scope(OpcodeLocation::default());
            }
            StmtKind::Loop {
                loop_type,
                condition,
                body,
                label,
                body_close_location,
                keyword_location,
            } => {
                let jump_span = keyword_location.merge_ordered(body_close_location);
                let jump_token = match loop_type {
                    LoopType::While => "while",
                    LoopType::Until => "until",
                };
                // Add new loop state
                let start = CodeOffset(self.current_chunk_len());
                let depth = self.scope_depth();
                self.loop_state_mut().push(LoopState::new(
                    depth,
                    start,
                    label.clone(),
                    *keyword_location,
                ));

                self.gen_expr(condition);
                let condition_location = OpcodeLocation::new(condition.location);
                let exit_jump = self.emit_jump(
                    match loop_type {
                        LoopType::While => OpCode::PopJumpIfFalse,
                        LoopType::Until => OpCode::PopJumpIfTrue,
                    },
                    condition_location,
                );
                self.begin_scope();
                self.gen_declarations(body);
                let close = OpcodeLocation::new(*body_close_location);
                self.end_scope(close);
                let loop_start = self.last_loop_state().as_ref().unwrap().start;
                self.emit_loop(loop_start, close, jump_span, jump_token);
                self.patch_jump(exit_jump, jump_span, jump_token);
                self.patch_break_jumps(*body_close_location);
                self.loop_state_mut().pop();
            }
            StmtKind::Continue {
                label,
                location: keyword_location,
            } => {
                let Some(loop_state) = self.loop_state_by_label_mut(label.as_deref()) else {
                    self.error("'continue' outside a loop.");
                    return;
                };
                let depth = loop_state.depth;
                let start = loop_state.start;
                let head_location = loop_state.head_location;
                let location = OpcodeLocation::default();

                // Calculate locals to drop
                let locals_to_drop = self
                    .locals()
                    .iter()
                    .rev()
                    .take_while(|local| local.depth > depth)
                    .count();

                // Emit pop instructions
                for _ in 0..locals_to_drop {
                    self.emit_byte(OpCode::Pop, location);
                }
                self.emit_loop(
                    start,
                    OpcodeLocation::new(*keyword_location),
                    head_location.merge_ordered(keyword_location),
                    "continue",
                );
            }
            StmtKind::Break { label } => {
                // First check if we're in a loop and get the depth
                let Some(loop_depth) = self.loop_depth_by_label(label.as_deref()) else {
                    self.error("'break' outside a loop.");
                    return;
                };
                let location = OpcodeLocation::default();

                // Calculate locals to drop
                let locals_to_drop = self
                    .locals()
                    .iter()
                    .rev()
                    .take_while(|local| local.depth > loop_depth)
                    .count();

                // Emit pop instructions
                for _ in 0..locals_to_drop {
                    self.emit_byte(OpCode::Pop, location);
                }

                // Now we can safely get the mutable reference and add the jump
                let jump = self.emit_jump(OpCode::Jump, location);
                self.loop_state_by_label_mut(label.as_deref())
                    .unwrap()
                    .break_jumps
                    .push(jump);
            }
            StmtKind::For {
                label,
                init,
                condition,
                increment,
                body,
                right_paren_location,
                body_close_location,
                keyword_location,
            } => self.gen_for(
                label.as_deref(),
                init.as_deref(),
                condition.as_deref(),
                increment.as_deref(),
                body,
                *right_paren_location,
                *body_close_location,
                *keyword_location,
            ),
            StmtKind::Foreach {
                label,
                mutability,
                var_name,
                iterable,
                body,
                var_name_location,
                in_location,
                close_location,
                body_close_location,
                keyword_location,
            } => self.gen_foreach(
                label.as_deref(),
                *mutability,
                var_name,
                *var_name_location,
                iterable,
                body,
                *in_location,
                *close_location,
                *body_close_location,
                *keyword_location,
            ),
            StmtKind::Return { value } => {
                if self.function_kind() == FunKind::Script {
                    self.error("Can't return from top-level code.");
                }
                match value {
                    None => self.emit_return(OpcodeLocation::default()),
                    Some(value) => {
                        if self.function_kind() == FunKind::Initializer {
                            self.error("Can't return a value from an initializer.");
                        } else if self.function_kind() == FunKind::Generator {
                            self.error("Can't return a value from a generator.");
                        }
                        self.gen_expr(value);
                        self.emit_byte(OpCode::Return, OpcodeLocation::new(value.location));
                    }
                }
            }
            StmtKind::Switch {
                subject,
                cases,
                subject_location,
                keyword_location,
            } => self.gen_switch(subject, *subject_location, cases, *keyword_location),
            StmtKind::Import {
                path,
                alias,
                path_location,
            } => self.gen_import(path, alias.as_deref(), *path_location),
            StmtKind::ImportFrom {
                path,
                names,
                path_location,
                statement_location,
            } => self.gen_import_from(path, names, *path_location, *statement_location),
            StmtKind::Expression { expr } => self.gen_expression_statement(expr),
        }
    }

    /// `try`/`catch`/`else` with the pending-exception protocol's jump
    /// structure.
    fn gen_try(&mut self, stmt: &TryStmt) {
        let location = OpcodeLocation::new(stmt.try_location);
        let mut first_catch_start = Some(self.emit_jump(OpCode::RegisterCatches, location));

        self.begin_scope();
        self.gen_declarations(&stmt.body);
        self.end_scope(OpcodeLocation::default());

        self.emit_byte(OpCode::PopHandler, OpcodeLocation::default());
        let jump_to_else = self.emit_jump(OpCode::Jump, location);
        let mut catch_jumps_to_end = Vec::new();
        let mut jump_to_next_catch: Option<CodeOffset> = None;

        for catch in &stmt.catches {
            if let Some(first_catch_offset) = first_catch_start {
                self.patch_jump(first_catch_offset, stmt.try_location, "try");
                first_catch_start = None;
            }

            self.begin_scope();

            // Declare space for where the exception variable will be sitting on the stack.
            self.add_local(String::from("@exception"), Mutability::Immutable);
            self.define_variable(None, Mutability::Immutable, OpcodeLocation::default());

            if let Some(jump) = jump_to_next_catch {
                self.patch_jump(jump, stmt.try_location, "try");
            }

            let mut jumps_to_catch_body = Vec::new();
            for matcher in &catch.matchers {
                self.gen_expr(matcher);
                let matcher_location = OpcodeLocation::new(matcher.location);
                // This compares the exception at STACK[-2] with the class at STACK[-1]
                // Pops the class and leaves the comparison result above the exception.
                self.emit_byte(OpCode::CompareException, matcher_location);
                jumps_to_catch_body.push(self.emit_jump(OpCode::PopJumpIfTrue, matcher_location));
            }

            jump_to_next_catch = Some(self.emit_jump(OpCode::Jump, location));

            for jump in jumps_to_catch_body {
                self.patch_jump(jump, stmt.try_location, "try");
            }

            if let Some((binding, binding_location)) = &catch.binding {
                let global = self.declare_or_intern(
                    binding.as_str(),
                    Mutability::Immutable,
                    *binding_location,
                );
                self.emit_named_read("@exception", stmt.try_location);
                self.define_variable(
                    global,
                    Mutability::Immutable,
                    OpcodeLocation::new(*binding_location),
                );
            }

            // The catch body lives in the catch scope itself; the single
            // `end_scope` below pops the `@exception` local - and the
            // binding, if any.
            self.gen_declarations(&catch.body);
            self.end_scope(OpcodeLocation::default());
            // We remove the exception with the end of the scope.
            // Leave nil on the stack to indicate to the reraise that we caught the exception.
            self.emit_byte(OpCode::Nil, OpcodeLocation::default());
            catch_jumps_to_end.push(self.emit_jump(OpCode::Jump, location));
        }

        self.patch_jump(jump_to_else, stmt.try_location, "try");

        // The else block runs scope-less.
        if let Some(else_block) = &stmt.else_block {
            self.gen_declarations(else_block);
        }
        let jump_over_reraise = self.emit_jump(OpCode::Jump, location);

        match jump_to_next_catch {
            Some(jump) => self.patch_jump(jump, stmt.try_location, "try"),
            None => self.error_at(
                stmt.try_location,
                "try",
                "Each try block needs at least one catch clause.",
            ),
        }
        for jump in catch_jumps_to_end {
            self.patch_jump(jump, stmt.try_location, "try");
        }
        self.emit_byte(OpCode::Reraise, location);

        self.patch_jump(jump_over_reraise, stmt.try_location, "try");

        // TODO: Add this back
        // Finally is tricky because it needs to interrupt not only exceptions
        // but also return, break and continue.
        // if self.match_(TK::Finally) {
        //     // Parse finally block
        //     self.consume(TK::LeftBrace, "Expect '{' after finally");
        //     self.block();
        // }
    }

    #[allow(clippy::too_many_arguments, clippy::too_many_lines)]
    fn gen_for(
        &mut self,
        label: Option<&str>,
        init: Option<&StmtKind>,
        condition: Option<&Expr>,
        increment: Option<&Expr>,
        body: &[StmtKind],
        right_paren_location: Location,
        body_close_location: Location,
        keyword_location: Location,
    ) {
        let paren = OpcodeLocation::new(right_paren_location);
        let close = OpcodeLocation::new(body_close_location);
        let jump_span = keyword_location.merge_ordered(&body_close_location);

        self.begin_scope();

        // Compile initializer, store loop variable
        let loop_var_name_mutable = match init {
            // No initializer
            None => None,
            Some(StmtKind::Expression { expr }) => {
                self.gen_expression_statement(expr);
                None
            }
            Some(StmtKind::VarDecl {
                name,
                name_location,
                mutability,
                initializer,
                semicolon_location,
            }) => {
                self.gen_var_decl_local(name, *name_location, *mutability, initializer.as_deref());
                // Challenge 25/2: alias loop variables
                u8::try_from(self.locals().len() - 1).map_or_else(
                    |_| {
                        self.error_at(
                            *semicolon_location,
                            ";",
                            "Creating loop variable led to too many locals.",
                        );
                        None
                    },
                    |loop_var| Some((loop_var, name.clone(), *mutability)),
                )
            }
            Some(_) => unreachable!("a for initializer is a declaration or an expression"),
        };

        // Add new loop state
        let start = CodeOffset(self.current_chunk_len());
        let depth = self.scope_depth();
        self.loop_state_mut().push(LoopState::new(
            depth,
            start,
            label.map(String::from),
            keyword_location,
        ));

        // Compile condition clause
        let exit_jump = condition.map(|condition| {
            self.gen_expr(condition);
            self.emit_jump(
                OpCode::PopJumpIfFalse,
                OpcodeLocation::new(condition.location),
            )
        });

        // Increment
        if let Some(increment) = increment {
            let body_jump = self.emit_jump(OpCode::Jump, paren);
            let increment_start = CodeOffset(self.current_chunk_len());
            self.gen_expr(increment);
            self.emit_byte(OpCode::Pop, paren);
            let loop_start = self.last_loop_state().as_ref().unwrap().start;
            self.emit_loop(
                loop_start,
                OpcodeLocation::new(right_paren_location),
                jump_span,
                "for",
            );
            self.last_loop_state_mut().as_mut().unwrap().start = increment_start;
            self.patch_jump(body_jump, jump_span, "for");
        }

        // Alias loop variable for this iteration of the loop
        let loop_and_inner_var =
            if let Some((loop_var, loop_var_name, mutability)) = loop_var_name_mutable {
                self.begin_scope();
                self.emit_bytes(OpCode::GetLocal, loop_var, paren);
                self.add_local(loop_var_name, mutability);
                self.mark_initialized();
                u8::try_from(self.locals().len() - 1).map_or_else(
                    |_| {
                        self.error_at(
                            right_paren_location,
                            ")",
                            "Aliasing loop variable led to too many locals.",
                        );
                        None
                    },
                    |inner_var| Some((loop_var, inner_var)),
                )
            } else {
                None
            };

        // Body.
        self.begin_scope();
        self.gen_declarations(body);
        self.end_scope(close);

        // Clean up alias for loop variable
        if let Some((loop_var, inner_var)) = loop_and_inner_var {
            self.emit_bytes(OpCode::GetLocal, inner_var, paren);
            self.emit_bytes(OpCode::SetLocal, loop_var, paren);
            self.emit_byte(OpCode::Pop, paren);
            self.end_scope(close);
        }

        let loop_start = self.last_loop_state().as_ref().unwrap().start;
        self.emit_loop(loop_start, close, jump_span, "for");

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump, jump_span, "for");
        }
        self.patch_break_jumps(body_close_location);
        self.loop_state_mut().pop();
        self.end_scope(close);
    }

    /// Loop:
    /// ```generic
    /// foreach (var val in my_list) {
    ///     print(val);
    /// }
    /// ```
    /// Should be identical to:
    /// ```generic
    /// {
    ///     var iter = my_list.__iter__();
    ///     var val = 0;
    ///
    ///     while (!((val = iter.__next__()) is StopIteration)) {
    ///         print(val);
    ///     }
    /// }
    /// ```
    /// Exhaustion is an identity (`is`) test against the `StopIteration`
    /// sentinel, never an `==` dispatch, so a user `__eq__` cannot end (or
    /// fail to end) the loop.
    #[allow(clippy::too_many_arguments)]
    fn gen_foreach(
        &mut self,
        label: Option<&str>,
        mutability: Mutability,
        var_name: &str,
        var_name_location: Location,
        iterable: &Expr,
        body: &[StmtKind],
        in_location: Location,
        close_location: Location,
        body_close_location: Location,
        keyword_location: Location,
    ) {
        let paren = OpcodeLocation::new(close_location);
        let iterable_location = OpcodeLocation::new(iterable.location);
        let close = OpcodeLocation::new(body_close_location);
        let jump_span = keyword_location.merge_ordered(&body_close_location);

        self.begin_scope();

        // Compile initializer, store loop variable
        // Define loop variable
        self.declare_variable(var_name, mutability, var_name_location);
        self.emit_byte(OpCode::Nil, paren);
        self.define_variable(None, mutability, OpcodeLocation::default());
        // Challenge 25/2: alias loop variables
        let loop_var = u8::try_from(self.locals().len() - 1).unwrap_or_else(|_| {
            self.error_at(
                in_location,
                "in",
                "Creating loop variable led to too many locals.",
            );
            0
        });

        // Get the iterable
        self.gen_expr(iterable);
        // Define the iterator
        self.invoke_fixed(
            "__iter__",
            0,
            "Too many constants created for OP_FOREACH.",
            iterable_location,
            close_location,
            ")",
        );
        // Do i even need this? This never actually needs a name.
        self.add_local(String::from("@iter"), Mutability::Mutable);
        self.define_variable(None, Mutability::Mutable, OpcodeLocation::default());
        let iter_var = u8::try_from(self.locals().len() - 1).unwrap_or_else(|_| {
            self.error_at(
                close_location,
                ")",
                "Creating loop iterator led to too many locals.",
            );
            0
        });

        // Store old loop state to restore at then end
        // Add new loop state
        let start = CodeOffset(self.current_chunk_len());
        let depth = self.scope_depth();
        self.loop_state_mut().push(LoopState::new(
            depth,
            start,
            label.map(String::from),
            keyword_location,
        ));

        // Compile the check clause as a call to "__next__" on the iterator
        // and an assignment to the loop variable
        // then check the loop variable for "StopIteration"
        // This is the loop check AND increment
        self.emit_bytes(OpCode::GetLocal, iter_var, paren);
        self.invoke_fixed(
            "__next__",
            0,
            "Too many constants created for OP_FOREACH.",
            iterable_location,
            close_location,
            ")",
        );
        self.emit_bytes(OpCode::SetLocal, loop_var, paren);
        // Exhaustion is a sentinel IDENTITY test: `Is` never dispatches to
        // a user `__eq__`, so iterating instances whose `__eq__` assumes
        // its own operand type works, and an always-true `__eq__` cannot
        // truncate the loop.
        self.emit_byte(OpCode::StopIteration, paren);
        self.emit_byte(OpCode::Is, paren);
        let exit_jump = self.emit_jump(OpCode::PopJumpIfTrue, iterable_location);

        // Alias loop variable for this iteration of the loop
        self.begin_scope();
        self.emit_bytes(OpCode::GetLocal, loop_var, paren);
        self.add_local(var_name.to_string(), mutability);
        self.mark_initialized();
        let inner_var = u8::try_from(self.locals().len() - 1).unwrap_or_else(|_| {
            self.error_at(
                close_location,
                ")",
                "Aliasing loop variable led to too many locals.",
            );
            0
        });

        // Body of the loop
        self.begin_scope();
        self.gen_declarations(body);
        self.end_scope(close);

        // Clean up alias for loop variable after the body.
        self.emit_bytes(OpCode::GetLocal, inner_var, paren);
        self.emit_bytes(OpCode::SetLocal, loop_var, paren);
        self.emit_byte(OpCode::Pop, paren);
        self.end_scope(close);

        // Loop back to the check/increment clause
        let loop_start = self.last_loop_state().as_ref().unwrap().start;
        self.emit_loop(loop_start, close, jump_span, "foreach");
        self.patch_jump(exit_jump, jump_span, "foreach");
        self.patch_break_jumps(body_close_location);
        self.loop_state_mut().pop();

        self.end_scope(close);
    }

    fn gen_switch(
        &mut self,
        subject: &Expr,
        subject_location: Location,
        cases: &[SwitchCase],
        keyword_location: Location,
    ) {
        let keyword_op_location = OpcodeLocation::new(keyword_location);
        let switch_var_location = OpcodeLocation::new(subject_location);

        self.gen_expr(subject);

        let mut end_jumps = Vec::new();

        for case in cases {
            // Check condition and build the jump over the case if false
            let miss_jump = case.value.as_ref().map(|value| {
                // Have to dup because equality check removes it
                self.emit_byte(OpCode::Dup, keyword_op_location);
                self.gen_expr(value);
                self.emit_byte(OpCode::Equal, switch_var_location);
                self.emit_jump(OpCode::PopJumpIfFalse, switch_var_location)
            });

            for stmt in &case.body {
                self.gen_stmt(stmt);
            }

            // If a branch has been entered then after it is finished we jump all the
            // way to the end of the switch
            end_jumps.push(self.emit_jump(OpCode::Jump, keyword_op_location));

            if let Some(miss_jump) = miss_jump {
                self.patch_jump(miss_jump, keyword_location, "switch");
            }
        }

        for end_jump in end_jumps {
            self.patch_jump(end_jump, keyword_location, "switch");
        }
        self.emit_byte(OpCode::Pop, keyword_op_location); // Get rid of switch value
    }

    fn gen_import(&mut self, path: &str, alias: Option<&str>, path_location: Location) {
        let location = OpcodeLocation::new(path_location);
        let is_local = *self.scope_depth() > 0;
        let path_constant = self.identifier_constant(path);

        if let Some(alias) = alias {
            let name_constant = self.identifier_constant(alias);
            if is_local {
                self.add_local(alias.to_string(), Mutability::Mutable);
                self.mark_initialized();
            }
            self.emit_byte(OpCode::ImportAs, location);
            if !self.emit_number(path_constant.0, NumberEncoding::Short, location) {
                self.error_at(
                    path_location,
                    path,
                    "Too many constants created for OP_IMPORT_AS.",
                );
            }
            if !self.emit_number(name_constant.0, NumberEncoding::Short, location) {
                self.error_at(
                    path_location,
                    path,
                    "Too many constants created for OP_IMPORT_AS.",
                );
            }
        } else {
            if is_local {
                let name = if let Some(stem) = get_file_stem(path) {
                    stem
                } else {
                    self.error_at(
                        path_location,
                        path,
                        &format!("Could not extract file stem from path: {path}"),
                    );
                    "unknown"
                };
                self.add_local(name.to_string(), Mutability::Mutable);
                self.mark_initialized();
            }
            self.emit_byte(OpCode::Import, location);
            if !self.emit_number(path_constant.0, NumberEncoding::Short, location) {
                self.error_at(
                    path_location,
                    path,
                    "Too many constants created for OP_IMPORT.",
                );
            }
        }
        self.emit_byte(is_local, location);
    }

    fn gen_import_from(
        &mut self,
        path: &str,
        names: &[(String, Location)],
        path_location: Location,
        statement_location: Location,
    ) {
        // One location covers the whole `from ... import ...`: a runtime
        // failure can originate from the module or from any of the names,
        // and the single `ImportFrom` opcode cannot tell which.
        // TODO: Report the missing module and each invalid name separately,
        // each pointing at its own span. That needs the VM to resolve the
        // names one at a time instead of reading them all up front.
        let location = OpcodeLocation::new(statement_location);
        let is_local = *self.scope_depth() > 0;
        let path_constant = self.identifier_constant(path);

        self.emit_byte(OpCode::ImportFrom, location);
        if !self.emit_number(path_constant.0, NumberEncoding::Short, location) {
            self.error_at(
                path_location,
                path,
                "Too many constants created for OP_IMPORT_FROM.",
            );
        }
        self.emit_byte(is_local, location);
        if !self.emit_number(names.len(), NumberEncoding::Short, location) {
            self.error_at(
                statement_location,
                "from",
                "Too many constants created for OP_IMPORT_FROM.",
            );
        }
        for (name, name_location) in names {
            let long_index = self.identifier_constant(name);
            if let Ok(short) = u8::try_from(*long_index) {
                self.emit_byte(short, location);
            } else {
                self.error_at(
                    *name_location,
                    name,
                    "Too many names to import from module.",
                );
            }
            if is_local {
                self.add_local(name.clone(), Mutability::Mutable);
                self.mark_initialized();
            }
        }
    }

    /// Compile an expression statement: evaluate it and discard the value.
    fn gen_expression_statement(&mut self, expr: &Expr) {
        self.gen_expr(expr);
        self.emit_byte(OpCode::Pop, OpcodeLocation::new(expr.location));
    }

    /// A class variable declaration inside a class body:
    /// `var name (: annotation)? (= default)? ;`. Compiles to the
    /// annotation and default expressions (nil placeholders when omitted)
    /// followed by `OP_CLASS_VARIABLE`, which records both on the class
    /// sitting below them on the stack. A `has_default` operand byte
    /// distinguishes an omitted default from an explicit `= nil`.
    fn gen_class_variable(&mut self, class_var: &ClassVar) {
        let name_constant = self.identifier_constant(&class_var.name);
        match &class_var.annotation {
            Some(annotation) => self.gen_expr(annotation),
            None => self.emit_byte(OpCode::Nil, OpcodeLocation::new(class_var.name_location)),
        }
        match &class_var.default {
            Some(default) => self.gen_expr(default),
            None => self.emit_byte(OpCode::Nil, OpcodeLocation::new(class_var.name_location)),
        }
        let name_index = ConstantIndex::try_from(name_constant).unwrap_or_else(|_| {
            self.error_at(
                class_var.semicolon_location,
                ";",
                "Too many constants when declaring class variable.",
            );
            ConstantIndex(0)
        });
        let semicolon = OpcodeLocation::new(class_var.semicolon_location);
        self.emit_bytes(OpCode::ClassVariable, name_index, semicolon);
        self.emit_byte(u8::from(class_var.default.is_some()), semicolon);
    }
}

// Expressions.
impl Codegen<'_, '_> {
    /// Compile an expression.
    ///
    /// An optional chain is a maximal run of postfix nodes (property,
    /// subscript, call) rooted at a non-postfix expression; it appears in
    /// the tree as nesting (`` a?.b.c `` is `` .c `` on `` ?.b `` on `` a ``).
    /// Each `?.`/`?[` link guards its receiver with a `JumpIfNil` whose
    /// target lies *outside* its own subtree (the chain's end), so the jump
    /// is recorded in `nullsafe_chain_jumps`. A null-safe chain link entered from a
    /// non-chain context is the chain root: it saves the surrounding
    /// frame, compiles the whole chain, then patches the collected jumps to
    /// skip past the chain's end. Only the root does this; local jump-out
    /// nodes (`and`/`or`, ternary) emit and patch their jumps within their
    /// own subtree.
    fn gen_expr(&mut self, expr: &Expr) {
        if expr.kind.is_nullsafe_chain_link() {
            let saved = std::mem::take(&mut self.nullsafe_chain_jumps);
            self.gen_expr_continue_nullsafe_chain(expr);
            let jumps = std::mem::replace(&mut self.nullsafe_chain_jumps, saved);
            for jump in jumps.into_iter().rev() {
                self.patch_jump(jump, expr.location, "?");
            }
        } else {
            self.gen_expr_continue_nullsafe_chain(expr);
        }
    }

    /// Compile one expression node without opening a chain scope. A chain
    /// link's receiver/callee stays in the caller's chain; operands, indices,
    /// arguments, and grouping re-enter [`Self::gen_expr`].
    #[allow(clippy::too_many_lines)] // one arm per expression kind
    fn gen_expr_continue_nullsafe_chain(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Unary {
                op,
                operand,
                op_location,
            } => {
                self.gen_expr(operand);
                let location = OpcodeLocation {
                    preceding: None,
                    source: *op_location,
                    following: Some(operand.location),
                };
                match op {
                    UnaryOp::Neg => self.emit_byte(OpCode::Negate, location),
                    UnaryOp::Not => self.emit_byte(OpCode::Not, location),
                }
            }
            ExprKind::Binary {
                op,
                lhs,
                rhs,
                op_location,
            } => self.gen_binary(*op, lhs, rhs, *op_location),
            ExprKind::Ternary {
                condition,
                then,
                else_,
                question_location,
            } => self.gen_ternary(condition, then, else_, *question_location),
            ExprKind::Lambda(lambda) => self.gen_lambda(lambda),
            ExprKind::Call {
                callee,
                args,
                args_location,
            } => self.gen_call(callee, args, *args_location),
            ExprKind::Property {
                object,
                name,
                mode,
                access_location,
                ..
            } => {
                self.gen_expr_continue_nullsafe_chain(object);
                if *mode == AccessMode::NullSafe {
                    self.emit_nullsafe_chain_jump(object.location);
                }
                let getter_location = OpcodeLocation {
                    preceding: Some(object.location),
                    source: *access_location,
                    following: None,
                };
                let name_constant = self.identifier_constant(name);
                self.emit_byte(OpCode::GetProperty, getter_location);
                if !self.emit_number(name_constant.0, NumberEncoding::Short, getter_location) {
                    self.error_at(
                        *access_location,
                        name,
                        "Too many constants created for OP_GET_PROPERTY.",
                    );
                }
            }
            ExprKind::Subscript {
                object,
                index,
                mode,
                access_location,
                ..
            } => {
                self.gen_expr_continue_nullsafe_chain(object);
                if *mode == AccessMode::NullSafe {
                    self.emit_nullsafe_chain_jump(object.location);
                }
                self.gen_expr(index);
                let getter_location = OpcodeLocation {
                    preceding: Some(object.location),
                    source: *access_location,
                    following: None,
                };
                self.invoke_fixed(
                    "__getitem__",
                    1,
                    "Too many constants created for OP_SUBSCRIPT.",
                    getter_location,
                    *access_location,
                    "]",
                );
            }
            ExprKind::Literal(lit) => self.gen_literal(lit, expr.location),
            ExprKind::Grouping { inner } => self.gen_expr(inner),
            ExprKind::Fstring { parts } => {
                for part in parts {
                    match part {
                        FstringPart::Str(value) => {
                            let string_id = self.heap.string_id(value);
                            self.emit_constant(
                                string_id.into(),
                                OpcodeLocation::new(expr.location),
                            );
                        }
                        FstringPart::Interp(interp) => self.gen_expr(interp),
                    }
                }
                let count = self.collection_count(parts.len(), "f-string");
                self.emit_bytes(
                    OpCode::BuildFstring,
                    count,
                    OpcodeLocation::new(expr.location),
                );
            }
            ExprKind::Tstring(tstring) => self.gen_tstring(tstring, expr.location),
            ExprKind::ListLit { items } => {
                for item in items {
                    self.gen_expr(item);
                }
                let count = self.collection_count(items.len(), "list");
                self.emit_bytes(OpCode::BuildList, count, OpcodeLocation::new(expr.location));
            }
            ExprKind::Tuple { elements } => {
                for element in elements {
                    self.gen_expr(element);
                }
                let count = self.collection_count(elements.len(), "tuple");
                self.emit_bytes(
                    OpCode::BuildTuple,
                    count,
                    OpcodeLocation::new(expr.location),
                );
            }
            ExprKind::SetLit { items } => {
                for item in items {
                    self.gen_expr(item);
                }
                let count = self.collection_count(items.len(), "set");
                self.emit_bytes(OpCode::BuildSet, count, OpcodeLocation::new(expr.location));
            }
            ExprKind::DictLit { entries } => {
                for (key, value) in entries {
                    self.gen_expr(key);
                    self.gen_expr(value);
                }
                let count = self.collection_count(entries.len(), "dict");
                self.emit_bytes(OpCode::BuildDict, count, OpcodeLocation::new(expr.location));
            }
            ExprKind::Logical {
                op,
                lhs,
                rhs,
                op_location,
            } => {
                self.gen_expr(lhs);
                let end_jump = self.emit_jump(
                    match op {
                        BoolOperator::And => OpCode::JumpIfFalseOrPop,
                        BoolOperator::Or => OpCode::JumpIfTrueOrPop,
                    },
                    OpcodeLocation {
                        preceding: None,
                        source: lhs.location,
                        following: Some(*op_location),
                    },
                );
                self.gen_expr(rhs);
                self.patch_jump(
                    end_jump,
                    op_location.merge_ordered(&rhs.location),
                    match op {
                        BoolOperator::And => "and",
                        BoolOperator::Or => "or",
                    },
                );
            }
            ExprKind::Variable { name, .. } => {
                self.emit_named_read(name, expr.location);
            }
            ExprKind::Super {
                method,
                super_location,
                access_location,
                ..
            } => {
                // The plain method lookup through the superclass.
                let name_constant = self.identifier_constant(method);
                self.emit_named_read("this", *super_location);
                let getter_location = OpcodeLocation {
                    preceding: Some(*super_location),
                    source: *access_location,
                    following: None,
                };
                self.emit_named_read("super", *super_location);
                self.emit_byte(OpCode::GetSuper, getter_location);
                if !self.emit_number(name_constant.0, NumberEncoding::Short, getter_location) {
                    self.error_at(
                        *access_location,
                        method,
                        "Too many constants while compiling OP_SUPER_INVOKE",
                    );
                }
            }
            ExprKind::Yield { operand } => {
                if let Some(operand) = operand {
                    self.gen_expr(operand);
                    self.emit_byte(
                        OpCode::Yield,
                        OpcodeLocation {
                            preceding: None,
                            source: expr.location,
                            following: Some(operand.location),
                        },
                    );
                } else {
                    let location = OpcodeLocation::new(expr.location);
                    self.emit_bytes(OpCode::Nil, OpCode::Yield, location);
                }
            }
            ExprKind::Assign {
                target,
                value,
                op_location,
            } => self.gen_assign(target, value, *op_location),
            ExprKind::AugAssign {
                target,
                op,
                value,
                op_location,
            } => self.gen_aug_assign(target, *op, value, *op_location),
        }
    }

    /// Compile a binary operation; operands are evaluated left to right.
    fn gen_binary(&mut self, op: BinaryOp, lhs: &Expr, rhs: &Expr, op_location: Location) {
        self.gen_expr(lhs);
        self.gen_expr(rhs);
        let location = OpcodeLocation {
            preceding: Some(lhs.location),
            source: op_location,
            following: Some(rhs.location),
        };
        // Emit the correct byte code to perform the operation on the two values
        match op {
            BinaryOp::Add => self.emit_byte(OpCode::Add, location),
            BinaryOp::Sub => self.emit_byte(OpCode::Subtract, location),
            BinaryOp::Mul => self.emit_byte(OpCode::Multiply, location),
            BinaryOp::Div => self.emit_byte(OpCode::Divide, location),
            BinaryOp::FloorDiv => self.emit_byte(OpCode::FloorDiv, location),
            BinaryOp::Mod => self.emit_byte(OpCode::Mod, location),
            BinaryOp::Exp => self.emit_byte(OpCode::Exp, location),
            BinaryOp::BitAnd => self.emit_byte(OpCode::BitAnd, location),
            BinaryOp::BitOr => self.emit_byte(OpCode::BitOr, location),
            BinaryOp::BitXor => self.emit_byte(OpCode::BitXor, location),
            BinaryOp::Equal => self.emit_byte(OpCode::Equal, location),
            BinaryOp::NotEqual => self.emit_byte(OpCode::NotEqual, location),
            BinaryOp::Greater => self.emit_byte(OpCode::Greater, location),
            BinaryOp::GreaterEqual => self.emit_byte(OpCode::GreaterEqual, location),
            BinaryOp::Less => self.emit_byte(OpCode::Less, location),
            BinaryOp::LessEqual => self.emit_byte(OpCode::LessEqual, location),
            BinaryOp::Is => self.emit_byte(OpCode::Is, location),
            BinaryOp::Rational => self.emit_byte(OpCode::BuildRational, location),
            //  Could think about making these one opcode with a boolean operand
            BinaryOp::RangeInclusive => {
                self.emit_byte(OpCode::BuildRangeInclusive, location);
            }
            BinaryOp::RangeExclusive => {
                self.emit_byte(OpCode::BuildRangeExclusive, location);
            }
            BinaryOp::In => self.gen_in(lhs.location, rhs.location, op_location),
        }
    }

    /// Compile `condition ? then : else`.
    fn gen_ternary(
        &mut self,
        condition: &Expr,
        then: &Expr,
        else_: &Expr,
        question_location: Location,
    ) {
        self.gen_expr(condition);
        let condition_location = OpcodeLocation::new(condition.location);
        let then_jump = self.emit_jump(OpCode::PopJumpIfFalse, condition_location);
        // First value: We parse the "then" expression and jump over the "else" expression.
        // The condition has already been popped by PopJumpIfFalse.
        self.gen_expr(then);
        let else_jump = self.emit_jump(OpCode::Jump, condition_location);
        // Patch the jump over the "else" expression to continue after the end of the "then" expression.
        self.patch_jump(
            then_jump,
            question_location.merge_ordered(&then.location),
            "?",
        );
        self.gen_expr(else_);
        self.patch_jump(
            else_jump,
            question_location.merge_ordered(&else_.location),
            "?",
        );
    }

    /// Parse `X in Y` by invoking `Y.contains(X)`.
    ///
    /// The stack holds element then container; swapping leaves the container
    /// as the receiver, so `contains` is invoked with the element as its one
    /// argument.
    fn gen_in(&mut self, lhs_location: Location, rhs_location: Location, op_location: Location) {
        let collection_location = OpcodeLocation {
            preceding: Some(lhs_location.merge_ordered(&op_location)),
            source: rhs_location,
            following: None,
        };
        self.emit_byte(OpCode::Swap, collection_location);
        self.invoke_fixed(
            "contains",
            1,
            "Too many constants created for OP_IN.",
            collection_location,
            rhs_location,
            "in",
        );
    }

    /// A call, with the callee deciding the shape: a property invokes a
    /// method directly, `super` invokes through the superclass, anything
    /// else is a plain call.
    fn gen_call(&mut self, callee: &Expr, args: &[Arg], args_location: Location) {
        match &callee.kind {
            // `object.name(args)`: invoke the method without materializing
            // it.
            ExprKind::Property {
                object, name, mode, ..
            } => {
                self.gen_expr_continue_nullsafe_chain(object);
                if *mode == AccessMode::NullSafe {
                    self.emit_nullsafe_chain_jump(object.location);
                }
                // The method name constant is interned before the
                // arguments.
                let name_constant = self.identifier_constant(name);
                let (arg_count, spreads) = self.gen_arguments(args);
                let invoke_location = OpcodeLocation {
                    preceding: Some(callee.location),
                    source: args_location,
                    following: None,
                };
                let opcode = if spreads.is_some() {
                    OpCode::InvokeUnpack
                } else {
                    OpCode::Invoke
                };
                self.emit_byte(opcode, invoke_location);
                if !self.emit_number(name_constant.0, NumberEncoding::Short, invoke_location) {
                    self.error_at(
                        callee.location,
                        name,
                        "Too many constants created for OP_INVOKE",
                    );
                }
                self.emit_byte(arg_count, invoke_location);
                if let Some(bitmap) = spreads {
                    self.emit_spread_bitmap(&bitmap, arg_count, invoke_location);
                }
            }
            // `super.name(args)`: invoke through the superclass.
            ExprKind::Super {
                method,
                super_location,
                ..
            } => {
                let name_constant = self.identifier_constant(method);
                self.emit_named_read("this", *super_location);
                let (arg_count, spreads) = self.gen_arguments(args);
                let invoke_location = OpcodeLocation {
                    preceding: Some(callee.location),
                    source: args_location,
                    following: None,
                };
                self.emit_named_read("super", *super_location);
                let opcode = if spreads.is_some() {
                    OpCode::SuperInvokeUnpack
                } else {
                    OpCode::SuperInvoke
                };
                self.emit_byte(opcode, invoke_location);
                if !self.emit_number(name_constant.0, NumberEncoding::Short, invoke_location) {
                    self.error_at(
                        callee.location,
                        method,
                        "Too many constants while compiling OP_SUPER_INVOKE",
                    );
                }
                self.emit_byte(arg_count, invoke_location);
                if let Some(bitmap) = spreads {
                    self.emit_spread_bitmap(&bitmap, arg_count, invoke_location);
                }
            }
            _ => {
                self.gen_expr_continue_nullsafe_chain(callee);
                let (arg_count, spreads) = self.gen_arguments(args);
                let location = OpcodeLocation {
                    preceding: Some(callee.location),
                    source: args_location,
                    following: None,
                };
                if let Some(bitmap) = spreads {
                    self.emit_bytes(OpCode::CallUnpack, arg_count, location);
                    self.emit_spread_bitmap(&bitmap, arg_count, location);
                } else {
                    self.emit_bytes(OpCode::Call, arg_count, location);
                }
            }
        }
    }

    /// Parse a call's argument list. Each argument leaves one value on the
    /// stack (a spread `*expr` leaves the iterable itself). Returns the segment
    /// count and, when any argument is a spread, a per-segment flag list so the
    /// caller can emit an unpacking call; `None` means a plain call.
    fn gen_arguments(&mut self, args: &[Arg]) -> (u8, Option<[u8; SPREAD_BITMAP_BYTES]>) {
        // Spread bitmap: segment `i` sets bit `i % 8` of byte `i / 8`. A fixed
        // stack array covers the full 255-argument range (32 bytes); only the
        // `ceil(arg_count / 8)` used bytes are emitted after the opcode.
        let mut spread_bitmap = [0u8; SPREAD_BITMAP_BYTES];
        for (i, arg) in args.iter().enumerate() {
            self.gen_expr(&arg.value);
            if arg.is_spread {
                spread_bitmap[i / 8] |= 1u8 << (i % 8);
            }
        }
        // An all-zero bitmap means no spreads, so a plain call.
        let spreads = spread_bitmap
            .iter()
            .any(|&byte| byte != 0)
            .then_some(spread_bitmap);
        (u8::try_from(args.len()).unwrap_or(255), spreads)
    }

    /// Emit one `?.`/`?[` link's nil-guard: when the receiver currently
    /// on the stack is nil, jump to the end of the whole chain (the nil
    /// stays on the stack as the chain's value). The jump's target lies
    /// outside the link's own subtree, so it is recorded in the open
    /// null-safe chain frame for the chain's root to patch.
    fn emit_nullsafe_chain_jump(&mut self, receiver_location: Location) {
        let jump = self.emit_jump(
            OpCode::JumpIfNil,
            OpcodeLocation {
                preceding: None,
                source: receiver_location,
                following: None,
            },
        );
        self.nullsafe_chain_jumps.push(jump);
    }

    fn gen_literal(&mut self, lit: &Literal, location: Location) {
        let location = OpcodeLocation::new(location);
        match lit {
            Literal::Nil => self.emit_byte(OpCode::Nil, location),
            Literal::True => self.emit_byte(OpCode::True, location),
            Literal::False => self.emit_byte(OpCode::False, location),
            Literal::StopIteration => self.emit_byte(OpCode::StopIteration, location),
            Literal::Int(value) => self.emit_constant((*value).into(), location),
            Literal::Float(value) => self.emit_constant((*value).into(), location),
            Literal::Str(value) => {
                let string_id = self.heap.string_id(value);
                self.emit_constant(string_id.into(), location);
            }
            Literal::BigInt(bigint) => {
                let value = self.heap.add_big_int(bigint.clone());
                self.emit_constant(value, location);
            }
        }
    }

    /// Compile a t-string: the interpolation tuple, the static parts, and the
    /// template that combines them.
    fn gen_tstring(&mut self, tstring: &Tstring, location: Location) {
        let location = OpcodeLocation::new(location);
        for interp in &tstring.interpolations {
            self.gen_expr(&interp.value);
            let source_id = self.heap.string_id(&interp.source_text);
            self.emit_constant(source_id.into(), location);
            // Emit BuildInterpolation opcode
            self.emit_byte(OpCode::BuildInterpolation, location);
        }
        let count = self.collection_count(tstring.interpolations.len(), "t-string");
        // Build tuple of interpolations
        self.emit_bytes(OpCode::BuildTuple, count, location);
        // The static parts travel as one prebuilt Tuple instance.
        let parts: Vec<Value> = tstring
            .parts
            .iter()
            .map(|part| self.heap.string_id(part).into())
            .collect();
        let tuple = Tuple::new(parts);
        let instance = Instance::new(
            *self.heap.native_classes.get("Tuple").unwrap(),
            Some(tuple.into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.emit_constant(instance_value, location);
        self.emit_byte(OpCode::BuildTemplate, location);
    }

    fn collection_count(&mut self, count: usize, kind: &str) -> u8 {
        u8::try_from(count).unwrap_or_else(|_| {
            self.error(&format!(
                "Can't have more than 255 items in a {kind} literal."
            ));
            255
        })
    }

    /// `` target = value ``. The target's nodes carry the positions their
    /// getter and setter instructions need.
    fn gen_assign(&mut self, target: &Expr, value: &Expr, op_location: Location) {
        debug_assert!(
            matches!(
                &target.kind,
                ExprKind::Variable {
                    ctx: ExprContext::Store,
                    ..
                } | ExprKind::Property {
                    ctx: ExprContext::Store,
                    ..
                } | ExprKind::Subscript {
                    ctx: ExprContext::Store,
                    ..
                }
            ),
            "assignment targets must be in Store context"
        );
        match &target.kind {
            ExprKind::Variable { name, .. } => {
                let variable_location = target.location;
                let (_, mut set_op, arg) = self.resolve_variable_ops(name, variable_location);
                // Get or set? / Generate the code. (the parser decided; here
                // only the setter remains)
                // Support for more than u8::MAX variables in a scope
                let encoding = if arg > usize::from(u8::MAX) {
                    set_op = set_op.to_long();
                    NumberEncoding::Long
                } else {
                    NumberEncoding::Short
                };

                self.gen_expr(value);
                if matches!(set_op, OpCode::SetLocal | OpCode::SetLocalLong) {
                    self.check_local_const(arg, variable_location, name);
                }
                let setter_location = OpcodeLocation {
                    preceding: None,
                    source: variable_location,
                    following: Some(op_location),
                };
                self.emit_byte(set_op, setter_location);
                if !self.emit_number(arg, encoding, setter_location) {
                    self.error(&format!("Too many globals in {set_op:?}"));
                }
            }
            ExprKind::Property {
                object,
                name,
                access_location,
                ..
            } => {
                self.gen_expr(object);
                let name_constant = self.identifier_constant(name);
                self.gen_expr(value);
                let setter_location = OpcodeLocation {
                    preceding: Some(object.location),
                    source: *access_location,
                    following: Some(op_location),
                };
                self.emit_byte(OpCode::SetProperty, setter_location);
                if !self.emit_number(name_constant.0, NumberEncoding::Short, setter_location) {
                    self.error_at(
                        *access_location,
                        name,
                        "Too many constants created for OP_SET_PROPERTY",
                    );
                }
            }
            ExprKind::Subscript {
                object,
                index,
                access_location,
                ..
            } => {
                self.gen_expr(object);
                self.gen_expr(index);
                self.gen_expr(value);
                let setter_location = OpcodeLocation {
                    preceding: Some(object.location),
                    source: *access_location,
                    following: Some(op_location),
                };
                self.invoke_fixed(
                    "__setitem__",
                    2,
                    "Too many constants created for OP_SUBSCRIPT.",
                    setter_location,
                    *access_location,
                    "]",
                );
            }
            _ => unreachable!("assignment targets are variables, properties, or subscripts"),
        }
    }

    /// `` target op= value ``: read the target, combine with the value,
    /// write back. (`op` is one of the arithmetic compound operators.)
    #[allow(clippy::too_many_lines)] // one arm per assignment target shape
    fn gen_aug_assign(&mut self, target: &Expr, op: BinaryOp, value: &Expr, op_location: Location) {
        debug_assert!(
            matches!(
                &target.kind,
                ExprKind::Variable {
                    ctx: ExprContext::Store,
                    ..
                } | ExprKind::Property {
                    ctx: ExprContext::Store,
                    ..
                } | ExprKind::Subscript {
                    ctx: ExprContext::Store,
                    ..
                }
            ),
            "assignment targets must be in Store context"
        );
        match &target.kind {
            ExprKind::Variable { name, .. } => {
                let variable_location = target.location;
                let (mut get_op, mut set_op, arg) =
                    self.resolve_variable_ops(name, variable_location);
                // Support for more than u8::MAX variables in a scope
                let encoding = if arg > usize::from(u8::MAX) {
                    get_op = get_op.to_long();
                    set_op = set_op.to_long();
                    NumberEncoding::Long
                } else {
                    NumberEncoding::Short
                };

                let getter_location = OpcodeLocation::new(variable_location);
                self.emit_byte(get_op, getter_location);
                if !self.emit_number(arg, encoding, getter_location) {
                    self.error(&format!("Too many globals in {get_op:?}"));
                }
                self.gen_expr(value);
                let operator_centered_location = OpcodeLocation {
                    preceding: Some(variable_location),
                    source: op_location,
                    following: Some(value.location),
                };
                self.emit_arith(op, operator_centered_location);
                if matches!(set_op, OpCode::SetLocal | OpCode::SetLocalLong) {
                    self.check_local_const(arg, variable_location, name);
                }
                let setter_location = OpcodeLocation {
                    preceding: None,
                    source: variable_location,
                    following: Some(op_location),
                };
                self.emit_byte(set_op, setter_location);
                if !self.emit_number(arg, encoding, setter_location) {
                    self.error(&format!("Too many globals in {set_op:?}"));
                }
            }
            ExprKind::Property {
                object,
                name,
                access_location,
                ..
            } => {
                self.gen_expr(object);
                let getter_location = OpcodeLocation {
                    preceding: Some(object.location),
                    source: *access_location,
                    following: None,
                };
                let name_constant = self.identifier_constant(name);
                self.emit_byte(OpCode::Dup, getter_location);
                self.emit_byte(OpCode::GetProperty, getter_location);
                if !self.emit_number(name_constant.0, NumberEncoding::Short, getter_location) {
                    self.error_at(
                        *access_location,
                        name,
                        "Too many constants created for OP_GET_PROPERTY.",
                    );
                }
                self.gen_expr(value);
                let operator_centered_location = OpcodeLocation {
                    preceding: Some(target.location),
                    source: op_location,
                    following: Some(value.location),
                };
                self.emit_arith(op, operator_centered_location);
                let setter_location = OpcodeLocation {
                    preceding: Some(object.location),
                    source: *access_location,
                    following: Some(op_location),
                };
                self.emit_byte(OpCode::SetProperty, setter_location);
                if !self.emit_number(name_constant.0, NumberEncoding::Short, setter_location) {
                    self.error_at(
                        *access_location,
                        name,
                        "Too many constants created for OP_SET_PROPERTY",
                    );
                }
            }
            ExprKind::Subscript {
                object,
                index,
                access_location,
                ..
            } => {
                self.gen_expr(object);
                self.gen_expr(index);
                let getter_location = OpcodeLocation {
                    preceding: Some(object.location),
                    source: *access_location,
                    following: None,
                };
                self.emit_bytes(OpCode::DupN, 2, getter_location);
                self.invoke_fixed(
                    "__getitem__",
                    1,
                    "Too many constants created for OP_SUBSCRIPT.",
                    getter_location,
                    *access_location,
                    "]",
                );
                self.gen_expr(value);
                let operator_centered_location = OpcodeLocation {
                    preceding: Some(object.location.merge_ordered(access_location)),
                    source: op_location,
                    following: Some(value.location),
                };
                self.emit_arith(op, operator_centered_location);
                let setter_location = OpcodeLocation {
                    preceding: Some(object.location),
                    source: *access_location,
                    following: Some(op_location),
                };
                self.invoke_fixed(
                    "__setitem__",
                    2,
                    "Too many constants created for OP_SUBSCRIPT.",
                    setter_location,
                    *access_location,
                    "]",
                );
            }
            _ => unreachable!("assignment targets are variables, properties, or subscripts"),
        }
    }

    /// The arithmetic of a compound assignment.
    fn emit_arith(&mut self, op: BinaryOp, location: OpcodeLocation) {
        match op {
            BinaryOp::Add => self.emit_byte(OpCode::Add, location),
            BinaryOp::Sub => self.emit_byte(OpCode::Subtract, location),
            BinaryOp::Mul => self.emit_byte(OpCode::Multiply, location),
            BinaryOp::Div => self.emit_byte(OpCode::Divide, location),
            BinaryOp::BitXor => self.emit_byte(OpCode::BitXor, location),
            BinaryOp::BitOr => self.emit_byte(OpCode::BitOr, location),
            BinaryOp::BitAnd => self.emit_byte(OpCode::BitAnd, location),
            BinaryOp::Mod => self.emit_byte(OpCode::Mod, location),
            _ => unreachable!("not a compound-assignment operator"),
        }
    }
}
