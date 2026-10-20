//! The parser's core: statements, declarations, and function literals
//! (expressions live in [`super::rules`], error handling in
//! [`super::error`]).

use super::ast::{
    CatchClause, ClassDecl, ClassMember, ClassVar, Expr, ExprContext, ExprKind, FunKind,
    FunctionDef, FunctionKind, InjectedBody, InjectedProgram, LambdaBody, LambdaLit, Literal,
    Method, MethodKind, Param, ParamList, Program, StmtKind, SwitchCase, TryStmt,
};
use super::rules::Precedence;

use rustc_hash::FxHashSet as HashSet;

use crate::{
    scanner::{Scanner, Token, TokenKind as TK},
    types::{ConditionType, InjectedKind, Location, LoopType, Mutability},
};

/// Per-function parse state: the function's kind (for the `return`/
/// `yield` validity checks) and the labels of its active loops.
pub(super) struct FunctionState {
    pub(super) kind: FunKind,
    loop_labels: Vec<Option<String>>,
}

impl FunctionState {
    fn new(kind: FunKind) -> Self {
        Self {
            kind,
            loop_labels: Vec::new(),
        }
    }
}

/// Keep track of the state of a class declaration
///
/// Similar to `LoopState`, this is needed for nested class declarations.
pub(super) struct ClassState {
    pub(super) has_superclass: bool,
    /// Class variable names declared in this class body, to reject
    /// duplicate declarations at compile time.
    class_variables: HashSet<String>,
}

impl ClassState {
    fn new() -> Self {
        Self {
            has_superclass: false,
            class_variables: HashSet::default(),
        }
    }
}

pub(crate) struct Parser<'scanner> {
    /// The scanner that provides tokens to the compiler.
    pub(super) scanner: Scanner<'scanner>,
    pub(super) previous: Option<Token<'scanner>>,
    pub(super) current: Option<Token<'scanner>>,

    pub(super) had_error: bool,
    pub(super) panic_mode: bool,
    /// Collected compile errors; printed to stderr by [`Self::compile`],
    /// returned to the caller by the collecting entry points (eval/exec
    /// wrap them in a catchable `SyntaxError`).
    pub(super) errors: Vec<String>,

    /// Function nesting, for `return`/`yield` validity checks. Each
    /// entry owns the loop-label stack of that function (loop labels
    /// are per-function).
    pub(super) function_stack: Vec<FunctionState>,

    /// Class nesting, for `this`/`super` validity checks. Not reset per
    /// function: methods nest inside their class.
    pub(super) class_stack: Vec<ClassState>,

    /// Whether the source is an embedded builtin; gates the
    /// `debug_parser` output.
    #[cfg(feature = "debug_parser")]
    pub(super) is_builtin: bool,
}

impl<'scanner> Parser<'scanner> {
    #[must_use]
    pub(crate) fn new(
        scanner: Scanner<'scanner>,
        #[cfg(feature = "debug_parser")] is_builtin: bool,
    ) -> Self {
        Self {
            scanner,
            previous: None,
            current: None,
            had_error: false,
            panic_mode: false,
            errors: Vec::new(),
            function_stack: vec![FunctionState::new(FunKind::Script)],
            class_stack: Vec::new(),
            #[cfg(feature = "debug_parser")]
            is_builtin,
        }
    }

    /// Parse a whole program: the top-level declaration loop.
    pub(crate) fn parse_program(&mut self) -> Program {
        self.advance();
        let mut declarations = Vec::new();
        while !self.match_(TK::Eof) {
            declarations.push(self.declaration());
        }
        Program { declarations }
    }

    /// Parse injected (`eval`/`exec`) source: the injected locals only
    /// matter to codegen (they become the function's leading slots), so
    /// this merely parses the body and places the arity check.
    pub(crate) fn parse_injected(
        &mut self,
        locals: &[String],
        kind: InjectedKind,
    ) -> InjectedProgram {
        self.advance();
        // Every local is filled by the caller, so all of them are
        // required and the closure carries no defaults; the callers
        // (`eval`/`exec`) reject more than 255 locals up front, so the
        // error here is a backstop (placed after `advance` so it
        // attaches to a token and is not dropped).
        if u8::try_from(locals.len()).is_err() {
            self.error("Can't inject more than 255 locals.");
        }
        let body = match kind {
            InjectedKind::Eval => {
                let expr = self.expression();
                self.consume(TK::Eof, "Expect end of expression.");
                InjectedBody::Eval(Box::new(expr))
            }
            InjectedKind::Exec => {
                let mut declarations = Vec::new();
                while !self.match_(TK::Eof) {
                    declarations.push(self.declaration());
                }
                InjectedBody::Exec(declarations)
            }
        };
        InjectedProgram {
            locals: locals.to_vec(),
            body,
        }
    }
}

// Scanner plumbing.
impl Parser<'_> {
    pub(super) fn advance(&mut self) {
        self.previous = std::mem::take(&mut self.current);
        loop {
            let token = self.scanner.scan();
            self.current = Some(token);
            #[cfg(feature = "debug_parser")]
            if cfg!(feature = "debug_parser_builtin") || !self.is_builtin {
                println!(
                    "Current: {:<25} Previous: {:<25} Panic: {:<5}",
                    format!("{:?}", self.current_token_kind()),
                    format!("{:?}", self.previous_token_kind()),
                    self.panic_mode
                );
            }
            if !self.check(TK::Error) {
                break;
            }
            // Could manually recursively inline `error_at_current` to get rid of this string copy,
            // but... this seems good enough, really.
            #[allow(clippy::unnecessary_to_owned)]
            self.error_at_current(&self.current.as_ref().unwrap().as_str().to_string());
        }
    }

    pub(super) fn consume(&mut self, kind: TK, msg: &str) {
        if self.check(kind) {
            self.advance();
            return;
        }
        self.error_at_current(msg);
    }

    pub(super) fn match_(&mut self, kind: TK) -> bool {
        if !self.check(kind) {
            return false;
        }
        self.advance();
        true
    }

    pub(super) fn check(&self, kind: TK) -> bool {
        self.current_token_kind() == Some(kind)
    }

    pub(super) fn current_token_kind(&self) -> Option<TK> {
        self.current.as_ref().map(|t| t.kind)
    }

    pub(super) fn check_previous(&self, kind: TK) -> bool {
        self.previous.as_ref().is_some_and(|t| t.kind == kind)
    }

    #[cfg(feature = "debug_parser")]
    fn previous_token_kind(&self) -> Option<TK> {
        self.previous.as_ref().map(|t| t.kind)
    }

    /// Parse a full expression (assignment precedence).
    pub(super) fn expression(&mut self) -> Expr {
        self.parse_precedence(Precedence::Assignment)
    }
}

// Statements.
impl Parser<'_> {
    fn declaration(&mut self) -> StmtKind {
        let kind = if self.match_(TK::Var) {
            self.var_declaration(Mutability::Mutable)
        } else if self.match_(TK::Const) {
            self.var_declaration(Mutability::Immutable)
        } else if self.check(TK::At) {
            self.decorated_declaration()
        } else if self.check(TK::Fun) || self.check(TK::Gen) {
            // `fun_declaration` consumes the keyword itself to know which
            // function type it declares, so the keyword is only peeked here.
            self.fun_declaration(Vec::new())
        } else if self.match_(TK::Class) {
            StmtKind::Class(Box::new(self.class_declaration(Vec::new())))
        } else {
            self.statement()
        };
        if self.panic_mode {
            self.synchronize();
        }
        kind
    }

    /// `var`/`const name (= init)?;` - also the initializer clause of a
    /// `for` loop, which reuses the same shape (including its `;`).
    fn var_declaration(&mut self, mutability: Mutability) -> StmtKind {
        let (name, name_location) = self.parse_variable("Expect variable name.");
        let initializer = self.match_(TK::Equal).then(|| Box::new(self.expression()));
        self.consume(TK::Semicolon, "Expect ';' after variable declaration.");
        let semicolon_location = self.location();
        StmtKind::VarDecl {
            name,
            mutability,
            initializer,
            name_location,
            semicolon_location,
        }
    }

    fn fun_declaration(&mut self, decorators: Vec<Expr>) -> StmtKind {
        let (context, kind) = if self.match_(TK::Gen) {
            (FunKind::Generator, FunctionKind::Generator)
        } else {
            self.consume(TK::Fun, "Expect function declaration.");
            (FunKind::Function, FunctionKind::Function)
        };
        let (name, _name_location) = self.parse_variable("Expect function name.");
        let fn_lit = self.parse_function_def(name.clone(), context);
        StmtKind::FunctionDecl {
            name,
            kind,
            fn_lit,
            decorators,
        }
    }

    /// One or more `` @decorator `` expressions in front of a function
    /// or class declaration.
    fn decorated_declaration(&mut self) -> StmtKind {
        let mut decorators = Vec::new();
        while self.match_(TK::At) {
            decorators.push(self.expression());
        }

        if self.match_(TK::Class) {
            StmtKind::Class(Box::new(self.class_declaration(decorators)))
        } else if self.check(TK::Fun) || self.check(TK::Gen) {
            self.fun_declaration(decorators)
        } else {
            self.error_at_current(
                "Expect function or class declaration or another decorator call after a decorator call.",
            );
            StmtKind::Expression {
                expr: Box::new(self.dummy_expr()),
            }
        }
    }

    /// Compile a class declaration (the `class` keyword is already
    /// consumed).
    fn class_declaration(&mut self, decorators: Vec<Expr>) -> ClassDecl {
        let (name, name_location) = self.parse_variable("Expect class name.");
        let mut members = Vec::new();

        // The class context must be active while the superclass clause and
        // the body parse.
        self.class_stack.push(ClassState::new());

        let superclass = self.match_(TK::Less).then(|| {
            let less_location = self.location();
            self.consume(TK::Identifier, "Expect superclass name.");
            let superclass_location = self.location();
            let superclass_name = self.previous.as_ref().unwrap().as_str().to_string();
            if name == superclass_name {
                self.error("A class can't inherit from itself.");
            }
            self.current_class_mut().unwrap().has_superclass = true;
            (
                Box::new(Expr {
                    location: superclass_location,
                    kind: ExprKind::Variable {
                        name: superclass_name,
                        ctx: ExprContext::Load,
                    },
                }),
                less_location,
            )
        });

        self.consume(TK::LeftBrace, "Expect '{' before class body.");
        while !self.check(TK::RightBrace) && !self.check(TK::Eof) {
            if self.match_(TK::Var) {
                members.push(ClassMember::Variable(self.class_variable_declaration()));
            } else {
                members.push(ClassMember::Method(self.method()));
            }
            // `method` does not consume tokens when the class body holds
            // something that cannot start a method, so without bailing here on
            // an error the loop would never make progress. Hand control back to
            // `declaration`, which owns synchronization.
            if self.panic_mode {
                break;
            }
        }
        self.consume(TK::RightBrace, "Expect '}' after class body.");
        self.class_stack.pop();

        ClassDecl {
            name,
            superclass,
            members,
            decorators,
            name_location,
        }
    }

    fn statement(&mut self) -> StmtKind {
        if self.match_(TK::If) || self.match_(TK::Unless) {
            let condition_type = if self.check_previous(TK::Unless) {
                ConditionType::Unless
            } else {
                ConditionType::If
            };
            self.conditional_statement(condition_type)
        } else if self.match_(TK::Try) {
            self.try_statement()
        } else if self.match_(TK::Throw) {
            self.throw_statement()
        } else if self.match_(TK::LeftBrace) {
            StmtKind::Block {
                stmts: self.block(),
            }
        } else if self.match_(TK::For) {
            self.for_statement()
        } else if self.match_(TK::ForEach) {
            self.foreach_statement()
        } else if self.match_(TK::While) || self.match_(TK::Until) {
            let loop_type = if self.check_previous(TK::Until) {
                LoopType::Until
            } else {
                LoopType::While
            };
            self.loop_statement(loop_type)
        } else if self.match_(TK::Continue) {
            self.continue_statement()
        } else if self.match_(TK::Break) {
            self.break_statement()
        } else if self.match_(TK::Return) {
            self.return_statement()
        } else if self.match_(TK::Switch) {
            self.switch_statement()
        } else if self.match_(TK::Import) {
            self.import_statement()
        } else if self.match_(TK::From) {
            self.import_from_statement()
        } else if self.match_(TK::Async) || self.match_(TK::Await) {
            self.error("Async and await are not yet implemented.");
            StmtKind::Expression {
                expr: Box::new(self.dummy_expr()),
            }
        } else if self.match_(TK::Finally) {
            self.error("Finally is not yet implemented.");
            StmtKind::Expression {
                expr: Box::new(self.dummy_expr()),
            }
        } else {
            self.expression_statement()
        }
    }

    fn try_statement(&mut self) -> StmtKind {
        let try_location = self.location();
        self.consume(TK::LeftBrace, "Expect '{' after try");
        let body = self.block();

        let mut catches = Vec::new();
        while self.match_(TK::Catch) {
            let matchers = if self.match_(TK::LeftParen) {
                let mut matchers = Vec::new();
                loop {
                    let expr = self.parse_precedence_ignoring(Precedence::Assignment, &[TK::Comma]);
                    matchers.push(expr);
                    if !self.match_(TK::Comma) {
                        break;
                    }
                }
                self.consume(TK::RightParen, "Expect ')' after exception types.");
                matchers
            } else {
                vec![self.expression()]
            };

            let binding = self.match_(TK::As).then(|| {
                let (name, location) = self.parse_variable("Expect exception variable name.");
                (name, location)
            });

            self.consume(TK::LeftBrace, "Expect '{' after catch clause");
            let catch_body = self.block();
            catches.push(CatchClause {
                matchers,
                binding,
                body: catch_body,
            });
        }

        let else_block = self.match_(TK::Else).then(|| {
            self.consume(TK::LeftBrace, "Expect '{' after else");
            self.block()
        });

        if catches.is_empty() {
            self.error("Each try block needs at least one catch clause.");
        }

        StmtKind::Try(Box::new(TryStmt {
            body,
            catches,
            else_block,
            try_location,
        }))
    }

    fn throw_statement(&mut self) -> StmtKind {
        let throw_location = self.location();
        let expr = Box::new(self.expression());
        self.consume(TK::Semicolon, "Expect ';' after throw expression.");
        StmtKind::Throw {
            throw_location,
            expr,
        }
    }

    /// Parse a conditional statement, either `if` or `unless`.
    fn conditional_statement(&mut self, condition_type: ConditionType) -> StmtKind {
        let keyword_location = self.location();
        // Parse the condition
        let condition = Box::new(self.expression());
        self.consume(TK::LeftBrace, "Expect '{' after condition");
        let then_block = self.block();
        let then_close_location = self.location();
        let else_block = self.match_(TK::Else).then(|| {
            self.consume(TK::LeftBrace, "Expect '{' after else");
            self.block()
        });
        let else_close_location = self.location();
        StmtKind::Conditional {
            condition_type,
            condition,
            then_block,
            else_block,
            then_close_location,
            else_close_location,
            keyword_location,
        }
    }

    fn block(&mut self) -> Vec<StmtKind> {
        let mut stmts = Vec::new();
        while !self.check(TK::RightBrace) && !self.check(TK::Eof) {
            stmts.push(self.declaration());
        }
        self.consume(TK::RightBrace, "Expect '}' after block.");
        stmts
    }

    fn loop_label(&mut self) -> Option<String> {
        self.match_(TK::Apostrophe).then(|| {
            self.consume(TK::Identifier, "Expect loop label after `'`.");
            self.previous.as_ref().unwrap().as_str().to_string()
        })
    }

    fn check_duplicate_loop_label(&mut self, label: Option<&str>) {
        if let Some(label) = label
            && self
                .function_stack
                .last()
                .unwrap()
                .loop_labels
                .iter()
                .any(|state| state.as_deref() == Some(label))
        {
            self.error(&format!("Duplicate loop label '{label}'."));
        }
    }

    fn push_loop_label(&mut self, label: Option<String>) {
        self.function_stack
            .last_mut()
            .unwrap()
            .loop_labels
            .push(label);
    }

    fn pop_loop_label(&mut self) {
        self.function_stack.last_mut().unwrap().loop_labels.pop();
    }

    fn find_loop_label(&self, label: Option<&str>) -> bool {
        let labels = &self.function_stack.last().unwrap().loop_labels;
        label.map_or_else(
            || !labels.is_empty(),
            |label| labels.iter().any(|state| state.as_deref() == Some(label)),
        )
    }

    fn loop_statement(&mut self, loop_type: LoopType) -> StmtKind {
        let keyword_location = self.location();
        let label = self.loop_label();
        self.check_duplicate_loop_label(label.as_deref());
        let condition = Box::new(self.expression());
        self.consume(TK::LeftBrace, "Expect '{' before loop body.");
        self.push_loop_label(label.clone());
        let body = self.block();
        let body_close_location = self.location();
        self.pop_loop_label();
        StmtKind::Loop {
            loop_type,
            condition,
            body,
            label,
            body_close_location,
            keyword_location,
        }
    }

    fn continue_statement(&mut self) -> StmtKind {
        let location = self.location();
        let label = self.loop_label();
        if !self.find_loop_label(label.as_deref()) {
            // Report and bail without consuming the `;` (its presence in
            // the stream drives the follow-up errors and synchronization).
            self.error("'continue' outside a loop.");
            return StmtKind::Expression {
                expr: Box::new(self.dummy_expr()),
            };
        }
        self.consume(TK::Semicolon, "Expect ';' after 'continue'.");
        StmtKind::Continue { label, location }
    }

    fn break_statement(&mut self) -> StmtKind {
        let label = self.loop_label();
        if !self.find_loop_label(label.as_deref()) {
            // Report and bail without consuming the `;` (its presence in
            // the stream drives the follow-up errors and synchronization).
            self.error("'break' outside a loop.");
            return StmtKind::Expression {
                expr: Box::new(self.dummy_expr()),
            };
        }
        self.consume(TK::Semicolon, "Expect ';' after 'break'.");
        StmtKind::Break { label }
    }

    fn for_statement(&mut self) -> StmtKind {
        let keyword_location = self.location();
        let label = self.loop_label();
        self.check_duplicate_loop_label(label.as_deref());
        self.consume(TK::LeftParen, "Expect label ('label) or '(' after 'for'.");

        // The initializer clause is a plain statement: a `var`/`const`
        // declaration or an expression.
        let init = if self.match_(TK::Semicolon) {
            None
        } else if self.match_(TK::Var) {
            Some(Box::new(self.var_declaration(Mutability::Mutable)))
        } else if self.match_(TK::Const) {
            Some(Box::new(self.var_declaration(Mutability::Immutable)))
        } else {
            Some(Box::new(self.expression_statement()))
        };

        let condition = if self.match_(TK::Semicolon) {
            None
        } else {
            let condition = Box::new(self.expression());
            self.consume(TK::Semicolon, "Expect ';' after loop condition.");
            Some(condition)
        };

        let increment = if self.match_(TK::RightParen) {
            None
        } else {
            let increment = Box::new(self.expression());
            self.consume(TK::RightParen, "Expect ')' after for clauses.");
            Some(increment)
        };
        let right_paren_location = self.location();

        self.consume(TK::LeftBrace, "Expect '{' before loop body.");
        self.push_loop_label(label.clone());
        let body = self.block();
        let body_close_location = self.location();
        self.pop_loop_label();

        StmtKind::For {
            label,
            init,
            condition,
            increment,
            body,
            right_paren_location,
            body_close_location,
            keyword_location,
        }
    }

    fn foreach_statement(&mut self) -> StmtKind {
        let keyword_location = self.location();
        let label = self.loop_label();
        self.check_duplicate_loop_label(label.as_deref());
        self.consume(
            TK::LeftParen,
            "Expect label ('label) or '(' after 'foreach'.",
        );

        if !(self.match_(TK::Var) || self.match_(TK::Const)) {
            self.error("Expect variable declaration ('var' or 'const') after '(' in 'foreach'.");
        }
        let mutability = if self.check_previous(TK::Var) {
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };
        // Define loop variable
        let (var_name, var_name_location) = self.parse_variable("Expect variable name.");
        self.consume(TK::In, "Expect 'in' after variable declaration.");
        let in_location = self.location();
        let iterable = Box::new(self.expression());
        self.consume(TK::RightParen, "Expect ')' after iterable.");
        let close_location = self.location();

        self.consume(TK::LeftBrace, "Expect '{' before loop body.");
        self.push_loop_label(label.clone());
        let body = self.block();
        let body_close_location = self.location();
        self.pop_loop_label();

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
        }
    }

    fn return_statement(&mut self) -> StmtKind {
        let context = self.function_stack.last().unwrap().kind;
        if context == FunKind::Script {
            self.error("Can't return from top-level code.");
        }
        if self.match_(TK::Semicolon) {
            return StmtKind::Return { value: None };
        }
        if context == FunKind::Initializer {
            self.error("Can't return a value from an initializer.");
        } else if context == FunKind::Generator {
            self.error("Can't return a value from a generator.");
        }
        let value = Box::new(self.expression());
        self.consume(TK::Semicolon, "Expect ';' after return value.");
        StmtKind::Return { value: Some(value) }
    }

    fn switch_statement(&mut self) -> StmtKind {
        let keyword_location = self.location();
        let subject = Box::new(self.expression());
        let subject_location = subject.location.merge_ordered(&self.current_location());
        self.consume(TK::LeftBrace, "Expect '{' before 'switch' body.");

        let mut cases = Vec::new();
        let mut had_default = false;
        // Loop cases and default until the file ends or the switch gets closed
        while !self.check(TK::RightBrace) && !self.check(TK::Eof) {
            if had_default {
                self.error_at_current("No 'case' or 'default' allowed after 'default' branch.");
            }
            let value = if self.match_(TK::Case) {
                let value = self.parse_precedence_ignoring(Precedence::Assignment, &[TK::Colon]);
                self.consume(TK::Colon, "Expect ':' after 'case' value.");
                Some(Box::new(value))
            } else {
                // The default case does not need to get jumped over
                self.consume(TK::Default, "Expect 'case' or 'default'.");
                self.consume(TK::Colon, "Expect ':' after 'default'");
                had_default = true;
                None
            };

            // Read the statements belonging to the current case
            let mut body = Vec::new();
            while !self.check(TK::RightBrace)
                && !self.check(TK::Case)
                && !self.check(TK::Default)
                && !self.check(TK::Eof)
            {
                body.push(self.statement());
                // A statement that cannot be parsed leaves the cursor unmoved;
                // stop consuming this case so the outer loop can bail and let
                // `declaration` synchronize instead of spinning here.
                if self.panic_mode {
                    break;
                }
            }
            cases.push(SwitchCase { value, body });
            // A case that cannot be parsed leaves the cursor unmoved; bail so the
            // enclosing `declaration` can synchronize instead of spinning here.
            if self.panic_mode {
                break;
            }
        }

        self.consume(TK::RightBrace, "Expect '}' after 'switch' body.");
        StmtKind::Switch {
            subject,
            cases,
            subject_location,
            keyword_location,
        }
    }

    fn import_statement(&mut self) -> StmtKind {
        self.consume(TK::String, "Expect import path as a string.");
        let path_token = self.previous.as_ref().unwrap();
        // Remove the quotation marks at the start and end
        let path = path_token.as_str()[1..path_token.as_str().len() - 1].to_string();
        let path_location = self.location();

        let alias = self
            .match_(TK::As)
            .then(|| self.parse_variable("Expect name to import as.").0);
        self.consume(
            TK::Semicolon,
            if alias.is_some() {
                "Expect ';' after import alias"
            } else {
                "Expect ';' after imported file path."
            },
        );
        StmtKind::Import {
            path,
            alias,
            path_location,
        }
    }

    fn import_from_statement(&mut self) -> StmtKind {
        // `from` was just consumed by the caller.
        let from_location = self.location();
        self.consume(TK::String, "Expect import path as a string.");
        let path_location = self.location();
        let path_token = self.previous.as_ref().unwrap();
        // Remove the quotation marks at the start and end
        let path = path_token.as_str()[1..path_token.as_str().len() - 1].to_string();
        self.consume(TK::Import, "Expect 'import' after imported file path.");

        let (first_name, first_location) = self.parse_variable("Expect name to import.");
        let mut names = vec![(first_name, first_location)];
        let mut last_name_location = first_location;
        while self.match_(TK::Comma) {
            let (name, location) = self.parse_variable("Expect name to import.");
            last_name_location = location;
            names.push((name, location));
        }
        self.consume(TK::Semicolon, "Expect ';' after names to import name.");
        StmtKind::ImportFrom {
            path,
            names,
            path_location,
            statement_location: from_location.merge_ordered(&last_name_location),
        }
    }

    fn expression_statement(&mut self) -> StmtKind {
        let expr = Box::new(self.expression());
        self.consume(TK::Semicolon, "Expect ';' after expression.");
        StmtKind::Expression { expr }
    }
}

// Function literals.
impl Parser<'_> {
    /// A declared function or method: a `fun`/`gen` declaration or a
    /// method. The body must be a block. `context` names the function's
    /// kind for the parse-time checks; the kind itself is named at the
    /// declaration site that called this.
    fn parse_function_def(&mut self, name: String, context: FunKind) -> FunctionDef {
        let fn_location = self.location();
        // The name local stays uninitialized while the parameter list
        // compiles: default expressions run in the enclosing scope before
        // the closure exists, so reading the name there is the
        // own-initializer error. Codegen marks it initialized ahead of
        // the body, which may self-reference for recursion.
        self.function_stack.push(FunctionState::new(context));

        self.consume(TK::LeftParen, "Expect '(' after function name.");
        let parameters = self.parse_params();

        let body = if self.match_(TK::LeftBrace) {
            self.block()
        } else {
            self.error_at_current("Expect '{' before function body.");
            Vec::new()
        };

        self.function_stack.pop();
        FunctionDef {
            name,
            parameters,
            body,
            fn_location,
        }
    }

    /// Parse lambda expressions.
    ///
    /// We support either single expressions, where no `return` is needed,
    /// or full blocks with a `return` statement.
    pub(super) fn parse_lambda(&mut self) -> LambdaLit {
        let fn_location = self.location();
        self.function_stack
            .push(FunctionState::new(FunKind::Lambda));

        self.consume(TK::LeftParen, "Expect '(' after function name.");
        let parameters = self.parse_params();

        let body = if self.match_(TK::LeftBrace) {
            LambdaBody::Block(self.block())
        } else {
            LambdaBody::Expression(Box::new(self.expression()))
        };

        self.function_stack.pop();
        LambdaLit {
            parameters,
            body,
            fn_location,
        }
    }

    /// Parse a parameter default in the enclosing function's context.
    ///
    /// Defaults run at definition time in the enclosing frame.
    fn parse_parameter_default(&mut self) -> Expr {
        let state = self.function_stack.pop().unwrap();
        // Non-assigning and no comma rule.
        let default = self.parse_precedence_ignoring(Precedence::non_assigning(), &[TK::Comma]);
        self.function_stack.push(state);
        default
    }

    fn parse_params(&mut self) -> ParamList {
        let mut params: Vec<Param> = Vec::new();
        let mut rest = None;
        if !self.check(TK::RightParen) {
            loop {
                if self.match_(TK::Star) {
                    // `*rest` collects surplus positional arguments; it is
                    // the last parameter and is not counted in the arity.
                    let (name, location) = self.parse_variable("Expect rest parameter name.");
                    if !self.check(TK::RightParen) {
                        self.error_at_current("A rest parameter must be the last parameter.");
                        // Discard the stray trailing parameters so the body
                        // still parses and no cascade errors follow.
                        while !self.check(TK::RightParen) && !self.check(TK::Eof) {
                            self.advance();
                        }
                    }
                    rest = Some(Param {
                        name,
                        default: None,
                        location,
                    });
                    break;
                }
                if params.len() == 255 {
                    self.error_at_current("Can't have more than 255 parameters.");
                }
                let (name, location) = self.parse_variable("Expect parameter name.");
                let default = if self.match_(TK::Equal) {
                    Some(Box::new(self.parse_parameter_default()))
                } else {
                    // Required parameters must form a prefix: once a default
                    // has appeared, `arity` outruns the required count, so a
                    // later required parameter shows up as the two diverging.
                    let required = params.iter().filter(|p| p.default.is_none()).count() + 1;
                    if required != params.len() + 1 {
                        self.error("A required parameter cannot follow an optional one.");
                    }
                    None
                };
                params.push(Param {
                    name,
                    default,
                    location,
                });
                if !self.match_(TK::Comma) {
                    break;
                }
            }
        }
        self.consume(TK::RightParen, "Expect ')' after parameters.");
        ParamList { params, rest }
    }
}

// Location helpers.
impl Parser<'_> {
    pub(super) fn location(&self) -> Location {
        self.previous
            .as_ref()
            .map_or_else(Location::default, |t| t.location)
    }

    pub(super) fn current_location(&self) -> Location {
        self.current
            .as_ref()
            .map_or_else(Location::default, |t| t.location)
    }

    pub(super) fn current_class(&self) -> Option<&ClassState> {
        self.class_stack.last()
    }

    pub(super) fn current_class_mut(&mut self) -> Option<&mut ClassState> {
        self.class_stack.last_mut()
    }

    pub(super) fn dummy_expr(&self) -> Expr {
        Expr {
            location: self.location(),
            kind: ExprKind::Literal(Literal::Nil),
        }
    }
}

// Class members.
impl Parser<'_> {
    /// `` var name (: annotation)? (= default)?; `` inside a class body.
    fn class_variable_declaration(&mut self) -> ClassVar {
        let (name, name_location) = self.parse_variable("Expect class variable name.");
        // Reject duplicate class variables at compile time, reported at
        // the repeated name.
        if let Some(class_state) = self.current_class_mut()
            && !class_state.class_variables.insert(name.clone())
        {
            self.error(&format!("Duplicate class variable '{name}'."));
        }
        let annotation = self.match_(TK::Colon).then(|| {
            // Non-assigning so the annotation stops before `= default`,
            // and no comma rule so it cannot swallow a bare tuple.
            Box::new(self.parse_precedence_ignoring(Precedence::non_assigning(), &[TK::Comma]))
        });
        let default = self.match_(TK::Equal).then(|| Box::new(self.expression()));

        self.consume(
            TK::Semicolon,
            "Expect ';' after class variable declaration.",
        );
        ClassVar {
            name,
            annotation,
            default,
            name_location,
            semicolon_location: self.location(),
        }
    }

    fn method(&mut self) -> Method {
        let (name, _name_location) = self.parse_variable("Expect method name.");
        let is_init = name == "__init__";
        let (context, kind) = if is_init {
            (FunKind::Initializer, MethodKind::Initializer)
        } else {
            (FunKind::Method, MethodKind::Method)
        };
        let fn_lit = self.parse_function_def(name.clone(), context);
        let close_location = self.location();
        Method {
            name,
            kind,
            fn_lit,
            close_location,
        }
    }
}
