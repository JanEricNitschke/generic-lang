//! Frontend of the compiler.
//!
//! Parses the tokens from the scanner to emit the correct bytecode for
//! declarations, statements and expressions.

use super::{ClassState, Compiler, FunctionType, LoopState, rules::Precedence};

use crate::{
    chunk::{CodeOffset, ConstantIndex, ConstantLongIndex, OpCode},
    scanner::{Token, TokenKind as TK},
    types::Line,
    utils::get_file_stem,
};

impl Compiler<'_, '_> {
    pub(super) fn advance(&mut self) {
        self.previous = std::mem::take(&mut self.current);
        loop {
            let token = self.scanner.scan();
            self.current = Some(token);
            #[cfg(feature = "debug_parser")]
            {
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
    pub(super) fn previous_token_kind(&self) -> Option<TK> {
        self.previous.as_ref().map(|t| t.kind)
    }

    /// Produce bytecode for parsing an expression.
    ///
    /// After that bytecode runs the resulting value will be on top of the stack.
    pub(super) fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    pub(super) fn declaration(&mut self) {
        if self.match_(TK::Var) {
            self.var_declaration(true);
        } else if self.match_(TK::Const) {
            self.var_declaration(false);
        } else if self.check(TK::Fun) || self.check(TK::At) {
            // We use `check` instead of `match_` here because
            // `fun_declaration` will handle the advance for us
            // because it needs to do so anyway for multiple decorators
            self.fun_declaration();
        } else if self.match_(TK::Class) {
            self.class_declaration();
        } else {
            self.statement();
        }
        if self.panic_mode {
            self.synchronize();
        }
    }

    fn var_declaration(&mut self, mutable: bool) {
        let global = self.parse_variable("Expect variable name.", mutable);

        if self.match_(TK::Equal) {
            self.expression();
        } else {
            self.emit_byte(OpCode::Nil, self.line());
        }

        self.consume(TK::Semicolon, "Expect ';' after variable declaration.");

        self.define_variable(global, mutable);
    }

    fn fun_declaration(&mut self) {
        let global = self.fun_definition();
        self.define_variable(global, true);
    }

    fn fun_definition(&mut self) -> Option<ConstantLongIndex> {
        if self.match_(TK::At) {
            self.decorated_fun_definition()
        } else if self.match_(TK::Fun) {
            self.undecorated_fun_definition()
        } else {
            self.error_at_current(
                "Expect function declaration or another decorator call after a decorator call.",
            );
            if *self.scope_depth() > 0 {
                None
            } else {
                Some(ConstantLongIndex(0))
            }
        }
    }

    fn decorated_fun_definition(&mut self) -> Option<ConstantLongIndex> {
        let line = self.line();
        self.expression();
        let fun_global = self.fun_definition();
        self.emit_bytes(OpCode::Call, 1, line);
        fun_global
    }

    fn undecorated_fun_definition(&mut self) -> Option<ConstantLongIndex> {
        let global = self.parse_variable("Expect function name.", true);
        self.mark_initialized();
        self.named_function(FunctionType::Function);
        global
    }

    fn class_declaration(&mut self) {
        self.consume(TK::Identifier, "Expect class name.");
        let class_name = self.previous.as_ref().unwrap().as_str().to_string();
        let name_constant = self.identifier_constant(&class_name);
        self.declare_variable(true);
        self.emit_bytes(
            OpCode::Class,
            ConstantIndex::try_from(name_constant)
                .expect("Too many constants when declaring class."),
            self.line(),
        );
        self.define_variable(Some(name_constant), true);
        self.class_state.push(ClassState::new());

        if self.match_(TK::Less) {
            self.consume(TK::Identifier, "Expect superclass name.");
            self.variable(false);

            if class_name == self.previous.as_ref().unwrap().as_str() {
                self.error("A class can't inherit from itself.");
            }

            self.begin_scope();
            self.add_local(self.synthetic_token(TK::Super), true);
            self.define_variable(None, true);

            self.named_variable(&class_name, true);
            self.emit_byte(OpCode::Inherit, self.line());
            self.current_class_mut().unwrap().has_superclass = true;
        }

        self.named_variable(&class_name, true);
        self.consume(TK::LeftBrace, "Expect '{' before class body.");
        while !self.check(TK::RightBrace) && !self.check(TK::Eof) {
            self.method();
        }
        self.consume(TK::RightBrace, "Expect '}' after class body.");
        self.emit_byte(OpCode::Pop, self.line());

        if self.current_class().unwrap().has_superclass {
            self.end_scope();
        }

        self.class_state.pop();
    }

    fn statement(&mut self) {
        if self.match_(TK::If) || self.match_(TK::Unless) {
            self.conditional_statement(self.check_previous(TK::If));
        } else if self.match_(TK::Try) {
            self.try_statement();
        } else if self.match_(TK::Throw) {
            self.throw_statement();
        } else if self.match_(TK::LeftBrace) {
            self.scoped_block();
        } else if self.match_(TK::For) {
            self.for_statement();
        } else if self.match_(TK::ForEach) {
            self.foreach_statement();
        } else if self.match_(TK::While) || self.match_(TK::Until) {
            self.loop_statement(self.check_previous(TK::While));
        } else if self.match_(TK::Continue) {
            self.continue_statement();
        } else if self.match_(TK::Break) {
            self.break_statement();
        } else if self.match_(TK::Return) {
            self.return_statement();
        } else if self.match_(TK::Switch) {
            self.switch_statement();
        } else if self.match_(TK::Import) {
            self.import_statement();
        } else if self.match_(TK::From) {
            self.import_from_statement();
        } else if self.match_(TK::Async) || self.match_(TK::Await) || self.match_(TK::Yield) {
            self.error("Async, await and yield are not yet implemented.");
        } else if self.match_(TK::Finally) {
            self.error("Finally is not yet implemented.");
        } else {
            self.expression_statement();
        }
    }

    fn try_statement(&mut self) {
        let mut first_catch_start = Some(self.emit_jump(OpCode::RegisterCatches));

        self.consume(TK::LeftBrace, "Expect '{' after try");
        self.scoped_block();

        self.emit_byte(OpCode::PopHandler, self.line());
        let jump_to_else = self.emit_jump(OpCode::Jump);
        let mut catch_jumps_to_end = Vec::new();
        let mut jump_to_next_catch = None;

        while self.match_(TK::Catch) {
            if let Some(first_catch_offset) = first_catch_start {
                self.patch_jump(first_catch_offset);
                first_catch_start = None;
            }

            self.begin_scope();

            let exception_var_name = "@exception";
            // Declare space for where the exception variable will be sitting on the stack.
            self.add_local(
                self.synthetic_identifier_token(exception_var_name.as_bytes()),
                false,
            );
            self.define_variable(None, false);

            if let Some(jump) = jump_to_next_catch {
                self.patch_jump(jump);
            }

            let mut jumps_to_catch_body = Vec::new();

            if self.match_(TK::LeftParen) {
                loop {
                    self.expression();
                    // This compares the exception at STACK[-2] with the class at STACK[-1]
                    // Pops the class and leaves the comparison result above the exception.
                    self.emit_byte(OpCode::CompareException, self.line());
                    jumps_to_catch_body.push(self.emit_jump(OpCode::PopJumpIfTrue));

                    if !self.match_(TK::Comma) {
                        break;
                    }
                }
                self.consume(TK::RightParen, "Expect ')' after exception types.");
            } else {
                self.expression();
                self.emit_byte(OpCode::CompareException, self.line());
                jumps_to_catch_body.push(self.emit_jump(OpCode::PopJumpIfTrue));
            }

            jump_to_next_catch = Some(self.emit_jump(OpCode::Jump));

            for jump in jumps_to_catch_body {
                self.patch_jump(jump);
            }

            if self.match_(TK::As) {
                let global = self.parse_variable("Expect exception variable name.", false);
                self.named_variable(&exception_var_name, false);
                self.define_variable(global, false);
            }

            self.consume(TK::LeftBrace, "Expect '{' after catch clause");
            self.block();
            self.end_scope();
            // We remove the exception with the end of the scope.
            // Leave nil on the stack to indicate the the reraise that we caught the exception.
            self.emit_byte(OpCode::Nil, self.line());
            catch_jumps_to_end.push(self.emit_jump(OpCode::Jump));
        }

        self.patch_jump(jump_to_else);

        if self.match_(TK::Else) {
            self.consume(TK::LeftBrace, "Expect '{' after else");
            self.block();
        }
        let jump_over_reraise = self.emit_jump(OpCode::Jump);

        match jump_to_next_catch {
            Some(jump) => self.patch_jump(jump),
            None => self.error("Each try block needs at least one catch clause."),
        }
        for jump in catch_jumps_to_end {
            self.patch_jump(jump);
        }
        self.emit_byte(OpCode::Reraise, self.line());

        self.patch_jump(jump_over_reraise);

        // TODO: Add this back
        // Finally is tricky because it needs to interrupt not only exceptions
        // but also return, break and continue.
        // if self.match_(TK::Finally) {
        //     // Parse finally block
        //     self.consume(TK::LeftBrace, "Expect '{' after finally");
        //     self.block();
        // }
    }

    fn throw_statement(&mut self) {
        self.expression();
        self.consume(TK::Semicolon, "Expect ';' after throw expression.");
        self.emit_byte(OpCode::Throw, self.line());
    }

    /// Parse a conditional statement, either `if` or `unless`.
    ///
    /// Structure is:
    ///
    /// if (condition) {
    ///     "A": then block
    /// } else {
    ///     "B": else block
    /// }
    ///  "C" // Continue here after the conditional statement.
    fn conditional_statement(&mut self, if_statement: bool) {
        // Parse the condition
        self.expression();

        self.consume(TK::LeftBrace, "Expect '{' after condition");

        // If the condition is such that we do NOT want the "then" block,
        // we jump over it and continue at B. The new PopJumpIf opcodes
        // automatically pop the condition value.
        let then_jump = self.emit_jump(if if_statement {
            OpCode::PopJumpIfFalse
        } else {
            OpCode::PopJumpIfTrue
        });
        self.scoped_block();

        // And afterwards always jump over the "else" block to C.
        let else_jump = self.emit_jump(OpCode::Jump);

        // If we do the jump over the "then" block, we need to patch it
        // to continue at "B" the "else" block. Which is here.
        self.patch_jump(then_jump);

        // If we have an "else" block, we parse it now.
        if self.match_(TK::Else) {
            self.consume(TK::LeftBrace, "Expect '{' after else");
            self.scoped_block();
        }

        // And finally we patch the jump over the "else" block to continue at "C".
        self.patch_jump(else_jump);
    }

    fn scoped_block(&mut self) {
        self.begin_scope();
        self.block();
        self.end_scope();
    }

    fn block(&mut self) {
        while !self.check(TK::RightBrace) && !self.check(TK::Eof) {
            self.declaration();
        }

        self.consume(TK::RightBrace, "Expect '}' after block.");
    }

    fn loop_label(&mut self) -> Option<String> {
        if self.match_(TK::Apostrophe) {
            self.consume(TK::Identifier, "Expect loop label after `'`.");
            Some(self.previous.as_ref().unwrap().as_str().to_string())
        } else {
            None
        }
    }

    fn check_duplicate_loop_label(&mut self, label: Option<&str>) {
        if let Some(label) = label
            && self
                .loop_state()
                .iter()
                .any(|state| state.label.as_deref() == Some(label))
        {
            self.error(&format!("Duplicate loop label '{label}'."));
        }
    }

    fn loop_statement(&mut self, while_statement: bool) {
        // Add new loop state
        let start = CodeOffset(self.current_chunk_len());
        let depth = self.scope_depth();
        let label = self.loop_label();
        self.check_duplicate_loop_label(label.as_deref());
        self.loop_state_mut()
            .push(LoopState::new(depth, start, label));

        self.expression();
        self.consume(TK::LeftBrace, "Expect '{' before loop body.");

        let exit_jump = self.emit_jump(if while_statement {
            OpCode::PopJumpIfFalse
        } else {
            OpCode::PopJumpIfTrue
        });
        self.scoped_block();
        let loop_start = self.last_loop_state().as_ref().unwrap().start;
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.patch_break_jumps();
        self.loop_state_mut().pop();
    }

    fn continue_statement(&mut self) {
        let line = self.line();
        let label = self.loop_label();

        match self.loop_state_by_label(label.as_deref()) {
            None => self.error("'continue' outside a loop."),
            Some(state) => {
                let depth = state.depth;
                let start = state.start;
                self.consume(TK::Semicolon, "Expect ';' after 'continue'.");
                let locals_to_drop = self
                    .locals()
                    .iter()
                    .rev()
                    .take_while(|local| local.depth > depth)
                    .count();
                for _ in 0..locals_to_drop {
                    self.emit_byte(OpCode::Pop, line);
                }

                self.emit_loop(start);
            }
        }
    }

    fn break_statement(&mut self) {
        let line = self.line();
        let label = self.loop_label();

        // First check if we're in a loop and get the depth
        let loop_depth = match self.loop_state_by_label(label.as_deref()) {
            None => {
                self.error("'break' outside a loop.");
                return;
            }
            Some(state) => state.depth,
        };

        self.consume(TK::Semicolon, "Expect ';' after 'break'.");

        // Calculate locals to drop
        let locals_to_drop = self
            .locals()
            .iter()
            .rev()
            .take_while(|local| local.depth > loop_depth)
            .count();

        // Emit pop instructions
        for _ in 0..locals_to_drop {
            self.emit_byte(OpCode::Pop, line);
        }

        // Now we can safely get the mutable reference and add the jump
        let jump = self.emit_jump(OpCode::Jump);
        self.loop_state_by_label_mut(label.as_deref())
            .unwrap()
            .break_jumps
            .push(jump);
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        let label = self.loop_label();
        self.check_duplicate_loop_label(label.as_deref());
        self.consume(TK::LeftParen, "Expect label ('label) or '(' after 'for'.");
        let line = self.line();

        // Compile initializer, store loop variable
        let loop_var_name_mutable = if self.match_(TK::Semicolon) {
            // No initializer
            None
        } else if self.match_(TK::Var) || self.match_(TK::Const) {
            let name = self.current.clone().unwrap();
            let is_mutable = self.check_previous(TK::Var);
            self.var_declaration(is_mutable);
            // Challenge 25/2: alias loop variables
            u8::try_from(self.locals().len() - 1).map_or_else(
                |_| {
                    self.error("Creating loop variable led to too many locals.");
                    None
                },
                |loop_var| Some((loop_var, name, is_mutable)),
            )
        } else {
            self.expression_statement();
            None
        };

        // Add new loop state
        let start = CodeOffset(self.current_chunk_len());
        let depth = self.scope_depth();
        self.loop_state_mut()
            .push(LoopState::new(depth, start, label));

        // Compile increment clause
        let exit_jump = (!self.match_(TK::Semicolon)).then(|| {
            self.expression();
            self.consume(TK::Semicolon, "Expect ';' after loop condition.");
            self.emit_jump(OpCode::PopJumpIfFalse)
        });

        // Increment
        if !self.match_(TK::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump);
            let increment_start = CodeOffset(self.current_chunk_len());
            self.expression();
            self.emit_byte(OpCode::Pop, line);
            self.consume(TK::RightParen, "Expect ')' after for clauses.");

            let loop_start = self.last_loop_state().as_ref().unwrap().start;
            self.emit_loop(loop_start);
            self.last_loop_state_mut().as_mut().unwrap().start = increment_start;
            self.patch_jump(body_jump);
        }

        // Alias loop variable for this iteration of the loop
        let loop_and_inner_var =
            if let Some((loop_var, loop_var_name, is_mutable)) = loop_var_name_mutable {
                self.begin_scope();
                self.emit_bytes(OpCode::GetLocal, loop_var, line);
                self.add_local(loop_var_name, is_mutable);
                self.mark_initialized();
                u8::try_from(self.locals().len() - 1).map_or_else(
                    |_| {
                        self.error("Aliasing loop variable led to too many locals.");
                        None
                    },
                    |inner_var| Some((loop_var, inner_var)),
                )
            } else {
                None
            };

        self.consume(TK::LeftBrace, "Expect '{' before loop body.");
        self.scoped_block();

        // Clean up alias for loop variable
        if let Some((loop_var, inner_var)) = loop_and_inner_var {
            self.emit_bytes(OpCode::GetLocal, inner_var, line);
            self.emit_bytes(OpCode::SetLocal, loop_var, line);
            self.emit_byte(OpCode::Pop, line);
            self.end_scope();
        }

        let loop_start = self.last_loop_state().as_ref().unwrap().start;
        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
        }

        self.patch_break_jumps();

        self.loop_state_mut().pop();
        self.end_scope();
    }

    /// Loop:
    /// foreach (var val in `my_list`) {
    ///     print(val);
    /// }
    /// Should be identical to:
    /// {
    ///     var iter = `my_list`.__iter__();
    ///     var val = 0;
    ///
    ///     while ((val = iter.__next__()) != StopIteration) {
    ///         print(val);
    ///     }
    /// }
    fn foreach_statement(&mut self) {
        self.begin_scope();
        let label = self.loop_label();
        self.check_duplicate_loop_label(label.as_deref());
        self.consume(
            TK::LeftParen,
            "Expect label ('label) or '(' after 'foreach'.",
        );
        let line = self.line();

        // Compile initializer, store loop variable
        if !(self.match_(TK::Var) || self.match_(TK::Const)) {
            self.error("Expect variable declaration ('var' or 'const') after '(' in 'foreach'.");
        }

        // Define loop variable
        let name = self.current.clone().unwrap(); // Needed for aliasing
        let is_mutable = self.check_previous(TK::Var);
        let global = self.parse_variable("Expect variable name.", is_mutable);
        self.emit_byte(OpCode::Nil, self.line());
        self.consume(TK::In, "Expect 'in' after variable declaration.");
        self.define_variable(global, is_mutable);
        // Challenge 25/2: alias loop variables
        let loop_var = u8::try_from(self.locals().len() - 1)
            .expect("Creating loop variable led to too many locals.");

        // Get the iterable
        self.expression();
        self.consume(TK::RightParen, "Expect ')' after iterable.");

        // Define the iterator
        self.emit_byte(OpCode::Invoke, line);
        let iter_constant = self.identifier_constant(&"__iter__");
        if !self.emit_number(iter_constant.0, false) {
            self.error("Too many constants created for OP_FOREACH.");
        }
        self.emit_byte(0, line);

        // Do i even need this? This never actually needs a name.
        self.add_local(self.synthetic_identifier_token(b"@iter"), true);
        self.define_variable(None, true);
        let iter_var = u8::try_from(self.locals().len() - 1)
            .expect("Creating loop iterator led to too many locals.");

        // Store old loop state to restore at then end
        // Add new loop state
        let start = CodeOffset(self.current_chunk_len());
        let depth = self.scope_depth();
        self.loop_state_mut()
            .push(LoopState::new(depth, start, label));

        // Compile the check clause as a call to "__next__" on the iterator
        // and an assignment to the loop variable
        // then check the loop variable for "StopIteration"
        // This is the loop check AND increment
        self.emit_bytes(OpCode::GetLocal, iter_var, line);
        self.emit_byte(OpCode::Invoke, line);
        let next_constant = self.identifier_constant(&"__next__");
        if !self.emit_number(next_constant.0, false) {
            self.error("Too many constants created for OP_FOREACH.");
        }
        self.emit_byte(0, line);
        self.emit_bytes(OpCode::SetLocal, loop_var, line);
        self.emit_byte(OpCode::StopIteration, line);
        self.emit_byte(OpCode::NotEqual, line);

        let exit_jump = self.emit_jump(OpCode::PopJumpIfFalse);

        // Alias loop variable for this iteration of the loop
        self.begin_scope();
        self.emit_bytes(OpCode::GetLocal, loop_var, line);
        self.add_local(name, is_mutable);
        self.mark_initialized();
        let inner_var = u8::try_from(self.locals().len() - 1)
            .expect("Aliasing loop variable led to too many locals.");

        // Body of the loop
        self.consume(TK::LeftBrace, "Expect '{' before loop body.");
        self.scoped_block();

        // Clean up alias for loop variable after the body.
        self.emit_bytes(OpCode::GetLocal, inner_var, line);
        self.emit_bytes(OpCode::SetLocal, loop_var, line);
        self.emit_byte(OpCode::Pop, line);
        self.end_scope();

        // Loop back to the check/increment clause
        let loop_start = self.last_loop_state().as_ref().unwrap().start;
        self.emit_loop(loop_start);
        self.patch_jump(exit_jump);
        self.patch_break_jumps();
        self.loop_state_mut().pop();

        self.end_scope();
    }

    fn return_statement(&mut self) {
        if self.function_type() == FunctionType::Script {
            self.error("Can't return from top-level code.");
        }
        if self.match_(TK::Semicolon) {
            self.emit_return();
        } else {
            if self.function_type() == FunctionType::Initializer {
                self.error("Can't return a value from an initializer.");
            }
            self.expression();
            self.consume(TK::Semicolon, "Expect ';' after return value.");
            self.emit_byte(OpCode::Return, self.line());
        }
    }

    fn switch_statement(&mut self) {
        self.expression();

        self.consume(TK::LeftBrace, "Expect '{' before 'switch' body.");

        let mut end_jumps = vec![];
        let mut had_default = false;

        // Loop cases and default until the file ends or  the switch gets closed
        while !self.check(TK::RightBrace) && !self.check(TK::Eof) {
            if had_default {
                self.error_at_current("No 'case' or 'default' allowed after 'default' branch.");
            }

            // Check condition and build the jump over the case if false
            let miss_jump = if self.match_(TK::Case) {
                // Have to dup because equality check removes it
                self.emit_byte(OpCode::Dup, self.line());
                self.expression();
                self.consume(TK::Colon, "Expect ':' after 'case' value.");
                self.emit_byte(OpCode::Equal, self.line());
                let jump = self.emit_jump(OpCode::PopJumpIfFalse);
                Some(jump)
            } else {
                // The default case does not need to get jumped over
                self.consume(TK::Default, "Expect 'case' or 'default'.");
                self.consume(TK::Colon, "Expect ':' after 'default'");
                had_default = true;
                None
            };

            // Read the statements belonging to the current case
            while !self.check(TK::RightBrace)
                && !self.check(TK::Case)
                && !self.check(TK::Default)
                && !self.check(TK::Eof)
            {
                self.statement();
            }

            // If a branch has been entered then after it is finished we jump all the
            // way to the end of the switch
            end_jumps.push(self.emit_jump(OpCode::Jump));

            if let Some(miss_jump) = miss_jump {
                self.patch_jump(miss_jump);
            }
        }

        for end_jump in end_jumps {
            self.patch_jump(end_jump);
        }
        self.emit_byte(OpCode::Pop, self.line()); // Get rid of switch value

        self.consume(TK::RightBrace, "Expect '}' after 'switch' body.");
    }

    #[allow(clippy::option_if_let_else)]
    fn import_statement(&mut self) {
        self.consume(TK::String, "Expect import path as a string.");
        let is_local = *self.scope_depth() > 0;
        let path_token = self.previous.clone().unwrap();

        let path_str = path_token.as_str();
        // Remove the quotation marks at the start and end
        let path = &path_str[1..path_str.len() - 1].to_string();
        let path_constant: crate::chunk::ConstantLongIndex = self.identifier_constant(&path);

        if self.match_(TK::As) {
            self.consume(TK::Identifier, "Expect name to import as.");
            let name_token = self.previous.clone().unwrap();
            let name = name_token.as_str().to_string();
            let name_constant = self.identifier_constant(&name);
            if is_local {
                self.add_local(name_token.clone(), true);
                self.mark_initialized();
            }
            self.consume(TK::Semicolon, "Expect ';' after import alias");
            self.emit_byte(OpCode::ImportAs, self.line());
            if !self.emit_number(path_constant.0, false) {
                self.error("Too many constants created for OP_IMPORT_AS.");
            }
            if !self.emit_number(name_constant.0, false) {
                self.error("Too many constants created for OP_IMPORT_AS.");
            }
        } else {
            if is_local {
                let name = if let Some(stem) =
                    get_file_stem(&path_token.lexeme[1..path_token.lexeme.len() - 1])
                {
                    stem
                } else {
                    self.error(&format!("Could not extract file stem from path: {path}"));
                    b"unknown"
                };
                self.add_local(
                    Token {
                        kind: path_token.kind,
                        lexeme: name,
                        line: path_token.line,
                    },
                    true,
                );

                self.mark_initialized();
            }
            self.consume(TK::Semicolon, "Expect ';' after imported file path.");
            self.emit_byte(OpCode::Import, self.line());
            if !self.emit_number(path_constant.0, false) {
                self.error("Too many constants created for OP_IMPORT.");
            }
        }
        self.emit_byte(is_local, self.line());
    }

    fn import_from_statement(&mut self) {
        self.consume(TK::String, "Expect import path as a string.");
        let is_local = *self.scope_depth() > 0;
        let path_str = self.previous.as_ref().unwrap().as_str();
        // Remove the quotation marks at the start and end
        let path = &path_str[1..path_str.len() - 1].to_string();
        let path_constant = self.identifier_constant(&path);
        self.consume(TK::Import, "Expect 'import' after imported file path.");

        let mut import_tokens = vec![];
        loop {
            // Best way would be to have these as variable length opcodes
            // with the indices pointing to the constants.
            // But i am not sure how to get that working with the disassembly,
            // or more generally where to put the length of the opcode.
            self.consume(TK::Identifier, "Expect name to import.");
            import_tokens.push(self.previous.as_ref().unwrap().clone());
            if !self.match_(TK::Comma) {
                break;
            }
        }
        self.consume(TK::Semicolon, "Expect ';' after names to import name.");
        self.emit_byte(OpCode::ImportFrom, self.line());
        if !self.emit_number(path_constant.0, false) {
            self.error("Too many constants created for OP_IMPORT_FROM.");
        }
        self.emit_byte(is_local, self.line());
        if !self.emit_number(import_tokens.len(), false) {
            self.error("Too many constants created for OP_IMPORT_FROM.");
        }
        for constant in import_tokens {
            let long_index = self.identifier_constant(&constant.as_str().to_string());
            if let Ok(short) = u8::try_from(*long_index) {
                self.emit_byte(short, self.line());
            } else {
                self.error("Too many names to import from module.");
            }
            if is_local {
                self.add_local(constant, true);
                self.mark_initialized();
            }
        }
    }

    fn expression_statement(&mut self) {
        let line = self.line();
        self.expression();
        self.consume(TK::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::Pop, line);
    }

    fn named_function(&mut self, function_type: FunctionType) {
        let function_name = self.previous.as_ref().unwrap().as_str().to_string();
        self.function(&function_name, function_type, false);
    }

    pub(super) fn function<S: ToString>(
        &mut self,
        function_name: &S,
        function_type: FunctionType,
        allow_expression_body: bool,
    ) {
        let line = self.line();
        let nested_state = self.nested(function_name, function_type, |compiler| {
            compiler.begin_scope();

            compiler.consume(TK::LeftParen, "Expect '(' after function name.");

            if !compiler.check(TK::RightParen) {
                loop {
                    if compiler.current_function().arity == 255 {
                        compiler.error_at_current("Can't have more than 255 parameters.");
                    } else {
                        compiler.current_function_mut().arity += 1;
                    }
                    let constant = compiler.parse_variable("Expect parameter name.", true);
                    compiler.define_variable(constant, true);
                    if !compiler.match_(TK::Comma) {
                        break;
                    }
                }
            }

            compiler.consume(TK::RightParen, "Expect ')' after parameters.");

            if compiler.match_(TK::LeftBrace) {
                compiler.block();
                compiler.end(false);
            } else if allow_expression_body {
                compiler.expression();
                compiler.end(true);
            } else {
                compiler.error_at_current("Expect '{' before function body.");
            }
        });

        let nested_function = nested_state.current_function;
        let nested_upvalues = nested_state.upvalues;

        self.emit_byte(OpCode::Closure, line);
        let function_id = self.heap.add_function(nested_function);
        let value_id_byte =
            u8::try_from(self.current_chunk().make_constant(function_id).0).unwrap();
        self.emit_byte(value_id_byte, line);

        for upvalue in nested_upvalues {
            self.emit_bytes(upvalue.is_local, upvalue.index, line);
        }
    }

    fn method(&mut self) {
        self.consume(TK::Identifier, "Expect method name.");
        let name_constant =
            self.identifier_constant(&self.previous.as_ref().unwrap().as_str().to_string());
        let function_type = if self.previous.as_ref().unwrap().lexeme == b"__init__" {
            FunctionType::Initializer
        } else {
            FunctionType::Method
        };
        self.named_function(function_type);
        self.emit_bytes(
            OpCode::Method,
            ConstantIndex::try_from(name_constant)
                .expect("Too many constants when declaring method."),
            self.line(),
        );
    }

    pub(super) fn line(&self) -> Line {
        self.previous.as_ref().map_or(Line(0), |x| x.line)
    }
}
