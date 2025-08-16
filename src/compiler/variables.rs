//! This module handles functionality related variables.
//!
//! Contains functions for resolving variables ad upvalue, local or global.

use super::{Compiler, Local, ScopeDepth, Upvalue};
use crate::{
    chunk::{ConstantLongIndex, OpCode},
    compiler::rules::Precedence,
    enums::{AssignmentCapability, ConstantSize, Mutability},
    scanner::{Token, TokenKind as TK},
};

impl<'scanner> Compiler<'scanner, '_> {
    /// Start a new scope.
    pub(super) fn begin_scope(&mut self) {
        **self.scope_depth_mut() += 1;
    }

    /// End the most recent scope.
    ///
    /// Handles all local variables with a depth larger than the new
    /// (outer) scope. Each local is either popped directly from the stack
    /// or turned into a closed upvalue.
    pub(super) fn end_scope(&mut self) {
        **self.scope_depth_mut() -= 1;
        let scope_depth = self.scope_depth();
        let mut instructions = vec![];
        let line = self.line();
        {
            let locals = self.locals_mut();
            while locals.last().is_some_and(|local| local.depth > scope_depth) {
                instructions.push(if locals.last().unwrap().is_captured {
                    OpCode::CloseUpvalue
                } else {
                    OpCode::Pop
                });
                locals.pop();
            }
        }
        for instruction in instructions {
            self.emit_byte(instruction, line);
        }
    }

    /// Handle a named variable based on the identifier of the last token.
    pub(super) fn variable(
        &mut self,
        assignment_capability: AssignmentCapability,
        _ignore_operators: &[TK],
    ) {
        self.named_variable(
            &self.previous.as_ref().unwrap().as_str().to_string(),
            assignment_capability,
        );
    }

    /// Handle a named variable with the given name.
    ///
    /// First checks of the name belongs to a local variable.
    /// If none is found, upvalues are searched, before finally it is assumed to
    /// belong to a global variable.
    ///
    /// Also handles whether the variable is to be gotten or set.
    pub(super) fn named_variable<S>(
        &mut self,
        name: &S,
        assignment_capability: AssignmentCapability,
    ) where
        S: ToString,
    {
        let line = self.line();
        let mut get_op = OpCode::GetLocal;
        let mut set_op = OpCode::SetLocal;
        let mut arg = self.resolve_local(name);

        // Upvalue?
        if arg.is_none()
            && let Some(upvalue_arg) = self.resolve_upvalue(name)
        {
            get_op = OpCode::GetUpvalue;
            set_op = OpCode::SetUpvalue;
            arg = Some(usize::from(upvalue_arg));
        }

        // If neither local nor upvalue, then it must be a global
        if arg.is_none() {
            arg = Some(*self.identifier_constant(name));
            get_op = OpCode::GetGlobal;
            set_op = OpCode::SetGlobal;
        }
        let arg = arg.unwrap();

        // Support for more than u8::MAX variables in a scope
        let size = if arg > u8::MAX.into() {
            get_op = get_op.to_long();
            set_op = set_op.to_long();
            ConstantSize::Long
        } else {
            ConstantSize::Short
        };

        // Get or set?
        let op = if assignment_capability == AssignmentCapability::CanAssign
            && (self.match_(TK::Equal)
                | self.match_(TK::PlusEqual)
                | self.match_(TK::MinusEqual)
                | self.match_(TK::StarEqual)
                | self.match_(TK::SlashEqual)
                | self.match_(TK::HatEqual)
                | self.match_(TK::PipeEqual)
                | self.match_(TK::AmperEqual)
                | self.match_(TK::PercentEqual))
        {
            let previous_kind = self.previous.as_ref().unwrap().kind;
            if matches!(previous_kind, TK::Equal) {
                self.expression();
            } else {
                self.emit_byte(get_op, line);
                if !self.emit_number(arg, size) {
                    self.error(&format!("Too many globals in {get_op:?}"));
                }
                self.expression();
                match previous_kind {
                    TK::PlusEqual => self.emit_byte(OpCode::Add, line),
                    TK::MinusEqual => self.emit_byte(OpCode::Subtract, line),
                    TK::StarEqual => self.emit_byte(OpCode::Multiply, line),
                    TK::SlashEqual => self.emit_byte(OpCode::Divide, line),
                    TK::HatEqual => self.emit_byte(OpCode::BitXor, line),
                    TK::PipeEqual => self.emit_byte(OpCode::BitOr, line),
                    TK::AmperEqual => self.emit_byte(OpCode::BitAnd, line),
                    TK::PercentEqual => self.emit_byte(OpCode::Mod, line),
                    _ => unreachable!("Unexpected byte code "),
                }
            }
            if set_op == OpCode::SetLocal || set_op == OpCode::SetLocalLong {
                self.check_local_const(arg);
            }
            set_op
        } else {
            get_op
        };

        // Generate the code.
        self.emit_byte(op, line);

        if !self.emit_number(arg, size) {
            self.error(&format!("Too many globals in {op:?}"));
        }
    }

    // Because the closure would need unique access to self while it
    // is mutably borrowed.
    /// Global values are constant names bound to values.
    /// If already a global with that name exists then its index is used.
    /// Otherwise a new global is created and the string constant corresponding
    /// to its name is added to the constant table.
    #[allow(clippy::option_if_let_else)]
    pub(super) fn identifier_constant<S>(&mut self, name: &S) -> ConstantLongIndex
    where
        S: ToString,
    {
        let string_id = self.heap.string_id(name);
        if let Some(index) = self.globals_by_name().get(&string_id) {
            *index
        } else {
            let index = self.current_chunk().make_constant(string_id.into());
            self.globals_by_name_mut().insert(string_id, index);
            index
        }
    }

    /// Resolve a local variable by its name.
    ///
    /// Iterate from the top of the locals backwards until one
    /// with that name is found. If the variable is the most recent one,
    /// than it is read during its own initializer, which is not allowed.
    ///
    /// If it is not found at all, then `None` is returned.
    fn resolve_local<S>(&mut self, name: &S) -> Option<usize>
    where
        S: ToString,
    {
        let name_string = name.to_string();
        let name = name_string.as_bytes();
        let retval = self
            .locals()
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| local.name.lexeme == name)
            .map(|(index, local)| {
                if *local.depth == -1 {
                    self.locals().len()
                } else {
                    index
                }
            });
        if retval == Some(self.locals().len()) {
            self.error("Can't read local variable in its own initializer.");
        }
        retval
    }

    pub(super) fn add_local(&mut self, name: Token<'scanner>, mutability: Mutability) {
        if self.locals().len() > usize::pow(2, 24) - 1 {
            self.error("Too many local variables in function.");
            return;
        }

        self.locals_mut().push(Local {
            name,
            depth: ScopeDepth(-1),
            mutable: bool::from(mutability),
            is_captured: false,
        });
    }

    /// Try to find the upvalue with the given name.
    ///
    /// Iterates up the scopes checking if a local or upvalue with that name
    /// exists. If nothing is found or the current scope is the outermost,
    /// then `None` is returned.
    fn resolve_upvalue<S>(&mut self, name: &S) -> Option<u8>
    where
        S: ToString,
    {
        if !self.has_enclosing() {
            return None;
        }

        if let Some(local) = self.in_enclosing(|compiler| compiler.resolve_local(name)) {
            self.in_enclosing(|compiler| compiler.locals_mut()[local].is_captured = true);
            return Some(self.add_upvalue(local, true));
        }

        if let Some(upvalue) = self.in_enclosing(|compiler| compiler.resolve_upvalue(name)) {
            return Some(self.add_upvalue(usize::from(upvalue), false));
        }

        None
    }

    /// Resolve an upvalue or add a new one if it does not yet exist.
    fn add_upvalue(&mut self, local_index: usize, is_local: bool) -> u8 {
        if let Ok(local_index) = u8::try_from(local_index) {
            // Return index if we already have it
            if let Some((upvalue_index, _)) =
                self.upvalues().iter().enumerate().find(|(_, upvalue)| {
                    upvalue.index == local_index && upvalue.is_local == is_local
                })
            {
                return u8::try_from(upvalue_index).unwrap();
            }

            if self.upvalues().len() > usize::from(u8::MAX) {
                self.error("Too many closure variables in function.");
                return 0;
            }

            // Record new upvalue
            self.upvalues_mut().push(Upvalue {
                index: local_index,
                is_local,
            });
            let upvalue_count = self.upvalues().len();
            self.current_function_mut().upvalue_count = upvalue_count;
            u8::try_from(upvalue_count - 1).unwrap()
        } else {
            // This is where `(Get|Set)UpvalueLong` would go
            self.error("Too many variables in function surrounding closure.");
            0
        }
    }

    /// Declare a variable in a local scope.
    ///
    /// If called from the top level, no action is taken.
    /// Otherwise it is checked if there already is an exiting local
    /// with that name. If so, an error is reported.
    /// Otherwise, the variable is added to the list of locals.
    pub(super) fn declare_variable(&mut self, mutability: Mutability) {
        if *self.scope_depth() == 0 {
            return;
        }

        let name = self.previous.clone().unwrap();
        let scope_depth = self.scope_depth();
        if self.locals_mut().iter().rev().any(|local| {
            if *local.depth != -1 && local.depth < scope_depth {
                false
            } else {
                local.name.lexeme == name.lexeme
            }
        }) {
            self.error("Already a variable with this name in this scope.");
        }

        self.add_local(name, mutability);
    }

    /// Parse a variable. If it is local, it gets declared normally.
    /// Otherwise, it is added as a global.
    pub(super) fn parse_variable(
        &mut self,
        msg: &str,
        mutability: Mutability,
    ) -> Option<ConstantLongIndex> {
        self.consume(TK::Identifier, msg);

        self.declare_variable(mutability);
        if *self.scope_depth() > 0 {
            None
        } else {
            Some(self.identifier_constant(&self.previous.as_ref().unwrap().as_str().to_string()))
        }
    }

    /// Mark the local as initialized.
    /// Normally on creation the depth of a local is set
    /// to `-1` to indicate that it has been declared but not initialized.
    /// Once properly initialized its depth is set to the scope depth
    /// of its defining scope.
    pub(super) fn mark_initialized(&mut self) {
        let scope_depth = self.scope_depth();
        if *scope_depth == 0 {
            return;
        }
        if let Some(local) = self.locals_mut().last_mut() {
            local.depth = scope_depth;
        }
    }

    /// Emit bytecode for defining a variable..
    ///
    /// If it is local it just gets marked as initialized.
    /// Otherwise, the `OpCode` matching the mutability and index size of the global index
    /// is emitted.
    pub(super) fn define_variable(
        &mut self,
        global: Option<ConstantLongIndex>,
        mutability: Mutability,
    ) {
        if *self.scope_depth() > 0 {
            self.mark_initialized();
            return;
        }

        let global = global.unwrap();

        if let Ok(short) = u8::try_from(*global) {
            if mutability == Mutability::Mutable {
                self.emit_byte(OpCode::DefineGlobal, self.line());
            } else {
                self.emit_byte(OpCode::DefineGlobalConst, self.line());
            }
            self.emit_byte(short, self.line());
        } else {
            if mutability == Mutability::Mutable {
                self.emit_byte(OpCode::DefineGlobalLong, self.line());
            } else {
                self.emit_byte(OpCode::DefineGlobalConstLong, self.line());
            }
            if !self.emit_24bit_number(*global) {
                self.error("Too many globals in define_global!");
            }
        }
    }

    /// Parse a list of arguments to a call.
    ///
    /// This means all comma separated expressions until a closing `)` is found.
    /// Does NOT permit trailing commas.
    pub(super) fn argument_list(&mut self) -> u8 {
        let mut arg_count = 0;
        if !self.check(TK::RightParen) {
            loop {
                self.parse_precedence_ignoring(Precedence::Assignment, &[TK::Comma]);

                if arg_count == 255 {
                    self.error("Can't have more than 255 arguments.");
                    break;
                }
                arg_count += 1;

                if !self.match_(TK::Comma) {
                    break;
                }
            }
        }
        self.consume(TK::RightParen, "Expect ')' after arguments.");
        arg_count
    }

    fn check_local_const(&mut self, local_index: usize) {
        let local = &self.locals()[local_index];
        if *local.depth != -1 && !local.mutable {
            self.error("Reassignment to local 'const'.");
        }
    }
}
