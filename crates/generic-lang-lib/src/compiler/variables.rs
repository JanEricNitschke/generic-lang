//! Variable resolution: scopes, locals, upvalues, and globals.
//!
//! Declarations must happen ahead of the initializer (so
//! own-initializer reads are errors), defines after it. Resolution
//! order, innermost first: local slot, then upvalue, then global.

use super::ast::{Arg, BinaryOp, Expr, ExprContext, ExprKind};
use super::back::{Codegen, Local, ScopeDepth, Upvalue};
use super::front::Parser;
use super::rules::Precedence;
use crate::{
    chunk::{ConstantLongIndex, OpCode},
    config::SPREAD_BITMAP_BYTES,
    scanner::TokenKind as TK,
    types::{Location, Mutability, NumberEncoding, OpcodeLocation},
};

impl Codegen<'_, '_> {
    /// Start a new scope.
    pub(super) fn begin_scope(&mut self) {
        **self.scope_depth_mut() += 1;
    }

    /// End the most recent scope.
    ///
    /// Handles all local variables with a depth larger than the new
    /// (outer) scope. Each local is either popped directly from the stack
    /// or turned into a closed upvalue.
    /// The pop instructions are emitted *after* the locals are removed
    /// (collect-then-emit).
    pub(super) fn end_scope(&mut self, location: OpcodeLocation) {
        **self.scope_depth_mut() -= 1;
        let scope_depth = self.scope_depth();
        let mut instructions = vec![];
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
            self.emit_byte(instruction, location);
        }
    }
}

impl Parser<'_> {
    /// Handle a named variable based on the identifier of the last token.
    pub(super) fn variable(&mut self, can_assign: bool, _ignore_operators: &[TK]) -> Expr {
        let name = self.previous.as_ref().unwrap().as_str().to_string();
        let location = self.location();
        self.named_variable(name, can_assign, location)
    }

    /// Handle a named variable with the given name.
    ///
    /// Also handles whether the variable is to be gotten or set.
    /// An identifier read: a load, or an assignment when `can_assign`
    /// and an assignment operator follows.
    pub(super) fn named_variable(
        &mut self,
        name: String,
        can_assign: bool,
        location: Location,
    ) -> Expr {
        if can_assign && self.match_(TK::Equal) {
            let op_location = self.location();
            let value = Box::new(self.expression());
            return Expr {
                location: location.merge_ordered(&value.location),
                kind: ExprKind::Assign {
                    target: Box::new(Expr {
                        location,
                        kind: ExprKind::Variable {
                            name,
                            ctx: ExprContext::Store,
                        },
                    }),
                    value,
                    op_location,
                },
            };
        }
        if can_assign && self.match_aug_operator() {
            let (op, op_location) = self.take_aug_operator();
            let value = Box::new(self.expression());
            return Expr {
                location: location.merge_ordered(&value.location),
                kind: ExprKind::AugAssign {
                    target: Box::new(Expr {
                        location,
                        kind: ExprKind::Variable {
                            name,
                            ctx: ExprContext::Store,
                        },
                    }),
                    op,
                    value,
                    op_location,
                },
            };
        }
        Expr {
            location,
            kind: ExprKind::Variable {
                name,
                ctx: ExprContext::Load,
            },
        }
    }

    /// Match a compound assignment operator (the plain `=` is matched
    /// separately, first).
    pub(super) fn match_aug_operator(&mut self) -> bool {
        self.match_(TK::PlusEqual)
            | self.match_(TK::MinusEqual)
            | self.match_(TK::StarEqual)
            | self.match_(TK::SlashEqual)
            | self.match_(TK::HatEqual)
            | self.match_(TK::PipeEqual)
            | self.match_(TK::AmperEqual)
            | self.match_(TK::PercentEqual)
    }

    /// Classify the just-consumed compound-assignment operator.
    pub(super) fn take_aug_operator(&self) -> (BinaryOp, Location) {
        let kind = self.previous.as_ref().unwrap().kind;
        let location = self.location();
        let op = match kind {
            TK::PlusEqual => BinaryOp::Add,
            TK::MinusEqual => BinaryOp::Sub,
            TK::StarEqual => BinaryOp::Mul,
            TK::SlashEqual => BinaryOp::Div,
            TK::HatEqual => BinaryOp::BitXor,
            TK::PipeEqual => BinaryOp::BitOr,
            TK::AmperEqual => BinaryOp::BitAnd,
            TK::PercentEqual => BinaryOp::Mod,
            _ => unreachable!("not a compound assignment operator"),
        };
        (op, location)
    }
}

impl Codegen<'_, '_> {
    /// First checks if the name belongs to a local variable.
    /// If none is found, upvalues are searched, before finally it is assumed to
    /// belong to a global variable.
    /// Resolve `name` against the current scopes, innermost first: local
    /// slot, then upvalue, then global constant index. Returns the matching
    /// get and set opcodes (short variants) along with the operand.
    pub(super) fn resolve_variable_ops(
        &mut self,
        name: &str,
        error_location: Location,
    ) -> (OpCode, OpCode, usize) {
        let mut get_op = OpCode::GetLocal;
        let mut set_op = OpCode::SetLocal;
        let mut arg = self.resolve_local(name, error_location);

        // Upvalue?
        if arg.is_none()
            && let Some(upvalue_arg) = self.resolve_upvalue(name, error_location)
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
        (get_op, set_op, arg.unwrap())
    }

    /// Emit a load of the local at `slot`. Used by decorated declarations,
    /// whose decorator temporaries are anonymous locals that cannot be
    /// resolved by name.
    ///
    /// Stack: `[..] -> [.., slot value]`
    pub(super) fn emit_local_get(&mut self, slot: usize, variable_location: Location) {
        let location = OpcodeLocation::new(variable_location);
        let (op, encoding) = if slot > usize::from(u8::MAX) {
            (OpCode::GetLocalLong, NumberEncoding::Long)
        } else {
            (OpCode::GetLocal, NumberEncoding::Short)
        };
        self.emit_byte(op, location);
        if !self.emit_number(slot, encoding, location) {
            self.error(&format!("Too many locals in {op:?}"));
        }
    }

    /// Emit a plain store of the stack top into `name` (local, upvalue, or
    /// global), then pop the stored value. Used by decorated declarations,
    /// which rebind an already-declared name outside any assignment
    /// expression.
    ///
    /// Stack: `[.., value] -> [..]`
    pub(super) fn emit_variable_set_and_pop(&mut self, name: &str, variable_location: Location) {
        let location = OpcodeLocation::new(variable_location);
        let (_, mut set_op, arg) = self.resolve_variable_ops(name, variable_location);

        let encoding = if arg > usize::from(u8::MAX) {
            set_op = set_op.to_long();
            NumberEncoding::Long
        } else {
            NumberEncoding::Short
        };

        self.emit_byte(set_op, location);
        if !self.emit_number(arg, encoding, location) {
            self.error(&format!("Too many globals in {set_op:?}"));
        }
        self.emit_byte(OpCode::Pop, location);
    }

    /// Emit a plain read of `name`.
    pub(super) fn emit_named_read(&mut self, name: &str, variable_location: Location) {
        let location = OpcodeLocation::new(variable_location);
        let (mut get_op, _, arg) = self.resolve_variable_ops(name, variable_location);
        let encoding = if arg > usize::from(u8::MAX) {
            get_op = get_op.to_long();
            NumberEncoding::Long
        } else {
            NumberEncoding::Short
        };
        self.emit_byte(get_op, location);
        if !self.emit_number(arg, encoding, location) {
            self.error(&format!("Too many globals in {get_op:?}"));
        }
    }

    // Because the closure would need unique access to self while it
    // is mutably borrowed.
    /// Global values are constant names bound to values.
    /// If already a global with that name exists then its index is used.
    /// Otherwise a new global is created and the string constant corresponding
    /// to its name is added to the constant table.
    pub(super) fn identifier_constant(&mut self, name: &str) -> ConstantLongIndex {
        let string_id = self.heap.string_id(&name);
        if let Some(index) = self.globals_by_name().get(&string_id) {
            *index
        } else {
            let index = self.current_chunk_mut().make_constant(string_id.into());
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
    /// (Current frame; reports the own-initializer error.)
    fn resolve_local(&mut self, name: &str, error_location: Location) -> Option<usize> {
        let retval = self
            .locals()
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| local.name == name)
            .map(|(index, local)| {
                if *local.depth == -1 {
                    self.locals().len()
                } else {
                    index
                }
            });
        if retval == Some(self.locals().len()) {
            self.error_at(
                error_location,
                name,
                "Can't read local variable in its own initializer.",
            );
        }
        retval
    }

    pub(super) fn add_local(&mut self, name: String, mutability: Mutability) {
        if self.locals().len() > usize::pow(2, 24) - 1 {
            self.error("Too many local variables in function.");
            return;
        }
        self.locals_mut().push(Local {
            name,
            depth: ScopeDepth(-1),
            mutability,
            is_captured: false,
        });
    }

    /// Try to find the upvalue with the given name.
    ///
    /// Iterates up the scopes checking if a local or upvalue with that name
    /// exists. If nothing is found or the current scope is the outermost,
    /// then `None` is returned.
    fn resolve_upvalue(&mut self, name: &str, error_location: Location) -> Option<u8> {
        if !self.has_enclosing() {
            return None;
        }

        if let Some(local) =
            self.in_enclosing(|codegen| codegen.resolve_local(name, error_location))
        {
            // `resolve_local` reports the own-initializer error and returns
            // `locals.len()` as a sentinel index; mark the capture only when
            // the index names a real slot and fail the resolution otherwise.
            return self
                .in_enclosing(|codegen| {
                    codegen.locals_mut().get_mut(local)?.is_captured = true;
                    Some(local)
                })
                .map(|local| self.add_upvalue(local, true, error_location, name));
        }

        if let Some(upvalue) =
            self.in_enclosing(|codegen| codegen.resolve_upvalue(name, error_location))
        {
            return Some(self.add_upvalue(usize::from(upvalue), false, error_location, name));
        }

        None
    }

    /// Resolve an upvalue or add a new one if it does not yet exist.
    fn add_upvalue(
        &mut self,
        local_index: usize,
        is_local: bool,
        error_location: Location,
        error_name: &str,
    ) -> u8 {
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
                self.error_at(
                    error_location,
                    error_name,
                    "Too many closure variables in function.",
                );
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
            self.error_at(
                error_location,
                error_name,
                "Too many variables in function surrounding closure.",
            );
            0
        }
    }

    /// Declare a variable in a local scope.
    ///
    /// If called from the top level, no action is taken.
    /// Otherwise it is checked if there already is an exiting local
    /// with that name. If so, an error is reported.
    /// Otherwise, the variable is added to the list of locals.
    pub(super) fn declare_variable(
        &mut self,
        name: &str,
        mutability: Mutability,
        location: Location,
    ) {
        if *self.scope_depth() == 0 {
            return;
        }
        let scope_depth = self.scope_depth();
        if self.locals().iter().rev().any(|local| {
            if *local.depth != -1 && local.depth < scope_depth {
                false
            } else {
                local.name == name
            }
        }) {
            self.error_at(
                location,
                name,
                "Already a variable with this name in this scope.",
            );
        }
        self.add_local(name.to_string(), mutability);
    }
}

impl Parser<'_> {
    /// Consume an identifier.
    pub(super) fn parse_variable(&mut self, msg: &str) -> (String, Location) {
        self.consume(TK::Identifier, msg);
        (
            self.previous.as_ref().unwrap().as_str().to_string(),
            self.location(),
        )
    }
}

impl Codegen<'_, '_> {
    /// Parse a variable. If it is local, it gets declared normally.
    /// Otherwise, it is added as a global.
    pub(super) fn declare_or_intern(
        &mut self,
        name: &str,
        mutability: Mutability,
        location: Location,
    ) -> Option<ConstantLongIndex> {
        self.declare_variable(name, mutability, location);
        if *self.scope_depth() > 0 {
            None
        } else {
            Some(self.identifier_constant(name))
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
        location: OpcodeLocation,
    ) {
        if *self.scope_depth() > 0 {
            self.mark_initialized();
            return;
        }

        let global = global.unwrap();

        if let Ok(short) = u8::try_from(*global) {
            match mutability {
                Mutability::Mutable => self.emit_byte(OpCode::DefineGlobal, location),
                Mutability::Immutable => self.emit_byte(OpCode::DefineGlobalConst, location),
            }
            self.emit_byte(short, location);
        } else {
            match mutability {
                Mutability::Mutable => self.emit_byte(OpCode::DefineGlobalLong, location),
                Mutability::Immutable => self.emit_byte(OpCode::DefineGlobalConstLong, location),
            }
            if !self.emit_24bit_number(*global, location) {
                self.error("Too many globals in define_global!");
            }
        }
    }
}

impl Parser<'_> {
    /// Parse a list of arguments to a call.
    ///
    /// This means all comma separated expressions until a closing `)` is found.
    /// An optional trailing comma is permitted.
    pub(super) fn argument_list(&mut self) -> Vec<Arg> {
        let mut args: Vec<Arg> = Vec::new();
        while !self.check(TK::RightParen) {
            let is_spread = self.match_(TK::Star);
            let value = self.parse_precedence_ignoring(Precedence::Assignment, &[TK::Comma]);
            // The check fires on the 256th parsed argument, which is
            // then discarded.
            if args.len() == 255 {
                self.error("Can't have more than 255 arguments.");
                break;
            }
            args.push(Arg { value, is_spread });
            if !self.match_(TK::Comma) {
                // The next token starts a new argument, so the comma
                // between them was probably forgotten.
                if !self.check(TK::RightParen) && self.current_starts_expression() {
                    self.error_at_current(
                        "Expect ',' or ')' after argument. Perhaps you forgot a comma?",
                    );
                }
                break;
            }
        }
        self.consume(TK::RightParen, "Expect ')' after arguments.");
        args
    }
}

impl Codegen<'_, '_> {
    /// Emit the `ceil(arg_count / 8)` used spread-bitmap bytes after an
    /// unpacking-call opcode and its segment count.
    pub(super) fn emit_spread_bitmap(
        &mut self,
        bitmap: &[u8; SPREAD_BITMAP_BYTES],
        arg_count: u8,
        location: OpcodeLocation,
    ) {
        for &byte in &bitmap[..usize::from(arg_count).div_ceil(8)] {
            self.emit_byte(byte, location);
        }
    }

    /// Reject a write to a `const` local, reported at the variable's
    /// own name (the token the user declared it with).
    pub(super) fn check_local_const(
        &mut self,
        local_index: usize,
        error_location: Location,
        error_token: &str,
    ) {
        let local = &self.locals()[local_index];
        if *local.depth != -1 && local.mutability == Mutability::Immutable {
            self.error_at(
                error_location,
                error_token,
                "Reassignment to local 'const'.",
            );
        }
    }
}
