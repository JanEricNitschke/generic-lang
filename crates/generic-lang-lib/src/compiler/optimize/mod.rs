//! AST optimization passes.

mod constant_fold;
mod walk;

#[cfg(test)]
mod tests;

use super::ast::{InjectedBody, InjectedProgram, Program};
use constant_fold::{fold_expr, fold_stmts};

impl Program {
    /// Run all AST optimizations over the parsed declarations.
    pub(crate) fn optimize(&mut self) {
        fold_stmts(&mut self.declarations);
    }
}

impl InjectedProgram {
    /// Run all AST optimizations over injected `eval`/`exec` source.
    pub(crate) fn optimize(&mut self) {
        match &mut self.body {
            InjectedBody::Eval(expr) => fold_expr(expr),
            InjectedBody::Exec(stmts) => fold_stmts(stmts),
        }
    }
}
