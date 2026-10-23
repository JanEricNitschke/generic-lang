//! AST optimization passes: constant folding, dead-code elimination, and loop
//! unrolling.

mod constant_fold;
mod dead_code_elimination;
mod loop_unrolling;
mod walk;

#[cfg(test)]
mod tests;

use super::ast::{InjectedBody, InjectedProgram, Program, Stmt};
use constant_fold::{fold_expr, fold_stmts};
use dead_code_elimination::eliminate_dead_code;
use loop_unrolling::unroll_stmts;

/// Unrolling creates wrapper blocks and copies statements, so fold and
/// eliminate dead code again to clean up the expanded body.
fn optimize_stmts(stmts: &mut Vec<Stmt>) {
    fold_stmts(stmts);
    eliminate_dead_code(stmts);
    unroll_stmts(stmts);
    fold_stmts(stmts);
    eliminate_dead_code(stmts);
}

impl Program {
    /// Run all AST optimizations over the parsed declarations.
    pub(crate) fn optimize(&mut self) {
        optimize_stmts(&mut self.declarations);
    }
}

impl InjectedProgram {
    /// Run all AST optimizations over injected `eval`/`exec` source.
    pub(crate) fn optimize(&mut self) {
        match &mut self.body {
            InjectedBody::Eval(expr) => fold_expr(expr),
            InjectedBody::Exec(stmts) => optimize_stmts(stmts),
        }
    }
}
