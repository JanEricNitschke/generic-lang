use super::super::ast::{Expr, ExprKind, Stmt, StmtKind};
use super::constant_fold::{literal, literal_truthy};
use super::walk::Visitor;
use crate::types::{ConditionType, Location, LoopType};

struct DeadCodeEliminator;

impl Visitor for DeadCodeEliminator {
    fn visit_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        for stmt in stmts.iter_mut() {
            self.visit_stmt_kind(&mut stmt.kind);
        }
        *stmts = eliminate_list(std::mem::take(stmts));
    }
}

/// Eliminate unreachable and statically dead statements in every nested list.
pub(super) fn eliminate_dead_code(stmts: &mut Vec<Stmt>) {
    DeadCodeEliminator.visit_stmts(stmts);
}

fn eliminate_list(old: Vec<Stmt>) -> Vec<Stmt> {
    let mut optimized = Vec::with_capacity(old.len());
    'outer: for stmt in old {
        let location = stmt.location;
        for replacement in simplify_stmt(stmt.kind) {
            if is_empty_block(&replacement) || is_pure_expression_stmt(&replacement) {
                continue;
            }
            let terminates = is_terminator(&replacement);
            optimized.push(Stmt {
                kind: replacement,
                location,
            });
            if terminates {
                break 'outer;
            }
        }
    }
    optimized
}

#[allow(clippy::too_many_lines)]
pub(super) fn simplify_stmt(stmt: StmtKind) -> Vec<StmtKind> {
    match stmt {
        StmtKind::Expression { expr } if is_pure_expr(&expr) => Vec::new(),
        StmtKind::Block { stmts, .. } if stmts.is_empty() => Vec::new(),
        StmtKind::Conditional {
            condition_type,
            condition,
            then_block,
            else_block,
            then_close_location,
            else_close_location,
            keyword_location,
        } => {
            let Some(literal) = literal(&condition) else {
                return vec![StmtKind::Conditional {
                    condition_type,
                    condition,
                    then_block,
                    else_block,
                    then_close_location,
                    else_close_location,
                    keyword_location,
                }];
            };
            let condition_truthy = literal_truthy(literal);
            let take_then = match condition_type {
                ConditionType::If => condition_truthy,
                ConditionType::Unless => !condition_truthy,
            };
            let taken = if take_then {
                then_block
            } else {
                else_block.unwrap_or_default()
            };
            if taken.is_empty() {
                Vec::new()
            } else {
                vec![StmtKind::Block { stmts: taken }]
            }
        }
        StmtKind::Loop {
            loop_type,
            condition,
            body,
            label,
            body_close_location,
            keyword_location,
        } => {
            if let Some(literal) = literal(&condition) {
                let condition_truthy = literal_truthy(literal);
                let executes = match loop_type {
                    LoopType::While => condition_truthy,
                    LoopType::Until => !condition_truthy,
                };
                if !executes {
                    return Vec::new();
                }
            }
            vec![StmtKind::Loop {
                loop_type,
                condition,
                body,
                label,
                body_close_location,
                keyword_location,
            }]
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
        } => {
            let condition_is_false = condition
                .as_deref()
                .and_then(literal)
                .is_some_and(|literal| !literal_truthy(literal));
            if condition_is_false {
                return match init {
                    Some(init) if !is_effect_free_stmt(&init) => {
                        vec![StmtKind::Block {
                            stmts: vec![Stmt {
                                kind: *init,
                                location: Location::default(),
                            }],
                        }]
                    }
                    _ => Vec::new(),
                };
            }
            vec![StmtKind::For {
                label,
                init,
                condition,
                increment,
                body,
                right_paren_location,
                body_close_location,
                keyword_location,
            }]
        }
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
        } => vec![StmtKind::Foreach {
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
        }],
        other => vec![other],
    }
}

pub(super) fn is_empty_block(stmt: &StmtKind) -> bool {
    matches!(stmt, StmtKind::Block { stmts, .. } if stmts.is_empty())
}

pub(super) fn is_terminator(stmt: &StmtKind) -> bool {
    matches!(
        stmt,
        StmtKind::Return { .. }
            | StmtKind::Throw { .. }
            | StmtKind::Break { .. }
            | StmtKind::Continue { .. }
    )
}

pub(super) fn is_effect_free_stmt(stmt: &StmtKind) -> bool {
    match stmt {
        StmtKind::Expression { expr } => is_pure_expr(expr),
        StmtKind::VarDecl { initializer, .. } => initializer.as_deref().is_none_or(is_pure_expr),
        StmtKind::Block { stmts, .. } => stmts.iter().all(|stmt| is_effect_free_stmt(&stmt.kind)),
        _ => false,
    }
}

pub(super) fn is_pure_expression_stmt(stmt: &StmtKind) -> bool {
    matches!(stmt, StmtKind::Expression { expr } if is_pure_expr(expr))
}

pub(super) fn is_pure_expr(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Literal(_) => true,
        ExprKind::Grouping { inner } => is_pure_expr(inner),
        _ => false,
    }
}
