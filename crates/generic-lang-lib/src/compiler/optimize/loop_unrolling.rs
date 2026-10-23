use super::super::ast::{BinaryOp, Expr, ExprKind, FstringPart, Literal, Stmt, StmtKind};
use super::constant_fold::literal;
use super::walk::Visitor;
use crate::types::{Location, Mutability};

pub(super) const MAX_UNROLL_TRIPS: usize = 32;
/// Maximum number of inline statements unrolling may add to one chunk.
/// Function and method bodies compile into separate chunks and are not counted.
pub(super) const MAX_UNROLLED_STATEMENTS: usize = 4096;

struct LoopUnroller;

impl Visitor for LoopUnroller {
    fn visit_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        for stmt in stmts.iter_mut() {
            self.visit_stmt_kind(&mut stmt.kind);
        }
        *stmts = unroll_list(std::mem::take(stmts));
    }
}

/// Unroll statically-known loops in every nested statement list.
pub(super) fn unroll_stmts(stmts: &mut Vec<Stmt>) {
    LoopUnroller.visit_stmts(stmts);
}

fn unroll_list(old: Vec<Stmt>) -> Vec<Stmt> {
    let mut optimized = Vec::with_capacity(old.len());
    for stmt in old {
        let location = stmt.location;
        match unroll_stmt(&stmt.kind) {
            Some(replacements) => {
                optimized.extend(replacements.into_iter().map(|kind| Stmt { kind, location }));
            }
            None => optimized.push(stmt),
        }
    }
    optimized
}

fn unroll_stmt(stmt: &StmtKind) -> Option<Vec<StmtKind>> {
    match stmt {
        StmtKind::For {
            init,
            condition,
            increment,
            body,
            ..
        } => unroll_for(
            init.as_deref(),
            condition.as_deref(),
            increment.as_deref(),
            body,
        ),
        StmtKind::Foreach {
            mutability,
            var_name,
            iterable,
            body,
            var_name_location,
            close_location,
            ..
        } => unroll_foreach(
            *mutability,
            var_name,
            iterable,
            *var_name_location,
            *close_location,
            body,
        ),
        _ => None,
    }
}

pub(super) fn statement_count(stmts: &[Stmt]) -> usize {
    stmts
        .iter()
        .fold(0, |count, stmt| count.saturating_add(statement_size(stmt)))
}

fn statement_size(stmt: &Stmt) -> usize {
    1usize.saturating_add(statement_children(&stmt.kind))
}

fn statement_children(stmt: &StmtKind) -> usize {
    match stmt {
        StmtKind::Block { stmts, .. } => statement_count(stmts),
        StmtKind::Class(class) => class.members.len(),
        StmtKind::Conditional {
            then_block,
            else_block,
            ..
        } => statement_count(then_block) + else_block.as_deref().map_or(0, statement_count),
        StmtKind::Loop { body, .. } => statement_count(body),
        StmtKind::For { init, body, .. } => {
            init.as_deref()
                .map_or(0, |init| 1usize.saturating_add(statement_children(init)))
                + statement_count(body)
        }
        StmtKind::Foreach { body, .. } => statement_count(body),
        StmtKind::Try(try_stmt) => {
            statement_count(&try_stmt.body)
                + try_stmt
                    .catches
                    .iter()
                    .map(|catch| statement_count(&catch.body))
                    .sum::<usize>()
                + try_stmt.else_block.as_deref().map_or(0, statement_count)
        }
        StmtKind::Switch { cases, .. } => {
            cases.iter().map(|case| statement_count(&case.body)).sum()
        }
        StmtKind::Expression { .. }
        | StmtKind::VarDecl { .. }
        | StmtKind::FunctionDecl { .. }
        | StmtKind::Break { .. }
        | StmtKind::Continue { .. }
        | StmtKind::Return { .. }
        | StmtKind::Throw { .. }
        | StmtKind::Import { .. }
        | StmtKind::ImportFrom { .. } => 0,
    }
}

fn unroll_foreach(
    mutability: Mutability,
    var_name: &str,
    iterable: &Expr,
    var_name_location: Location,
    close_location: Location,
    body: &[Stmt],
) -> Option<Vec<StmtKind>> {
    let items = tuple_elements(iterable)?;
    if items.len() > MAX_UNROLL_TRIPS
        || items
            .iter()
            .any(|item| matches!(item, Literal::StopIteration))
        // Assigning the per-iteration variable cannot affect iterator progress,
        // so only loop control flow matters here.
        || stmts_are_unsafe_to_unroll(body, None)
        || statement_count(body)
            .saturating_add(2)
            .checked_mul(items.len())
            .is_none_or(|size| size > MAX_UNROLLED_STATEMENTS)
    {
        return None;
    }
    Some(
        items
            .iter()
            .map(|item| {
                foreach_iteration(
                    mutability,
                    var_name,
                    item.clone(),
                    iterable.location,
                    var_name_location,
                    close_location,
                    body,
                )
            })
            .collect(),
    )
}

pub(super) fn unroll_for(
    init: Option<&StmtKind>,
    condition: Option<&Expr>,
    increment: Option<&Expr>,
    body: &[Stmt],
) -> Option<Vec<StmtKind>> {
    // Only canonical three-clause loops are unrolled. Without an initializer we
    // could mutate an outer variable; without a condition the loop is infinite;
    // without an increment termination depends on body mutations we do not analyze.
    let init = init?;
    let condition = condition?;
    let increment = increment?;
    // A fresh mutable declaration with a literal integer start is required so
    // each unrolled iteration has a known, independent loop-variable value.
    let StmtKind::VarDecl {
        name,
        mutability,
        initializer: Some(initializer),
        name_location,
        semicolon_location,
    } = init
    else {
        return None;
    };
    if *mutability == Mutability::Immutable {
        return None;
    }
    let Literal::Int(start) = literal(initializer)? else {
        return None;
    };
    if stmts_are_unsafe_to_unroll(body, Some(name)) {
        return None;
    }
    let (comparison, bound) = for_condition(condition, name)?;
    let step = for_step(increment, name)?;
    let values = for_trip_values(*start, bound, step, comparison)?;
    // An empty body is currently still unrolled into empty iteration blocks;
    // removing it entirely is a separate optimization.
    if statement_count(body)
        .saturating_add(2)
        .checked_mul(values.len())?
        > MAX_UNROLLED_STATEMENTS
    {
        return None;
    }
    Some(
        values
            .into_iter()
            .map(|value| {
                let loop_var = Stmt {
                    kind: StmtKind::VarDecl {
                        name: name.clone(),
                        mutability: *mutability,
                        initializer: Some(Box::new(Expr {
                            kind: ExprKind::Literal(Literal::Int(value)),
                            location: initializer.location,
                        })),
                        name_location: *name_location,
                        semicolon_location: *semicolon_location,
                    },
                    location: initializer.location,
                };
                StmtKind::Block {
                    stmts: vec![
                        loop_var,
                        Stmt {
                            kind: StmtKind::Block {
                                stmts: body.to_vec(),
                            },
                            location: Location::default(),
                        },
                    ],
                }
            })
            .collect(),
    )
}

pub(super) fn for_condition(condition: &Expr, name: &str) -> Option<(BinaryOp, i64)> {
    let ExprKind::Binary { op, lhs, rhs, .. } = &condition.kind else {
        return None;
    };
    if !matches!(
        op,
        BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual
    ) {
        return None;
    }
    if !is_variable(lhs, name) {
        return None;
    }
    let Literal::Int(bound) = literal(rhs)? else {
        return None;
    };
    Some((*op, *bound))
}

pub(super) fn for_step(increment: &Expr, name: &str) -> Option<i64> {
    match &increment.kind {
        ExprKind::AugAssign {
            target, op, value, ..
        } if is_variable(target, name) => {
            let Literal::Int(step) = literal(value)? else {
                return None;
            };
            match op {
                BinaryOp::Add => Some(*step),
                BinaryOp::Sub => step.checked_neg(),
                _ => None,
            }
        }
        ExprKind::Assign { target, value, .. } if is_variable(target, name) => {
            let ExprKind::Binary { op, lhs, rhs, .. } = &value.kind else {
                return None;
            };
            if !is_variable(lhs, name) {
                return None;
            }
            let Literal::Int(step) = literal(rhs)? else {
                return None;
            };
            match op {
                BinaryOp::Add => Some(*step),
                BinaryOp::Sub => step.checked_neg(),
                _ => None,
            }
        }
        _ => None,
    }
}

pub(super) fn is_variable(expr: &Expr, name: &str) -> bool {
    matches!(
        &expr.kind,
        ExprKind::Variable {
            name: variable_name,
            ..
        } if variable_name == name
    )
}

pub(super) fn for_trip_values(
    start: i64,
    bound: i64,
    step: i64,
    comparison: BinaryOp,
) -> Option<Vec<i64>> {
    // Widen so stepping past i64::MAX/MIN after the final iteration does not
    // look like overflow; the condition can then terminate normally.
    let bound = i128::from(bound);
    let step = i128::from(step);
    let mut value = i128::from(start);
    let mut values = Vec::new();
    loop {
        let continues = match comparison {
            BinaryOp::Less => value < bound,
            BinaryOp::LessEqual => value <= bound,
            BinaryOp::Greater => value > bound,
            BinaryOp::GreaterEqual => value >= bound,
            _ => return None,
        };
        if !continues {
            return Some(values);
        }
        if values.len() == MAX_UNROLL_TRIPS {
            return None;
        }
        values.push(i64::try_from(value).ok()?);
        value = value.checked_add(step)?;
    }
}

pub(super) fn foreach_iteration(
    mutability: Mutability,
    var_name: &str,
    item: Literal,
    iterable_location: Location,
    var_name_location: Location,
    close_location: Location,
    body: &[Stmt],
) -> StmtKind {
    let loop_var = Stmt {
        kind: StmtKind::VarDecl {
            name: var_name.to_string(),
            mutability,
            initializer: Some(Box::new(Expr {
                kind: ExprKind::Literal(item),
                location: iterable_location,
            })),
            name_location: var_name_location,
            semicolon_location: close_location,
        },
        location: iterable_location,
    };
    StmtKind::Block {
        stmts: vec![
            loop_var,
            Stmt {
                kind: StmtKind::Block {
                    stmts: body.to_vec(),
                },
                location: Location::default(),
            },
        ],
    }
}

pub(super) fn stmts_are_unsafe_to_unroll(stmts: &[Stmt], loop_var: Option<&str>) -> bool {
    stmts
        .iter()
        .any(|stmt| stmt_is_unsafe_to_unroll(&stmt.kind, loop_var))
}

#[allow(clippy::too_many_lines)]
pub(super) fn stmt_is_unsafe_to_unroll(stmt: &StmtKind, loop_var: Option<&str>) -> bool {
    match stmt {
        StmtKind::Break { .. } | StmtKind::Continue { .. } => true,
        StmtKind::Import { .. } | StmtKind::ImportFrom { .. } => false,
        // Unrolling duplicates these declarations, and every copy compiles
        // another function/class constant into the same chunk.
        StmtKind::FunctionDecl { .. } | StmtKind::Class(_) => true,
        // Control-flow statements need their nested bodies and conditions checked.
        StmtKind::Conditional {
            condition,
            then_block,
            else_block,
            ..
        } => {
            expr_is_unsafe_to_unroll(condition, loop_var)
                || stmts_are_unsafe_to_unroll(then_block, loop_var)
                || else_block
                    .as_ref()
                    .is_some_and(|stmts| stmts_are_unsafe_to_unroll(stmts, loop_var))
        }
        StmtKind::Loop {
            condition, body, ..
        } => {
            expr_is_unsafe_to_unroll(condition, loop_var)
                || stmts_are_unsafe_to_unroll(body, loop_var)
        }
        StmtKind::For {
            init,
            condition,
            increment,
            body,
            ..
        } => {
            init.as_deref()
                .is_some_and(|stmt| stmt_is_unsafe_to_unroll(stmt, loop_var))
                || condition
                    .as_deref()
                    .is_some_and(|expr| expr_is_unsafe_to_unroll(expr, loop_var))
                || increment
                    .as_deref()
                    .is_some_and(|expr| expr_is_unsafe_to_unroll(expr, loop_var))
                || stmts_are_unsafe_to_unroll(body, loop_var)
        }
        StmtKind::Foreach { iterable, body, .. } => {
            expr_is_unsafe_to_unroll(iterable, loop_var)
                || stmts_are_unsafe_to_unroll(body, loop_var)
        }
        // The remaining statements only delegate to their expressions or bodies.
        StmtKind::Expression { expr } | StmtKind::Throw { expr, .. } => {
            expr_is_unsafe_to_unroll(expr, loop_var)
        }
        StmtKind::VarDecl { initializer, .. } => initializer
            .as_deref()
            .is_some_and(|expr| expr_is_unsafe_to_unroll(expr, loop_var)),
        StmtKind::Block { stmts, .. } => stmts_are_unsafe_to_unroll(stmts, loop_var),
        StmtKind::Return { value } => value
            .as_deref()
            .is_some_and(|expr| expr_is_unsafe_to_unroll(expr, loop_var)),
        StmtKind::Try(try_stmt) => {
            stmts_are_unsafe_to_unroll(&try_stmt.body, loop_var)
                || try_stmt.catches.iter().any(|catch| {
                    catch
                        .matchers
                        .iter()
                        .any(|expr| expr_is_unsafe_to_unroll(expr, loop_var))
                        || stmts_are_unsafe_to_unroll(&catch.body, loop_var)
                })
                || try_stmt
                    .else_block
                    .as_ref()
                    .is_some_and(|stmts| stmts_are_unsafe_to_unroll(stmts, loop_var))
        }
        StmtKind::Switch { subject, cases, .. } => {
            expr_is_unsafe_to_unroll(subject, loop_var)
                || cases.iter().any(|case| {
                    case.value
                        .as_deref()
                        .is_some_and(|expr| expr_is_unsafe_to_unroll(expr, loop_var))
                        || stmts_are_unsafe_to_unroll(&case.body, loop_var)
                })
        }
    }
}

pub(super) fn expr_is_unsafe_to_unroll(expr: &Expr, loop_var: Option<&str>) -> bool {
    match &expr.kind {
        ExprKind::Literal(_) | ExprKind::Variable { .. } | ExprKind::Super { .. } => false,
        ExprKind::Assign { target, value, .. } | ExprKind::AugAssign { target, value, .. } => {
            loop_var.is_some_and(|name| is_variable(target, name))
                || expr_is_unsafe_to_unroll(target, loop_var)
                || expr_is_unsafe_to_unroll(value, loop_var)
        }
        // Unrolling duplicates the lambda, and every copy compiles another
        // function constant into the same chunk.
        ExprKind::Lambda(_) => true,
        ExprKind::Property { object, .. } => expr_is_unsafe_to_unroll(object, loop_var),
        ExprKind::Subscript { object, index, .. } => {
            expr_is_unsafe_to_unroll(object, loop_var) || expr_is_unsafe_to_unroll(index, loop_var)
        }
        ExprKind::Call { callee, args, .. } => {
            expr_is_unsafe_to_unroll(callee, loop_var)
                || args
                    .iter()
                    .any(|arg| expr_is_unsafe_to_unroll(&arg.value, loop_var))
        }
        ExprKind::Unary { operand, .. } => expr_is_unsafe_to_unroll(operand, loop_var),
        ExprKind::Binary { lhs, rhs, .. } | ExprKind::Logical { lhs, rhs, .. } => {
            expr_is_unsafe_to_unroll(lhs, loop_var) || expr_is_unsafe_to_unroll(rhs, loop_var)
        }
        ExprKind::Ternary {
            condition,
            then,
            else_,
            ..
        } => {
            expr_is_unsafe_to_unroll(condition, loop_var)
                || expr_is_unsafe_to_unroll(then, loop_var)
                || expr_is_unsafe_to_unroll(else_, loop_var)
        }
        ExprKind::Grouping { inner } => expr_is_unsafe_to_unroll(inner, loop_var),
        ExprKind::Tuple { elements }
        | ExprKind::ListLit { items: elements }
        | ExprKind::SetLit { items: elements } => elements
            .iter()
            .any(|expr| expr_is_unsafe_to_unroll(expr, loop_var)),
        ExprKind::DictLit { entries } => entries.iter().any(|(key, value)| {
            expr_is_unsafe_to_unroll(key, loop_var) || expr_is_unsafe_to_unroll(value, loop_var)
        }),
        ExprKind::Fstring { parts } => parts.iter().any(|part| match part {
            FstringPart::Str(_) => false,
            FstringPart::Interp(expr) => expr_is_unsafe_to_unroll(expr, loop_var),
        }),
        ExprKind::Tstring(tstring) => tstring
            .interpolations
            .iter()
            .any(|interpolation| expr_is_unsafe_to_unroll(&interpolation.value, loop_var)),
        ExprKind::Yield { operand } => operand
            .as_deref()
            .is_some_and(|expr| expr_is_unsafe_to_unroll(expr, loop_var)),
    }
}

pub(super) fn tuple_elements(expr: &Expr) -> Option<Vec<Literal>> {
    match &expr.kind {
        ExprKind::Tuple { elements } => elements
            .iter()
            .map(|element| literal(element).cloned())
            .collect(),
        ExprKind::Grouping { inner } => tuple_elements(inner),
        ExprKind::Literal(Literal::Tuple(items)) => Some(items.clone()),
        _ => None,
    }
}
