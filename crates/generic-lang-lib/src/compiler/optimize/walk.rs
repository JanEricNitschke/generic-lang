use super::super::ast::{
    ClassMemberKind, Expr, ExprKind, FstringPart, FunctionDef, LambdaBody, LambdaLit, ParamList,
    Stmt, StmtKind,
};

/// Overridable hooks for optimizer passes.
///
/// The standalone `walk_*` functions below perform the actual recursion. A pass
/// overrides a hook when it needs to act before/after visiting that node's
/// children.
pub(super) trait Visitor {
    fn visit_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        walk_stmts(self, stmts);
    }

    fn visit_stmt_kind(&mut self, stmt: &mut StmtKind) {
        walk_stmt_kind(self, stmt);
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr(self, expr);
    }
}

pub(super) fn walk_stmts<V: Visitor + ?Sized>(visitor: &mut V, stmts: &mut [Stmt]) {
    for stmt in stmts {
        visitor.visit_stmt_kind(&mut stmt.kind);
    }
}

fn walk_exprs<V: Visitor + ?Sized>(visitor: &mut V, exprs: &mut [Expr]) {
    for expr in exprs {
        visitor.visit_expr(expr);
    }
}

pub(super) fn walk_stmt_kind<V: Visitor + ?Sized>(visitor: &mut V, stmt: &mut StmtKind) {
    match stmt {
        StmtKind::Expression { expr } | StmtKind::Throw { expr, .. } => visitor.visit_expr(expr),
        StmtKind::VarDecl { initializer, .. } => {
            if let Some(initializer) = initializer {
                visitor.visit_expr(initializer);
            }
        }
        StmtKind::Block { stmts, .. } => visitor.visit_stmts(stmts),
        StmtKind::FunctionDecl {
            fn_lit, decorators, ..
        } => {
            walk_function(visitor, fn_lit);
            for decorator in decorators {
                visitor.visit_expr(decorator);
            }
        }
        StmtKind::Class(class) => {
            if let Some((superclass, _)) = &mut class.superclass {
                visitor.visit_expr(superclass);
            }
            for member in &mut class.members {
                match &mut member.kind {
                    ClassMemberKind::Variable(variable) => {
                        if let Some(annotation) = &mut variable.annotation {
                            visitor.visit_expr(annotation);
                        }
                        if let Some(default) = &mut variable.default {
                            visitor.visit_expr(default);
                        }
                    }
                    ClassMemberKind::Method(method) => {
                        walk_function(visitor, &mut method.fn_lit);
                    }
                }
            }
            for decorator in &mut class.decorators {
                visitor.visit_expr(decorator);
            }
        }
        StmtKind::Conditional {
            condition,
            then_block,
            else_block,
            ..
        } => {
            visitor.visit_expr(condition);
            visitor.visit_stmts(then_block);
            if let Some(else_block) = else_block {
                visitor.visit_stmts(else_block);
            }
        }
        StmtKind::Loop {
            condition, body, ..
        } => {
            visitor.visit_expr(condition);
            visitor.visit_stmts(body);
        }
        StmtKind::Break { .. }
        | StmtKind::Continue { .. }
        | StmtKind::Import { .. }
        | StmtKind::ImportFrom { .. } => {}
        StmtKind::For {
            init,
            condition,
            increment,
            body,
            ..
        } => {
            if let Some(init) = init {
                visitor.visit_stmt_kind(init);
            }
            if let Some(condition) = condition {
                visitor.visit_expr(condition);
            }
            if let Some(increment) = increment {
                visitor.visit_expr(increment);
            }
            visitor.visit_stmts(body);
        }
        StmtKind::Foreach { iterable, body, .. } => {
            visitor.visit_expr(iterable);
            visitor.visit_stmts(body);
        }
        StmtKind::Return { value } => {
            if let Some(value) = value {
                visitor.visit_expr(value);
            }
        }
        StmtKind::Try(try_stmt) => {
            visitor.visit_stmts(&mut try_stmt.body);
            for catch in &mut try_stmt.catches {
                for matcher in &mut catch.matchers {
                    visitor.visit_expr(matcher);
                }
                visitor.visit_stmts(&mut catch.body);
            }
            if let Some(else_block) = &mut try_stmt.else_block {
                visitor.visit_stmts(else_block);
            }
        }
        StmtKind::Switch { subject, cases, .. } => {
            visitor.visit_expr(subject);
            for case in cases {
                if let Some(value) = &mut case.value {
                    visitor.visit_expr(value);
                }
                visitor.visit_stmts(&mut case.body);
            }
        }
    }
}

pub(super) fn walk_expr<V: Visitor + ?Sized>(visitor: &mut V, expr: &mut Expr) {
    match &mut expr.kind {
        ExprKind::Literal(_) | ExprKind::Variable { .. } | ExprKind::Super { .. } => {}
        ExprKind::Property { object, .. } => visitor.visit_expr(object),
        ExprKind::Subscript { object, index, .. } => {
            visitor.visit_expr(object);
            visitor.visit_expr(index);
        }
        ExprKind::Call { callee, args, .. } => {
            visitor.visit_expr(callee);
            for arg in args {
                visitor.visit_expr(&mut arg.value);
            }
        }
        ExprKind::Unary { operand, .. } => visitor.visit_expr(operand),
        ExprKind::Binary { lhs, rhs, .. } | ExprKind::Logical { lhs, rhs, .. } => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        ExprKind::Ternary {
            condition,
            then,
            else_,
            ..
        } => {
            visitor.visit_expr(condition);
            visitor.visit_expr(then);
            visitor.visit_expr(else_);
        }
        ExprKind::Grouping { inner } => visitor.visit_expr(inner),
        ExprKind::Tuple { elements } => walk_exprs(visitor, elements),
        ExprKind::ListLit { items } | ExprKind::SetLit { items } => walk_exprs(visitor, items),
        ExprKind::DictLit { entries } => {
            for (key, value) in entries {
                visitor.visit_expr(key);
                visitor.visit_expr(value);
            }
        }
        ExprKind::Fstring { parts } => {
            for part in parts {
                if let FstringPart::Interp(expr) = part {
                    visitor.visit_expr(expr);
                }
            }
        }
        ExprKind::Tstring(tstring) => {
            for interpolation in &mut tstring.interpolations {
                visitor.visit_expr(&mut interpolation.value);
            }
        }
        ExprKind::Lambda(lambda) => walk_lambda(visitor, lambda),
        ExprKind::Yield { operand } => {
            if let Some(operand) = operand {
                visitor.visit_expr(operand);
            }
        }
        ExprKind::Assign { target, value, .. } | ExprKind::AugAssign { target, value, .. } => {
            visitor.visit_expr(target);
            visitor.visit_expr(value);
        }
    }
}

fn walk_function<V: Visitor + ?Sized>(visitor: &mut V, function: &mut FunctionDef) {
    walk_params(visitor, &mut function.parameters);
    visitor.visit_stmts(&mut function.body);
}

fn walk_params<V: Visitor + ?Sized>(visitor: &mut V, params: &mut ParamList) {
    for param in &mut params.params {
        if let Some(default) = &mut param.default {
            visitor.visit_expr(default);
        }
    }
    if let Some(rest) = &mut params.rest
        && let Some(default) = &mut rest.default
    {
        visitor.visit_expr(default);
    }
}

fn walk_lambda<V: Visitor + ?Sized>(visitor: &mut V, lambda: &mut LambdaLit) {
    walk_params(visitor, &mut lambda.parameters);
    match &mut lambda.body {
        LambdaBody::Block(stmts) => visitor.visit_stmts(stmts),
        LambdaBody::Expression(expr) => visitor.visit_expr(expr),
    }
}
