use super::super::ast::{
    BinaryOp, ClassMemberKind, Expr, ExprKind, FstringPart, FunctionDef, LambdaBody, LambdaLit,
    Literal, ParamList, Stmt, StmtKind, UnaryOp,
};
use crate::utils::repeat_string as repeat_checked;
use num_bigint::{BigInt, Sign};
use num_traits::{FromPrimitive, Signed, ToPrimitive, Zero};
use std::cmp::Ordering;
use unicode_normalization::UnicodeNormalization;

pub(super) const MAX_STRING_BYTES: usize = 64 * 1024;
pub(super) const MAX_FOLDED_BIGINT_BITS: u64 = 4096;

pub(super) fn fold_stmts(stmts: &mut [Stmt]) {
    for stmt in stmts {
        fold_stmt(&mut stmt.kind);
    }
}

#[allow(clippy::too_many_lines)]
fn fold_stmt(stmt: &mut StmtKind) {
    match stmt {
        StmtKind::Expression { expr } | StmtKind::Throw { expr, .. } => fold_expr(expr),
        StmtKind::VarDecl { initializer, .. } => {
            if let Some(initializer) = initializer {
                fold_expr(initializer);
            }
        }
        StmtKind::Block { stmts, .. } => fold_stmts(stmts),
        StmtKind::FunctionDecl {
            fn_lit, decorators, ..
        } => {
            fold_function(fn_lit);
            fold_exprs(decorators);
        }
        StmtKind::Class(class) => {
            if let Some((superclass, _)) = &mut class.superclass {
                fold_expr(superclass);
            }
            for member in &mut class.members {
                match &mut member.kind {
                    ClassMemberKind::Variable(variable) => {
                        if let Some(annotation) = &mut variable.annotation {
                            fold_expr(annotation);
                        }
                        if let Some(default) = &mut variable.default {
                            fold_expr(default);
                        }
                    }
                    ClassMemberKind::Method(method) => fold_function(&mut method.fn_lit),
                }
            }
            fold_exprs(&mut class.decorators);
        }
        StmtKind::Conditional {
            condition,
            then_block,
            else_block,
            ..
        } => {
            fold_expr(condition);
            fold_stmts(then_block);
            if let Some(else_block) = else_block {
                fold_stmts(else_block);
            }
        }
        StmtKind::Loop {
            condition, body, ..
        } => {
            fold_expr(condition);
            fold_stmts(body);
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
                fold_stmt(init);
            }
            if let Some(condition) = condition {
                fold_expr(condition);
            }
            if let Some(increment) = increment {
                fold_expr(increment);
            }
            fold_stmts(body);
        }
        StmtKind::Foreach { iterable, body, .. } => {
            fold_expr(iterable);
            fold_stmts(body);
        }
        StmtKind::Return { value } => {
            if let Some(value) = value {
                fold_expr(value);
            }
        }
        StmtKind::Try(try_stmt) => {
            fold_stmts(&mut try_stmt.body);
            for catch in &mut try_stmt.catches {
                fold_exprs(&mut catch.matchers);
                fold_stmts(&mut catch.body);
            }
            if let Some(else_block) = &mut try_stmt.else_block {
                fold_stmts(else_block);
            }
        }
        StmtKind::Switch { subject, cases, .. } => {
            fold_expr(subject);
            for case in cases {
                if let Some(value) = &mut case.value {
                    fold_expr(value);
                }
                fold_stmts(&mut case.body);
            }
        }
    }
}

fn fold_function(function: &mut FunctionDef) {
    fold_params(&mut function.parameters);
    fold_stmts(&mut function.body);
}

fn fold_params(params: &mut ParamList) {
    for param in &mut params.params {
        if let Some(default) = &mut param.default {
            fold_expr(default);
        }
    }
    if let Some(rest) = &mut params.rest
        && let Some(default) = &mut rest.default
    {
        fold_expr(default);
    }
}

fn fold_lambda(lambda: &mut LambdaLit) {
    fold_params(&mut lambda.parameters);
    match &mut lambda.body {
        LambdaBody::Block(stmts) => fold_stmts(stmts),
        LambdaBody::Expression(expr) => fold_expr(expr),
    }
}

fn fold_exprs(exprs: &mut [Expr]) {
    for expr in exprs {
        fold_expr(expr);
    }
}

pub(super) fn fold_expr(expr: &mut Expr) {
    match &mut expr.kind {
        ExprKind::Literal(_) | ExprKind::Variable { .. } | ExprKind::Super { .. } => {}
        ExprKind::Property { object, .. } => fold_expr(object),
        ExprKind::Subscript { object, index, .. } => {
            fold_expr(object);
            fold_expr(index);
        }
        ExprKind::Call { callee, args, .. } => {
            fold_expr(callee);
            for arg in args {
                fold_expr(&mut arg.value);
            }
        }
        ExprKind::Unary { operand, .. } => fold_expr(operand),
        ExprKind::Binary { lhs, rhs, .. } | ExprKind::Logical { lhs, rhs, .. } => {
            fold_expr(lhs);
            fold_expr(rhs);
        }
        ExprKind::Ternary {
            condition,
            then,
            else_,
            ..
        } => {
            fold_expr(condition);
            fold_expr(then);
            fold_expr(else_);
        }
        ExprKind::Grouping { inner } => fold_expr(inner),
        ExprKind::Tuple { elements } => fold_exprs(elements),
        ExprKind::ListLit { items } | ExprKind::SetLit { items } => fold_exprs(items),
        ExprKind::DictLit { entries } => {
            for (key, value) in entries {
                fold_expr(key);
                fold_expr(value);
            }
        }
        ExprKind::Fstring { parts } => {
            for part in parts {
                if let FstringPart::Interp(expr) = part {
                    fold_expr(expr);
                }
            }
        }
        ExprKind::Tstring(tstring) => {
            for interpolation in &mut tstring.interpolations {
                fold_expr(&mut interpolation.value);
            }
        }
        ExprKind::Lambda(lambda) => fold_lambda(lambda),
        ExprKind::Yield { operand } => {
            if let Some(operand) = operand {
                fold_expr(operand);
            }
        }
        ExprKind::Assign { target, value, .. } | ExprKind::AugAssign { target, value, .. } => {
            fold_expr(target);
            fold_expr(value);
        }
    }

    fold_literal_expr(expr);
}

fn fold_literal_expr(expr: &mut Expr) {
    if let Some(literal) = folded_literal(expr) {
        expr.kind = ExprKind::Literal(literal);
    }
}

fn folded_literal(expr: &Expr) -> Option<Literal> {
    match &expr.kind {
        ExprKind::Unary { op, operand, .. } => fold_unary(*op, literal(operand)?),
        ExprKind::Binary { op, lhs, rhs, .. } => fold_binary(*op, literal(lhs)?, literal(rhs)?),
        ExprKind::Tuple { elements } => Some(Literal::Tuple(
            elements
                .iter()
                .map(|element| literal(element).cloned())
                .collect::<Option<Vec<_>>>()?,
        )),
        ExprKind::Grouping { inner } => literal(inner).cloned(),
        _ => None,
    }
}

pub(super) fn literal(expr: &Expr) -> Option<&Literal> {
    match &expr.kind {
        ExprKind::Literal(literal) => Some(literal),
        ExprKind::Grouping { inner } => literal(inner),
        _ => None,
    }
}

pub(super) fn fold_unary(op: UnaryOp, operand: &Literal) -> Option<Literal> {
    match op {
        UnaryOp::Neg => fold_neg(operand),
        UnaryOp::Not => Some(fold_not(operand)),
    }
}

pub(super) fn fold_neg(operand: &Literal) -> Option<Literal> {
    match operand {
        Literal::Int(value) => Some(Literal::Int(value.checked_neg()?)),
        Literal::Float(value) => Some(Literal::Float(-value)),
        Literal::Rational {
            numerator,
            denominator,
        } => Some(Literal::Rational {
            numerator: -numerator.clone(),
            denominator: denominator.clone(),
        }),
        Literal::BigInt(_)
        | Literal::Nil
        | Literal::True
        | Literal::False
        | Literal::StopIteration
        | Literal::Str(_)
        | Literal::Tuple(_) => None,
    }
}

pub(super) fn literal_truthy(operand: &Literal) -> bool {
    match operand {
        Literal::Nil | Literal::False => false,
        Literal::True | Literal::StopIteration => true,
        Literal::BigInt(value) => !value.is_zero(),
        Literal::Float(value) => *value != 0.0,
        Literal::Rational { numerator, .. } => !numerator.is_zero(),
        Literal::Int(value) => *value != 0,
        Literal::Str(value) => !value.is_empty(),
        Literal::Tuple(items) => !items.is_empty(),
    }
}

pub(super) fn fold_not(operand: &Literal) -> Literal {
    if literal_truthy(operand) {
        Literal::False
    } else {
        Literal::True
    }
}

pub(super) fn fold_binary(op: BinaryOp, lhs: &Literal, rhs: &Literal) -> Option<Literal> {
    match op {
        BinaryOp::Add => match (lhs, rhs) {
            (Literal::Str(lhs), Literal::Str(rhs)) => fold_string_concat(lhs, rhs),
            _ => fold_numeric(op, lhs, rhs),
        },
        BinaryOp::Sub => fold_numeric(op, lhs, rhs),
        BinaryOp::Mul => match (lhs, rhs) {
            (Literal::Str(value), count) | (count, Literal::Str(value)) => {
                fold_string_repeat(value, count)
            }
            _ => fold_numeric(op, lhs, rhs),
        },
        BinaryOp::Div => fold_division(lhs, rhs),
        BinaryOp::FloorDiv => fold_floor_div(lhs, rhs),
        BinaryOp::Mod => fold_mod(lhs, rhs),
        BinaryOp::Exp => fold_exp(lhs, rhs),
        BinaryOp::Equal => fold_equality(lhs, rhs).map(Literal::from_bool),
        BinaryOp::NotEqual => fold_equality(lhs, rhs).map(|value| Literal::from_bool(!value)),
        BinaryOp::Less => {
            fold_ordering(lhs, rhs).map(|ordering| Literal::from_bool(ordering == Ordering::Less))
        }
        BinaryOp::LessEqual => fold_ordering(lhs, rhs)
            .map(|ordering| Literal::from_bool(ordering != Ordering::Greater)),
        BinaryOp::Greater => fold_ordering(lhs, rhs)
            .map(|ordering| Literal::from_bool(ordering == Ordering::Greater)),
        BinaryOp::GreaterEqual => {
            fold_ordering(lhs, rhs).map(|ordering| Literal::from_bool(ordering != Ordering::Less))
        }
        _ => None,
    }
}

impl Literal {
    fn from_bool(value: bool) -> Self {
        if value { Self::True } else { Self::False }
    }
}

fn fold_equality(lhs: &Literal, rhs: &Literal) -> Option<bool> {
    match (lhs, rhs) {
        (Literal::Str(lhs), Literal::Str(rhs)) => Some(lhs.nfc().eq(rhs.nfc())),
        (Literal::True, Literal::True) | (Literal::False, Literal::False) => Some(true),
        (Literal::True, Literal::False) | (Literal::False, Literal::True) => Some(false),
        (Literal::Nil, Literal::Nil) | (Literal::StopIteration, Literal::StopIteration) => {
            Some(true)
        }
        (Literal::Tuple(lhs), Literal::Tuple(rhs)) => {
            if lhs.len() != rhs.len() {
                return Some(false);
            }
            let mut equal = true;
            for (lhs, rhs) in lhs.iter().zip(rhs) {
                equal &= fold_equality(lhs, rhs)?;
            }
            Some(equal)
        }
        _ if lhs.is_number() && rhs.is_number() => numeric_equality(lhs, rhs),
        _ => Some(false),
    }
}

fn fold_ordering(lhs: &Literal, rhs: &Literal) -> Option<Ordering> {
    match (lhs, rhs) {
        (Literal::Str(lhs), Literal::Str(rhs)) => Some(lhs.nfc().cmp(rhs.nfc())),
        _ if lhs.is_number() && rhs.is_number() => numeric_ordering(lhs, rhs),
        _ => None,
    }
}

impl Literal {
    fn is_number(&self) -> bool {
        matches!(
            self,
            Self::Int(_) | Self::BigInt(_) | Self::Float(_) | Self::Rational { .. }
        )
    }
}

fn numeric_equality(lhs: &Literal, rhs: &Literal) -> Option<bool> {
    if let (Literal::Float(lhs), Literal::Float(rhs)) = (lhs, rhs) {
        return Some(lhs.partial_cmp(rhs) == Some(Ordering::Equal));
    }
    numeric_ordering(lhs, rhs).map(|ordering| ordering == Ordering::Equal)
}

fn numeric_ordering(lhs: &Literal, rhs: &Literal) -> Option<Ordering> {
    match (lhs, rhs) {
        (Literal::Float(lhs), Literal::Float(rhs)) => lhs.partial_cmp(rhs),
        (Literal::Float(lhs), rhs) => int_float_cmp(&rhs.as_bigint()?, *lhs).map(Ordering::reverse),
        (lhs, Literal::Float(rhs)) => int_float_cmp(&lhs.as_bigint()?, *rhs),
        _ => rational_ordering(lhs, rhs),
    }
}

impl Literal {
    fn as_bigint(&self) -> Option<BigInt> {
        match self {
            Self::Int(value) => Some(BigInt::from(*value)),
            Self::BigInt(value) => Some(value.clone()),
            _ => None,
        }
    }

    fn as_rational(&self) -> Option<(BigInt, BigInt)> {
        match self {
            Self::Int(value) => Some((BigInt::from(*value), BigInt::from(1))),
            Self::BigInt(value) => Some((value.clone(), BigInt::from(1))),
            Self::Rational {
                numerator,
                denominator,
            } => Some((numerator.clone(), denominator.clone())),
            _ => None,
        }
    }
}

fn rational_ordering(lhs: &Literal, rhs: &Literal) -> Option<Ordering> {
    let (lhs_numerator, lhs_denominator) = lhs.as_rational()?;
    let (rhs_numerator, rhs_denominator) = rhs.as_rational()?;
    Some((lhs_numerator * rhs_denominator).cmp(&(rhs_numerator * lhs_denominator)))
}

fn int_float_cmp(int: &BigInt, float: f64) -> Option<Ordering> {
    if float.is_nan() {
        None
    } else if float.fract() == 0.0
        && let Some(whole) = BigInt::from_f64(float)
    {
        Some(int.cmp(&whole))
    } else {
        int.to_f64().unwrap_or(f64::INFINITY).partial_cmp(&float)
    }
}

pub(super) fn fold_string_concat(lhs: &str, rhs: &str) -> Option<Literal> {
    let len = lhs.len().checked_add(rhs.len())?;
    if len > MAX_STRING_BYTES {
        return None;
    }
    let mut value = String::with_capacity(len);
    value.push_str(lhs);
    value.push_str(rhs);
    Some(Literal::Str(value))
}

pub(super) fn fold_string_repeat(value: &str, count: &Literal) -> Option<Literal> {
    match count {
        Literal::Int(count) => {
            if *count <= 0 {
                return Some(Literal::Str(String::new()));
            }
            if value.is_empty() {
                return Some(Literal::Str(String::new()));
            }
            let count = usize::try_from(*count).ok()?;
            repeat_string(value, count)
        }
        Literal::BigInt(count) => {
            match count.sign() {
                Sign::Minus | Sign::NoSign => return Some(Literal::Str(String::new())),
                Sign::Plus => {}
            }
            if value.is_empty() {
                return Some(Literal::Str(String::new()));
            }
            let count = count.to_usize()?;
            repeat_string(value, count)
        }
        _ => None,
    }
}

pub(super) fn repeat_string(value: &str, count: usize) -> Option<Literal> {
    repeat_checked(value, count, MAX_STRING_BYTES).map(Literal::Str)
}

#[allow(clippy::cast_precision_loss)]
pub(super) fn fold_numeric(op: BinaryOp, lhs: &Literal, rhs: &Literal) -> Option<Literal> {
    match (lhs, rhs) {
        (Literal::Int(lhs), Literal::Int(rhs)) => Some(fold_small(op, *lhs, *rhs)),
        (Literal::BigInt(lhs), Literal::BigInt(rhs)) => fold_bigint(op, lhs, rhs),
        (Literal::BigInt(lhs), Literal::Int(rhs)) => fold_bigint(op, lhs, &BigInt::from(*rhs)),
        (Literal::Int(lhs), Literal::BigInt(rhs)) => fold_bigint(op, &BigInt::from(*lhs), rhs),
        (Literal::Float(lhs), Literal::Float(rhs)) => {
            Some(Literal::Float(fold_float(op, *lhs, *rhs)))
        }
        (Literal::Int(lhs), Literal::Float(rhs)) => {
            Some(Literal::Float(fold_float(op, *lhs as f64, *rhs)))
        }
        (Literal::Float(lhs), Literal::Int(rhs)) => {
            Some(Literal::Float(fold_float(op, *lhs, *rhs as f64)))
        }
        (Literal::BigInt(lhs), Literal::Float(rhs)) => {
            Some(Literal::Float(fold_float(op, big_to_f64(lhs), *rhs)))
        }
        (Literal::Float(lhs), Literal::BigInt(rhs)) => {
            Some(Literal::Float(fold_float(op, *lhs, big_to_f64(rhs))))
        }
        _ => None,
    }
}

fn fold_bigint(op: BinaryOp, lhs: &BigInt, rhs: &BigInt) -> Option<Literal> {
    if lhs.bits() > MAX_FOLDED_BIGINT_BITS || rhs.bits() > MAX_FOLDED_BIGINT_BITS {
        return None;
    }
    Some(Literal::BigInt(match op {
        BinaryOp::Add => lhs + rhs,
        BinaryOp::Sub => lhs - rhs,
        BinaryOp::Mul => lhs * rhs,
        _ => unreachable!("only additive and multiplicative operators are folded"),
    }))
}

pub(super) fn fold_small(op: BinaryOp, lhs: i64, rhs: i64) -> Literal {
    let value = match op {
        BinaryOp::Add => lhs.checked_add(rhs),
        BinaryOp::Sub => lhs.checked_sub(rhs),
        BinaryOp::Mul => lhs.checked_mul(rhs),
        _ => unreachable!("only additive and multiplicative operators are folded"),
    };
    if let Some(value) = value {
        return Literal::Int(value);
    }
    let lhs = BigInt::from(lhs);
    let rhs = BigInt::from(rhs);
    Literal::BigInt(match op {
        BinaryOp::Add => lhs + rhs,
        BinaryOp::Sub => lhs - rhs,
        BinaryOp::Mul => lhs * rhs,
        _ => unreachable!("only additive and multiplicative operators are folded"),
    })
}

pub(super) fn fold_float(op: BinaryOp, lhs: f64, rhs: f64) -> f64 {
    match op {
        BinaryOp::Add => lhs + rhs,
        BinaryOp::Sub => lhs - rhs,
        BinaryOp::Mul => lhs * rhs,
        _ => unreachable!("only additive and multiplicative operators are folded"),
    }
}

#[allow(clippy::cast_precision_loss)]
pub(super) fn fold_division(lhs: &Literal, rhs: &Literal) -> Option<Literal> {
    match (lhs, rhs) {
        (Literal::Int(_) | Literal::BigInt(_), Literal::Int(_) | Literal::BigInt(_)) => {
            fold_rational(lhs, rhs)
        }
        (Literal::Float(lhs), Literal::Float(rhs)) if *rhs != 0.0 => {
            Some(Literal::Float(lhs / rhs))
        }
        (Literal::Int(lhs), Literal::Float(rhs)) if *rhs != 0.0 => {
            Some(Literal::Float(*lhs as f64 / rhs))
        }
        (Literal::Float(lhs), Literal::Int(rhs)) if *rhs != 0 => {
            Some(Literal::Float(lhs / *rhs as f64))
        }
        (Literal::BigInt(lhs), Literal::Float(rhs)) if *rhs != 0.0 => {
            Some(Literal::Float(big_to_f64(lhs) / rhs))
        }
        (Literal::Float(lhs), Literal::BigInt(rhs)) if !rhs.is_zero() => {
            Some(Literal::Float(lhs / big_to_f64(rhs)))
        }
        _ => None,
    }
}

pub(super) fn fold_rational(numerator: &Literal, denominator: &Literal) -> Option<Literal> {
    let mut numerator = literal_bigint(numerator)?;
    let mut denominator = literal_bigint(denominator)?;
    if numerator.bits() > MAX_FOLDED_BIGINT_BITS || denominator.bits() > MAX_FOLDED_BIGINT_BITS {
        return None;
    }
    if denominator.is_zero() {
        return None;
    }
    let gcd = big_gcd(&numerator, &denominator);
    numerator /= &gcd;
    denominator /= &gcd;
    if denominator.sign() == Sign::Minus {
        numerator = -numerator;
        denominator = -denominator;
    }
    Some(Literal::Rational {
        numerator,
        denominator,
    })
}

fn literal_bigint(literal: &Literal) -> Option<BigInt> {
    match literal {
        Literal::Int(value) => Some(BigInt::from(*value)),
        Literal::BigInt(value) => Some(value.clone()),
        _ => None,
    }
}

#[allow(clippy::cast_precision_loss)]
pub(super) fn fold_floor_div(lhs: &Literal, rhs: &Literal) -> Option<Literal> {
    match (lhs, rhs) {
        (Literal::Int(lhs), Literal::Int(rhs)) if *rhs != 0 => fold_small_floor_div(*lhs, *rhs),
        (Literal::Float(lhs), Literal::Float(rhs)) if *rhs != 0.0 => {
            Some(Literal::Float((lhs / rhs).floor()))
        }
        (Literal::Int(lhs), Literal::Float(rhs)) if *rhs != 0.0 => {
            Some(Literal::Float((*lhs as f64 / rhs).floor()))
        }
        (Literal::Float(lhs), Literal::Int(rhs)) if *rhs != 0 => {
            Some(Literal::Float((lhs / *rhs as f64).floor()))
        }
        (Literal::BigInt(lhs), Literal::Float(rhs)) if *rhs != 0.0 => {
            Some(Literal::Float((big_to_f64(lhs) / rhs).floor()))
        }
        (Literal::Float(lhs), Literal::BigInt(rhs)) if !rhs.is_zero() => {
            Some(Literal::Float((lhs / big_to_f64(rhs)).floor()))
        }
        _ => None,
    }
}

pub(super) fn fold_small_floor_div(lhs: i64, rhs: i64) -> Option<Literal> {
    lhs.checked_div(rhs).map(Literal::Int)
}

#[allow(clippy::cast_precision_loss)]
pub(super) fn fold_mod(lhs: &Literal, rhs: &Literal) -> Option<Literal> {
    match (lhs, rhs) {
        (Literal::Int(lhs), Literal::Int(rhs)) if *rhs != 0 => Some(fold_small_mod(*lhs, *rhs)),
        (Literal::Float(lhs), Literal::Float(rhs)) if *rhs != 0.0 => {
            Some(Literal::Float(lhs % rhs))
        }
        (Literal::Int(lhs), Literal::Float(rhs)) if *rhs != 0.0 => {
            Some(Literal::Float(*lhs as f64 % rhs))
        }
        (Literal::Float(lhs), Literal::Int(rhs)) if *rhs != 0 => {
            Some(Literal::Float(lhs % *rhs as f64))
        }
        (Literal::BigInt(lhs), Literal::Float(rhs)) if *rhs != 0.0 => {
            Some(Literal::Float(big_to_f64(lhs) % rhs))
        }
        (Literal::Float(lhs), Literal::BigInt(rhs)) if !rhs.is_zero() => {
            Some(Literal::Float(lhs % big_to_f64(rhs)))
        }
        _ => None,
    }
}

pub(super) fn fold_small_mod(lhs: i64, rhs: i64) -> Literal {
    Literal::Int(lhs.checked_rem(rhs).unwrap_or(0))
}

pub(super) fn fold_exp(lhs: &Literal, rhs: &Literal) -> Option<Literal> {
    match (lhs, rhs) {
        (Literal::Int(base), Literal::Int(exponent)) => {
            if *exponent < 0 {
                return None;
            }
            u32::try_from(*exponent)
                .ok()
                .and_then(|exponent| base.checked_pow(exponent))
                .map(Literal::Int)
        }
        _ => None,
    }
}

pub(super) fn big_gcd(lhs: &BigInt, rhs: &BigInt) -> BigInt {
    let mut lhs = lhs.abs();
    let mut rhs = rhs.abs();
    while !rhs.is_zero() {
        let remainder = &lhs % &rhs;
        lhs = rhs;
        rhs = remainder;
    }
    lhs
}

pub(super) fn big_to_f64(value: &BigInt) -> f64 {
    value.to_f64().unwrap_or(f64::INFINITY)
}
