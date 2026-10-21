//! Canonical source formatting for parsed programs.
//!
//! The formatter walks the AST and prints a canonical token layout with
//! configurable indentation (four spaces by default). It deliberately does
//! not preserve the original line breaks or comments.
//!
//! **Comments are currently discarded.** The scanner skips them before the
//! parser runs, and the AST has no place to attach them, so formatting a
//! file loses every `#` comment. Preserving comments needs comment tokens
//! (or trivia) to be retained and attached to AST nodes first.

use super::ast::{
    AccessMode, BinaryOp, BoolOperator, CatchClause, ClassDecl, ClassMember, ClassVar, Expr,
    ExprKind, FstringPart, FunctionKind, Literal, Method, ParamList, Program, StmtKind, SwitchCase,
    TryStmt, UnaryOp,
};
use super::ast_eq::LocationLessEq;
use super::front::Parser;
use super::rules::Precedence;
use crate::scanner::Scanner;
use crate::types::{ConditionType, LoopType, Mutability};

/// Formatting configuration.
#[derive(Debug, Clone, Copy)]
pub struct FormatOptions {
    /// Number of spaces per indentation level.
    pub indent_width: usize,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self { indent_width: 4 }
    }
}

/// Format Generic source into canonical, comment-free source.
///
/// The source is parsed with the normal compiler front end; parse errors
/// are returned as the compiler's error messages. Comments are **not**
/// preserved: the scanner drops them and the AST has no comment nodes.
///
/// Before returning, the formatted source is re-parsed and checked: it must
/// produce the same AST (ignoring source locations) and formatting it again
/// must be idempotent.
pub fn format_source(source: &str, options: FormatOptions) -> Result<String, Vec<String>> {
    let program = parse(source)?;
    let formatted = Formatter::new(options).format_program(&program);

    let reparsed = parse(&formatted).map_err(|errors| {
        vec![format!(
            "internal formatter error: formatted output failed to parse:\n{}",
            errors.join("\n")
        )]
    })?;
    if !program.loc_eq(&reparsed) {
        return Err(vec![
            "internal formatter error: formatting changed the AST".to_string(),
        ]);
    }

    let again = Formatter::new(options).format_program(&reparsed);
    if again != formatted {
        return Err(vec![
            "internal formatter error: formatting is not idempotent".to_string(),
        ]);
    }

    Ok(formatted)
}

/// Format Generic source with the default [`FormatOptions`].
pub fn format_source_with_default(source: &str) -> Result<String, Vec<String>> {
    format_source(source, FormatOptions::default())
}

fn parse(source: &str) -> Result<Program, Vec<String>> {
    let mut parser = Parser::new(
        Scanner::new(
            source,
            #[cfg(feature = "debug_scanner")]
            false,
        ),
        #[cfg(feature = "debug_parser")]
        false,
    );
    let program = parser.parse_program();
    if parser.had_error {
        return Err(std::mem::take(&mut parser.errors));
    }
    Ok(program)
}

struct Formatter {
    out: String,
    indent_unit: String,
}

impl Formatter {
    fn new(options: FormatOptions) -> Self {
        Self {
            out: String::new(),
            indent_unit: " ".repeat(options.indent_width),
        }
    }

    fn format_program(mut self, program: &Program) -> String {
        for declaration in &program.declarations {
            self.statement(declaration, 0);
        }
        self.out
    }

    fn indent(&mut self, level: usize) {
        for _ in 0..level {
            self.out.push_str(&self.indent_unit);
        }
    }

    fn indented(&mut self, level: usize, text: &str) {
        self.indent(level);
        self.out.push_str(text);
    }

    fn newline(&mut self) {
        self.out.push('\n');
    }

    /// Write a block at the current output position: the caller has already
    /// emitted and indented the line containing the opening brace.
    fn block(&mut self, statements: &[StmtKind], level: usize) {
        if statements.is_empty() {
            self.out.push_str("{}");
            return;
        }
        self.out.push_str("{\n");
        for statement in statements {
            self.statement(statement, level + 1);
        }
        self.indented(level, "}");
    }

    fn statement(&mut self, statement: &StmtKind, level: usize) {
        match statement {
            StmtKind::Expression { expr } => {
                self.indent(level);
                self.expr(expr, Precedence::Assignment, level);
                self.out.push_str(";\n");
            }
            StmtKind::VarDecl {
                name,
                mutability,
                initializer,
                ..
            } => {
                self.variable_declaration_statement(
                    name,
                    *mutability,
                    initializer.as_deref(),
                    level,
                );
            }
            StmtKind::Block { stmts } => {
                self.indent(level);
                self.block(stmts, level);
                self.newline();
            }
            StmtKind::FunctionDecl {
                name,
                kind,
                fn_lit,
                decorators,
            } => {
                self.decorators(decorators, level);
                self.indented(
                    level,
                    match kind {
                        FunctionKind::Function => "fun ",
                        FunctionKind::Generator => "gen ",
                    },
                );
                self.out.push_str(name);
                self.parameters(&fn_lit.parameters, level);
                self.out.push(' ');
                self.block(&fn_lit.body, level);
                self.newline();
            }
            StmtKind::Class(class) => {
                self.decorators(&class.decorators, level);
                self.class(class, level);
                self.newline();
            }
            StmtKind::Conditional {
                condition_type,
                condition,
                then_block,
                else_block,
                ..
            } => {
                self.indented(
                    level,
                    match condition_type {
                        ConditionType::If => "if ",
                        ConditionType::Unless => "unless ",
                    },
                );
                self.expr(condition, Precedence::None, level);
                self.out.push(' ');
                self.block(then_block, level);
                if let Some(else_block) = else_block {
                    self.out.push_str(" else ");
                    self.block(else_block, level);
                }
                self.newline();
            }
            StmtKind::Loop {
                loop_type,
                condition,
                body,
                label,
                ..
            } => {
                self.indented(
                    level,
                    match loop_type {
                        LoopType::While => "while",
                        LoopType::Until => "until",
                    },
                );
                self.label(label);
                self.out.push(' ');
                self.expr(condition, Precedence::None, level);
                self.out.push(' ');
                self.block(body, level);
                self.newline();
            }
            StmtKind::Break { label } => {
                self.indented(level, "break");
                self.label(label);
                self.out.push_str(";\n");
            }
            StmtKind::Continue { label, .. } => {
                self.indented(level, "continue");
                self.label(label);
                self.out.push_str(";\n");
            }
            StmtKind::For {
                label,
                init,
                condition,
                increment,
                body,
                ..
            } => {
                self.indented(level, "for");
                self.label(label);
                self.out.push_str(" (");
                if let Some(init) = init {
                    self.for_initializer(init, level);
                }
                self.out.push_str("; ");
                if let Some(condition) = condition {
                    self.expr(condition, Precedence::Assignment, level);
                }
                self.out.push_str("; ");
                if let Some(increment) = increment {
                    self.expr(increment, Precedence::Assignment, level);
                }
                self.out.push_str(") ");
                self.block(body, level);
                self.newline();
            }
            StmtKind::Foreach {
                label,
                mutability,
                var_name,
                iterable,
                body,
                ..
            } => {
                self.indented(level, "foreach");
                self.label(label);
                self.out.push_str(" (");
                self.mutability(*mutability);
                self.out.push(' ');
                self.out.push_str(var_name);
                self.out.push_str(" in ");
                self.expr(iterable, Precedence::Assignment, level);
                self.out.push_str(") ");
                self.block(body, level);
                self.newline();
            }
            StmtKind::Return { value } => {
                self.indented(level, "return");
                if let Some(value) = value {
                    self.out.push(' ');
                    self.expr(value, Precedence::Assignment, level);
                }
                self.out.push_str(";\n");
            }
            StmtKind::Throw { expr, .. } => {
                self.indented(level, "throw ");
                self.expr(expr, Precedence::Assignment, level);
                self.out.push_str(";\n");
            }
            StmtKind::Try(try_stmt) => {
                self.try_statement(try_stmt, level);
            }
            StmtKind::Switch { subject, cases, .. } => {
                self.indented(level, "switch ");
                self.expr(subject, Precedence::None, level);
                self.out.push(' ');
                self.switch_body(cases, level);
                self.newline();
            }
            StmtKind::Import { path, alias, .. } => {
                self.indented(level, "import \"");
                self.out.push_str(path);
                self.out.push('"');
                if let Some(alias) = alias {
                    self.out.push_str(" as ");
                    self.out.push_str(alias);
                }
                self.out.push_str(";\n");
            }
            StmtKind::ImportFrom { path, names, .. } => {
                self.indented(level, "from \"");
                self.out.push_str(path);
                self.out.push_str("\" import ");
                let parenthesized = names.len() > 1;
                if parenthesized {
                    self.out.push('(');
                }
                for (index, (name, _)) in names.iter().enumerate() {
                    if index > 0 {
                        self.out.push_str(", ");
                    }
                    self.out.push_str(name);
                }
                if parenthesized {
                    self.out.push(')');
                }
                self.out.push_str(";\n");
            }
        }
    }

    fn decorators(&mut self, decorators: &[Expr], level: usize) {
        for decorator in decorators {
            self.indented(level, "@");
            self.expr(decorator, Precedence::Assignment, level);
            self.newline();
        }
    }

    fn class(&mut self, class: &ClassDecl, level: usize) {
        self.indented(level, "class ");
        self.out.push_str(&class.name);
        if let Some((superclass, _)) = &class.superclass {
            self.out.push_str(" < ");
            self.expr(superclass, Precedence::Assignment, level);
        }
        self.out.push(' ');
        if class.members.is_empty() {
            self.out.push_str("{}");
            return;
        }
        self.out.push_str("{\n");
        for member in &class.members {
            match member {
                ClassMember::Variable(variable) => self.class_variable(variable, level + 1),
                ClassMember::Method(method) => self.method(method, level + 1),
            }
        }
        self.indented(level, "}");
    }

    fn class_variable(&mut self, variable: &ClassVar, level: usize) {
        self.indented(level, "var ");
        self.out.push_str(&variable.name);
        if let Some(annotation) = &variable.annotation {
            self.out.push_str(": ");
            self.expr_single(annotation, Precedence::Tuple, level);
        }
        if let Some(default) = &variable.default {
            self.out.push_str(" = ");
            self.expr(default, Precedence::Assignment, level);
        }
        self.out.push_str(";\n");
    }

    fn method(&mut self, method: &Method, level: usize) {
        self.indented(level, &method.name);
        self.parameters(&method.fn_lit.parameters, level);
        self.out.push(' ');
        self.block(&method.fn_lit.body, level);
        self.newline();
    }

    fn try_statement(&mut self, try_stmt: &TryStmt, level: usize) {
        self.indented(level, "try ");
        self.block(&try_stmt.body, level);
        for catch in &try_stmt.catches {
            self.catch_clause(catch, level);
        }
        if let Some(else_block) = &try_stmt.else_block {
            self.out.push_str(" else ");
            self.block(else_block, level);
        }
        self.newline();
    }

    fn catch_clause(&mut self, catch: &CatchClause, level: usize) {
        self.out.push_str(" catch");
        if catch.matchers.len() == 1 {
            let matcher = &catch.matchers[0];
            let needs_catch_parentheses = match &matcher.kind {
                ExprKind::Grouping { .. } => true,
                ExprKind::Tuple { elements } => elements.is_empty(),
                _ => false,
            };
            if needs_catch_parentheses {
                self.out.push_str(" (");
                self.expr(matcher, Precedence::Assignment, level);
                self.out.push(')');
            } else {
                self.out.push(' ');
                self.expr(matcher, Precedence::Assignment, level);
            }
        } else {
            self.out.push_str(" (");
            for (index, matcher) in catch.matchers.iter().enumerate() {
                if index > 0 {
                    self.out.push_str(", ");
                }
                self.expr_single(matcher, Precedence::Assignment, level);
            }
            self.out.push(')');
        }
        if let Some((name, _)) = &catch.binding {
            self.out.push_str(" as ");
            self.out.push_str(name);
        }
        self.out.push(' ');
        self.block(&catch.body, level);
    }

    fn switch_body(&mut self, cases: &[SwitchCase], level: usize) {
        if cases.is_empty() {
            self.out.push_str("{}");
            return;
        }
        self.out.push_str("{\n");
        for case in cases {
            if let Some(value) = &case.value {
                self.indented(level + 1, "case ");
                self.expr(value, Precedence::Assignment, level + 1);
                self.out.push(':');
            } else {
                self.indented(level + 1, "default:");
            }
            self.newline();
            for statement in &case.body {
                self.statement(statement, level + 2);
            }
        }
        self.indented(level, "}");
    }

    fn label(&mut self, label: &Option<String>) {
        if let Some(label) = label {
            self.out.push_str(" '");
            self.out.push_str(label);
        }
    }

    fn mutability(&mut self, mutability: Mutability) {
        self.out.push_str(match mutability {
            Mutability::Mutable => "var",
            Mutability::Immutable => "const",
        });
    }

    fn variable_declaration(
        &mut self,
        name: &str,
        mutability: Mutability,
        initializer: Option<&Expr>,
        level: usize,
    ) {
        self.mutability(mutability);
        self.out.push(' ');
        self.out.push_str(name);
        if let Some(initializer) = initializer {
            self.out.push_str(" = ");
            self.expr(initializer, Precedence::Assignment, level);
        }
    }

    fn variable_declaration_statement(
        &mut self,
        name: &str,
        mutability: Mutability,
        initializer: Option<&Expr>,
        level: usize,
    ) {
        self.indent(level);
        self.variable_declaration(name, mutability, initializer, level);
        self.out.push_str(";\n");
    }

    fn for_initializer(&mut self, initializer: &StmtKind, level: usize) {
        match initializer {
            StmtKind::VarDecl {
                name,
                mutability,
                initializer,
                ..
            } => self.variable_declaration(name, *mutability, initializer.as_deref(), level),
            StmtKind::Expression { expr } => {
                self.expr(expr, Precedence::Assignment, level);
            }
            _ => {}
        }
    }

    fn parameters(&mut self, parameters: &ParamList, level: usize) {
        self.out.push('(');
        let mut first = true;
        for parameter in &parameters.params {
            if !first {
                self.out.push_str(", ");
            }
            first = false;
            self.out.push_str(&parameter.name);
            if let Some(default) = &parameter.default {
                self.out.push('=');
                self.expr_single(default, Precedence::Tuple, level);
            }
        }
        if let Some(rest) = &parameters.rest {
            if !first {
                self.out.push_str(", ");
            }
            self.out.push('*');
            self.out.push_str(&rest.name);
        }
        self.out.push(')');
    }

    fn expr(&mut self, expr: &Expr, parent: Precedence, level: usize) {
        let precedence = precedence(&expr.kind);
        let parenthesize = precedence < parent;
        if parenthesize {
            self.out.push('(');
        }
        self.expr_inner(expr, level);
        if parenthesize {
            self.out.push(')');
        }
    }

    /// Write an expression in a position where a bare tuple would change the
    /// surrounding syntax, parenthesizing it when necessary.
    fn expr_single(&mut self, expr: &Expr, parent: Precedence, level: usize) {
        if matches!(
            &expr.kind,
            ExprKind::Tuple { elements } if !elements.is_empty()
        ) {
            self.out.push('(');
            self.expr(expr, Precedence::None, level);
            self.out.push(')');
        } else {
            self.expr(expr, parent, level);
        }
    }

    fn expr_inner(&mut self, expr: &Expr, level: usize) {
        match &expr.kind {
            ExprKind::Literal(literal) => self.literal(literal),
            ExprKind::Variable { name, .. } => self.out.push_str(name),
            ExprKind::Property {
                object, name, mode, ..
            } => {
                self.expr_single(object, Precedence::Call, level);
                self.out.push_str(match mode {
                    AccessMode::Direct => ".",
                    AccessMode::NullSafe => "?.",
                });
                self.out.push_str(name);
            }
            ExprKind::Subscript {
                object,
                index,
                mode,
                ..
            } => {
                self.expr_single(object, Precedence::Call, level);
                self.out.push_str(match mode {
                    AccessMode::Direct => "[",
                    AccessMode::NullSafe => "?[",
                });
                self.expr(index, Precedence::Tuple, level);
                self.out.push(']');
            }
            ExprKind::Call { callee, args, .. } => {
                self.expr_single(callee, Precedence::Call, level);
                self.out.push('(');
                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        self.out.push_str(", ");
                    }
                    if arg.is_spread {
                        self.out.push('*');
                    }
                    self.expr_single(&arg.value, Precedence::Assignment, level);
                }
                self.out.push(')');
            }
            ExprKind::Unary { op, operand, .. } => {
                self.out.push_str(match op {
                    UnaryOp::Neg => "-",
                    UnaryOp::Not => "!",
                });
                self.expr_single(operand, Precedence::Unary, level);
            }
            ExprKind::Binary { op, lhs, rhs, .. } => {
                let precedence = binary_precedence(*op);
                self.expr_single(lhs, binary_lhs_parent(*op, lhs), level);
                if matches!(
                    op,
                    BinaryOp::RangeInclusive | BinaryOp::RangeExclusive | BinaryOp::Rational
                ) {
                    self.out.push_str(binary_operator(*op));
                } else {
                    self.out.push(' ');
                    self.out.push_str(binary_operator(*op));
                    self.out.push(' ');
                }
                self.expr_single(rhs, precedence.tighter(), level);
            }
            ExprKind::Logical { op, lhs, rhs, .. } => {
                let precedence = match op {
                    BoolOperator::And => Precedence::And,
                    BoolOperator::Or => Precedence::Or,
                };
                self.expr_single(lhs, precedence.tighter(), level);
                self.out.push(' ');
                self.out.push_str(match op {
                    BoolOperator::And => "and",
                    BoolOperator::Or => "or",
                });
                self.out.push(' ');
                self.expr_single(rhs, precedence, level);
            }
            ExprKind::Ternary {
                condition,
                then,
                else_,
                ..
            } => {
                self.expr_single(condition, Precedence::Ternary.tighter(), level);
                self.out.push_str(" ? ");
                self.expr_single(then, Precedence::Ternary, level);
                self.out.push_str(" : ");
                self.expr_single(else_, Precedence::Ternary, level);
            }
            ExprKind::Grouping { inner } => {
                self.out.push('(');
                self.expr(inner, Precedence::None, level);
                self.out.push(')');
            }
            ExprKind::Tuple { elements } => {
                if elements.is_empty() {
                    self.out.push_str("()");
                } else {
                    self.tuple_elements(elements, level);
                    if elements.len() == 1 {
                        self.out.push(',');
                    }
                }
            }
            ExprKind::ListLit { items } => {
                self.out.push('[');
                self.sequence(items, level);
                self.out.push(']');
            }
            ExprKind::SetLit { items } => {
                if items.is_empty() {
                    self.out.push_str("{}");
                } else {
                    self.out.push('{');
                    self.sequence(items, level);
                    self.out.push('}');
                }
            }
            ExprKind::DictLit { entries } => {
                if entries.is_empty() {
                    self.out.push_str("{:}");
                } else {
                    self.out.push('{');
                    for (index, (key, value)) in entries.iter().enumerate() {
                        if index > 0 {
                            self.out.push_str(", ");
                        }
                        self.expr_single(key, Precedence::Tuple, level);
                        self.out.push_str(": ");
                        self.expr_single(value, Precedence::Tuple, level);
                    }
                    self.out.push('}');
                }
            }
            ExprKind::Fstring { parts } => {
                self.out.push_str("f\"");
                for part in parts {
                    match part {
                        FstringPart::Str(text) => self.out.push_str(text),
                        FstringPart::Interp(expr) => {
                            self.out.push_str("${");
                            self.expr(expr, Precedence::Assignment, level);
                            self.out.push('}');
                        }
                    }
                }
                self.out.push('"');
            }
            ExprKind::Tstring(tstring) => {
                self.out.push_str("t\"");
                let mut part_index = 0;
                for interpolation in &tstring.interpolations {
                    if let Some(part) = tstring.parts.get(part_index) {
                        self.out.push_str(part);
                        part_index += 1;
                    }
                    self.out.push_str("${");
                    self.out.push_str(&interpolation.source_text);
                    self.out.push('}');
                }
                while let Some(part) = tstring.parts.get(part_index) {
                    self.out.push_str(part);
                    part_index += 1;
                }
                self.out.push('"');
            }
            ExprKind::Lambda(lambda) => {
                self.out.push_str("-> ");
                self.parameters(&lambda.parameters, level);
                self.out.push(' ');
                match &lambda.body {
                    super::ast::LambdaBody::Block(body) => {
                        self.block(body, level);
                    }
                    super::ast::LambdaBody::Expression(expr) => {
                        self.expr(expr, Precedence::Assignment, level);
                    }
                }
            }
            ExprKind::Super { method, .. } => {
                self.out.push_str("super.");
                self.out.push_str(method);
            }
            ExprKind::Yield { operand } => {
                self.out.push_str("yield");
                if let Some(operand) = operand {
                    self.out.push(' ');
                    self.expr(operand, Precedence::Assignment, level);
                }
            }
            ExprKind::Assign { target, value, .. } => {
                self.expr(target, Precedence::Assignment, level);
                self.out.push_str(" = ");
                self.expr(value, Precedence::Assignment, level);
            }
            ExprKind::AugAssign {
                target, op, value, ..
            } => {
                self.expr(target, Precedence::Assignment, level);
                self.out.push(' ');
                self.out.push_str(binary_operator(*op));
                self.out.push_str("= ");
                self.expr(value, Precedence::Assignment, level);
            }
        }
    }

    fn tuple_elements(&mut self, elements: &[Expr], level: usize) {
        for (index, element) in elements.iter().enumerate() {
            if index > 0 {
                self.out.push_str(", ");
            }
            self.expr_single(element, Precedence::Ternary, level);
        }
    }

    fn sequence(&mut self, items: &[Expr], level: usize) {
        for (index, item) in items.iter().enumerate() {
            if index > 0 {
                self.out.push_str(", ");
            }
            self.expr_single(item, Precedence::Tuple, level);
        }
    }

    fn literal(&mut self, literal: &Literal) {
        match literal {
            Literal::Nil => self.out.push_str("nil"),
            Literal::True => self.out.push_str("true"),
            Literal::False => self.out.push_str("false"),
            Literal::StopIteration => self.out.push_str("StopIteration"),
            Literal::Int(value) => self.out.push_str(&value.to_string()),
            Literal::BigInt(value) => self.out.push_str(&value.to_string()),
            Literal::Float(value) => self.float(*value),
            Literal::Str(value) => {
                self.out.push('"');
                self.out.push_str(value);
                self.out.push('"');
            }
        }
    }

    fn float(&mut self, value: f64) {
        if value.is_finite() {
            let mut text = value.to_string();
            if !text.contains('.') {
                text.push_str(".0");
            }
            self.out.push_str(&text);
        } else if value.is_infinite() {
            if value.is_sign_negative() {
                self.out.push('-');
            }
            self.out.push('1');
            for _ in 0..400 {
                self.out.push('0');
            }
            self.out.push_str(".0");
        } else {
            self.out.push_str("0.0");
        }
    }
}

/// The parser can use a postfix or ternary expression as the left operand of
/// `:` without parentheses, even though those nodes have lower precedence.
fn binary_lhs_parent(op: BinaryOp, lhs: &Expr) -> Precedence {
    if op == BinaryOp::Rational
        && matches!(
            lhs.kind,
            ExprKind::Property { .. }
                | ExprKind::Subscript { .. }
                | ExprKind::Call { .. }
                | ExprKind::Super { .. }
                | ExprKind::Ternary { .. }
        )
    {
        Precedence::None
    } else {
        precedence(&lhs.kind)
    }
}

fn precedence(kind: &ExprKind) -> Precedence {
    match kind {
        ExprKind::Assign { .. } | ExprKind::AugAssign { .. } => Precedence::Assignment,
        ExprKind::Ternary { .. } => Precedence::Ternary,
        ExprKind::Logical { op, .. } => match op {
            BoolOperator::Or => Precedence::Or,
            BoolOperator::And => Precedence::And,
        },
        ExprKind::Binary { op, .. } => binary_precedence(*op),
        ExprKind::Unary { .. } => Precedence::Unary,
        ExprKind::Property { .. } | ExprKind::Subscript { .. } | ExprKind::Call { .. } => {
            Precedence::Call
        }
        ExprKind::Super { .. } => Precedence::Call,
        ExprKind::Tuple { .. } => Precedence::Primary,
        ExprKind::Literal(_)
        | ExprKind::Variable { .. }
        | ExprKind::Grouping { .. }
        | ExprKind::ListLit { .. }
        | ExprKind::SetLit { .. }
        | ExprKind::DictLit { .. }
        | ExprKind::Fstring { .. }
        | ExprKind::Tstring(_)
        | ExprKind::Lambda(_)
        | ExprKind::Yield { .. } => Precedence::Primary,
    }
}

const fn binary_precedence(op: BinaryOp) -> Precedence {
    match op {
        BinaryOp::In => Precedence::In,
        BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::Is => Precedence::Equality,
        BinaryOp::Greater | BinaryOp::GreaterEqual | BinaryOp::Less | BinaryOp::LessEqual => {
            Precedence::Comparison
        }
        BinaryOp::BitOr => Precedence::BitOr,
        BinaryOp::BitXor => Precedence::BitXor,
        BinaryOp::BitAnd => Precedence::BitAnd,
        BinaryOp::Add | BinaryOp::Sub => Precedence::Term,
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::FloorDiv | BinaryOp::Mod => Precedence::Factor,
        BinaryOp::RangeInclusive | BinaryOp::RangeExclusive => Precedence::Range,
        BinaryOp::Exp => Precedence::Exponent,
        BinaryOp::Rational => Precedence::Rational,
    }
}

const fn binary_operator(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::FloorDiv => "//",
        BinaryOp::Mod => "%",
        BinaryOp::Exp => "**",
        BinaryOp::BitAnd => "&",
        BinaryOp::BitOr => "|",
        BinaryOp::BitXor => "^",
        BinaryOp::Equal => "==",
        BinaryOp::NotEqual => "!=",
        BinaryOp::Greater => ">",
        BinaryOp::GreaterEqual => ">=",
        BinaryOp::Less => "<",
        BinaryOp::LessEqual => "<=",
        BinaryOp::In => "in",
        BinaryOp::Is => "is",
        BinaryOp::RangeInclusive => "..=",
        BinaryOp::RangeExclusive => "..<",
        BinaryOp::Rational => ":",
    }
}

#[cfg(test)]
mod tests {
    use super::{FormatOptions, Formatter, Precedence, format_source, format_source_with_default};
    use crate::compiler::ast::{Arg, Expr, ExprContext, ExprKind, Literal};
    use crate::types::Location;

    fn assert_formatting_with(source: &str, expected: &str, options: FormatOptions) {
        let formatted = format_source(source, options).expect("source should parse");
        assert_eq!(formatted, expected);
    }

    fn assert_formatting(source: &str, expected: &str) {
        let formatted = format_source_with_default(source).expect("source should parse");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn formats_representative_constructs() {
        let source = r#"# leading comment
import "math";
import "helper" as h;
from "mod" import alpha, beta;

@decorator
@wrap(1, "value")
fun example(a, b=1, *rest) {
    var numbers = [1, 2, 3];
    const tags = {1, 2, 3};
    var mapping = {"one": 1, "two": [2, 3]};
    var empty = ();
    var pair = (1, "two");
    var text = f"hello ${a + b}!";
    var template = t"hello ${a}!";
    var double = -> (n) n * 2;
    var block = -> (n) { return n + 1; };
    numbers[0] += 1;
    numbers[1] *= 2;
    print(mapping?.missing?["x"]);
    if (a > b and !false or a in numbers) {
        print(text);
    } else {
        unless (a == b) {
            print(template);
        }
    }
    while 'outer (a < 10) {
        a = a + 1;
        if (a == 5) {
            continue 'outer;
        }
        break;
    }
    until (a >= 10) {
        a += 1;
    }
    for 'i (var i = 0; i < 3; i += 1) {
        print(i);
    }
    foreach 'item (const item in numbers) {
        print(item);
    }
    switch (a) {
        case 1:
            print("one");
        case 2, 3:
            print("few");
        default:
            throw ValueError("bad");
    }
    try {
        risky();
    } catch (ValueError, TypeError) as error {
        print(error);
    } else {
        print("ok");
    }
    return double(1) ? 1 : 2;
}

gen counter(limit) {
    var i = 0;
    while (i < limit) {
        yield i;
        i += 1;
    }
}

@class_decorator
class Child < Parent {
    var count: Int = 0;
    var label;
    __init__(name) {
        this.name = name;
    }
    method(value) {
        return super.method(value) + this.count;
    }
}
"#;
        let expected = r#"import "math";
import "helper" as h;
from "mod" import (alpha, beta);
@decorator
@wrap(1, "value")
fun example(a, b=1, *rest) {
    var numbers = [1, 2, 3];
    const tags = {1, 2, 3};
    var mapping = {"one": 1, "two": [2, 3]};
    var empty = ();
    var pair = (1, "two");
    var text = f"hello ${a + b}!";
    var template = t"hello ${a}!";
    var double = -> (n) n * 2;
    var block = -> (n) {
        return n + 1;
    };
    numbers[0] += 1;
    numbers[1] *= 2;
    print(mapping?.missing?["x"]);
    if (a > b and !false or a in numbers) {
        print(text);
    } else {
        unless (a == b) {
            print(template);
        }
    }
    while 'outer (a < 10) {
        a = a + 1;
        if (a == 5) {
            continue 'outer;
        }
        break;
    }
    until (a >= 10) {
        a += 1;
    }
    for 'i (var i = 0; i < 3; i += 1) {
        print(i);
    }
    foreach 'item (const item in numbers) {
        print(item);
    }
    switch (a) {
        case 1:
            print("one");
        case 2, 3:
            print("few");
        default:
            throw ValueError("bad");
    }
    try {
        risky();
    } catch (ValueError, TypeError) as error {
        print(error);
    } else {
        print("ok");
    }
    return double(1) ? 1 : 2;
}
gen counter(limit) {
    var i = 0;
    while (i < limit) {
        yield i;
        i += 1;
    }
}
@class_decorator
class Child < Parent {
    var count: Int = 0;
    var label;
    __init__(name) {
        this.name = name;
    }
    method(value) {
        return super.method(value) + this.count;
    }
}
"#;
        assert_formatting(source, expected);
    }

    #[test]
    fn formats_expressions_with_minimal_parentheses() {
        let source = "var a = 1 + 2 * 3;
var b = (1 + 2) * 3;
var c = 2 ** 3 ** 2;
var d = a - (b - c);
var e = a and b or c;
var f = a or b and c;
var g = a ? b : c ? d : e;
var h = (a ? b : c) ? d : e;
var i = -a ** b;
var j = !(a == b);
var k = obj.field[0](1, 2);
var l = -1.0;
var m = 1..=3;
var n = 1:2;
";
        let expected = "var a = 1 + 2 * 3;
var b = (1 + 2) * 3;
var c = 2 ** 3 ** 2;
var d = a - (b - c);
var e = a and b or c;
var f = a or b and c;
var g = a ? b : c ? d : e;
var h = (a ? b : c) ? d : e;
var i = -a ** b;
var j = !(a == b);
var k = obj.field[0](1, 2);
var l = -1.0;
var m = 1..=3;
var n = 1:2;
";
        assert_formatting(source, expected);
    }

    #[test]
    fn formats_with_configured_indent_width() {
        let source = "fun f() {
var x = 1;
var g = -> () {
return x;
};
}
";
        let expected = "fun f() {
  var x = 1;
  var g = -> () {
    return x;
  };
}
";
        assert_formatting_with(source, expected, FormatOptions { indent_width: 2 });
    }

    #[test]
    fn formats_literals() {
        let source = r#"var nil_value = nil;
var yes = true;
var no = false;
var stop = StopIteration;
var big = 123456789012345678901234567890;
var float = 1.0;
var text = "hi";
"#;
        assert_formatting(source, source);
    }

    #[test]
    fn preserves_condition_parentheses_from_the_source() {
        assert_formatting(
            "if a {\n}\nwhile a {\n}\nswitch a {\n}\n",
            "if a {}\nwhile a {}\nswitch a {}\n",
        );
        assert_formatting(
            "if (a) {\n}\nwhile (a) {\n}\nswitch (a) {\n}\n",
            "if (a) {}\nwhile (a) {}\nswitch (a) {}\n",
        );
    }

    #[test]
    fn preserves_tuple_parentheses_and_single_element_comma() {
        assert_formatting(
            "fun f() {\nreturn a, b,;\n}\nvar one = (1,);\nvar empty = ();\n",
            "fun f() {\n    return a, b;\n}\nvar one = (1,);\nvar empty = ();\n",
        );
    }

    #[test]
    fn preserves_adjacent_tstring_interpolations() {
        assert_formatting("var t = t\"a${x}${y}b\";\n", "var t = t\"a${x}${y}b\";\n");
        assert_formatting("var t = t\"${x}${y}\";\n", "var t = t\"${x}${y}\";\n");
    }

    #[test]
    fn preserves_rational_left_operand_shapes() {
        let source = "var a = p.q:r;
var b = f():r;
var c = p[q]:r;
var d = p ? q : r : s;
var e = (a + b):c;
";
        assert_formatting(
            source,
            "var a = p.q:r;
var b = f():r;
var c = p[q]:r;
var d = p ? q : r:s;
var e = (a + b):c;
",
        );
    }

    #[test]
    fn keeps_bare_yield_in_expression_positions() {
        let source = "gen g() {\n    var xs = [yield];\n    var pair = (yield, 1);\n    var sum = a + yield;\n    var indexed = a[yield];\n    var negative = -yield a;\n    var ternary = cond ? yield : b;\n}\n";
        assert_formatting(source, source);
    }

    #[test]
    fn preserves_empty_tuple_single_values() {
        assert_formatting(
            "var a = f(());\nvar b = [()];\nvar c = ((), 1);\n",
            "var a = f(());\nvar b = [()];\nvar c = ((), 1);\n",
        );
    }

    #[test]
    fn preserves_single_catch_matcher_grouping() {
        assert_formatting(
            "try {} catch ((ValueError)) {}\ntry {} catch (()) {}\ntry {} catch (a, b) {}\n",
            "try {} catch ((ValueError)) {}\ntry {} catch (()) {}\ntry {} catch (a, b) {}\n",
        );
    }

    #[test]
    fn preserves_tstring_interpolation_source() {
        assert_formatting(
            "var template = t\"hello ${ a + b }\";\n",
            "var template = t\"hello ${a + b }\";\n",
        );
    }

    #[test]
    fn parenthesizes_synthetic_tuples_in_single_value_positions() {
        fn int(value: i64) -> Expr {
            Expr {
                kind: ExprKind::Literal(Literal::Int(value)),
                location: Location::default(),
            }
        }

        fn tuple(a: i64, b: i64) -> Expr {
            Expr {
                kind: ExprKind::Tuple {
                    elements: vec![int(a), int(b)],
                },
                location: Location::default(),
            }
        }

        let call = Expr {
            kind: ExprKind::Call {
                callee: Box::new(Expr {
                    kind: ExprKind::Variable {
                        name: String::from("f"),
                        ctx: ExprContext::Load,
                    },
                    location: Location::default(),
                }),
                args: vec![Arg {
                    value: tuple(1, 2),
                    is_spread: false,
                }],
                args_location: Location::default(),
            },
            location: Location::default(),
        };
        let list = Expr {
            kind: ExprKind::ListLit {
                items: vec![tuple(3, 4)],
            },
            location: Location::default(),
        };
        let nested = Expr {
            kind: ExprKind::Tuple {
                elements: vec![int(5), tuple(6, 7)],
            },
            location: Location::default(),
        };

        let mut formatter = Formatter::new(FormatOptions::default());
        formatter.expr(&call, Precedence::None, 0);
        assert_eq!(formatter.out, "f((1, 2))");

        let mut formatter = Formatter::new(FormatOptions::default());
        formatter.expr(&list, Precedence::None, 0);
        assert_eq!(formatter.out, "[(3, 4)]");

        let mut formatter = Formatter::new(FormatOptions::default());
        formatter.expr(&nested, Precedence::None, 0);
        assert_eq!(formatter.out, "5, (6, 7)");
    }

    #[test]
    fn canonicalizes_import_parentheses() {
        assert_formatting(
            "from \"a\" import x;\nfrom \"b\" import (y);\nfrom \"c\" import (a, b,);\n",
            "from \"a\" import x;\nfrom \"b\" import y;\nfrom \"c\" import (a, b);\n",
        );
    }

    #[test]
    fn canonicalizes_catch_parentheses() {
        assert_formatting(
            "try {\n} catch ValueError {\n}\ntry {\n} catch (ValueError) {\n}\n",
            "try {} catch ValueError {}\ntry {} catch ValueError {}\n",
        );
        assert_formatting(
            "try {\n} catch (ValueError, TypeError) {\n}\n",
            "try {} catch (ValueError, TypeError) {}\n",
        );
    }

    #[test]
    fn discards_comments() {
        let options = FormatOptions::default();
        let formatted =
            format_source("# before\nvar x = 1; # after\n", options).expect("source should parse");
        assert_eq!(formatted, "var x = 1;\n");
    }

    #[test]
    fn formats_an_empty_program() {
        let options = FormatOptions::default();
        assert_eq!(format_source("", options).unwrap(), "");
        assert_eq!(format_source("# only a comment\n", options).unwrap(), "");
    }
}
