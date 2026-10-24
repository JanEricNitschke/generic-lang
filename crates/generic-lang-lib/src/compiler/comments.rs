//! Comment trivia: capture, placement, and formatter support.
//!
//! # Lifecycle
//!
//! 1. The scanner skips `#` comments but records their raw text and location.
//! 2. The parser builds the semantic AST and gives statements, class members,
//!    switch cases, and comment-bearing bodies their source locations.
//! 3. After parsing, [`attach`] walks those locations and matches each comment
//!    to the nearest following AST slot.
//! 4. The formatter reads the resulting [`CommentMap`]; codegen only receives
//!    the semantic AST.
//!
//! Comments never influence parsing, lookahead, or error recovery.
//!
//! # Placement
//!
//! [`Collector`] walks the AST in source order and records one insertion
//! anchor before each item of every comment-bearing list. A list that owns
//! trailing comments also records an anchor after its last item, using the end
//! of the enclosing node's location. [`attach`] sorts those anchors and gives
//! each comment to the first anchor after it. Comments after the final anchor
//! are kept as trailing program comments.
//!
//! The anchor also carries the end line of the previous item. That line is the
//! baseline for `inline`: a comment on the same line as the previous item is
//! printed after it instead of on its own line.

use std::collections::BTreeMap;

use super::ast::{
    ClassMember, ClassMemberKind, Expr, ExprKind, LambdaBody, LambdaLit, Located, Stmt, StmtKind,
    SwitchCase, TryStmt,
};
use crate::scanner::ScannedComment;
use crate::types::Location;

/// A source position: line and column.
pub(super) type Position = (usize, usize);

pub(super) fn start_position(location: Location) -> Position {
    (location.start_line.0, location.start_column.0)
}

pub(super) fn end_position(location: Location) -> Position {
    (location.end_line.0, location.end_column.0)
}

/// One comment ready for the formatter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Comment {
    /// Text including the leading `#`, with trailing whitespace removed.
    pub(super) text: String,
    /// Whether it followed the previous item or comment on the same line.
    pub(super) inline: bool,
}

/// Comments keyed by the position of their insertion slot.
///
/// Comments are also stored in source order so formatter round-trips can
/// compare them independently of the positions, which change after formatting.
#[derive(Debug, Clone, Default)]
pub(crate) struct CommentMap {
    /// Comments per insertion position.
    slots: BTreeMap<Position, Vec<Comment>>,
    /// Comments after the last AST item.
    trailing: Vec<Comment>,
}

impl CommentMap {
    pub(super) fn get(&self, position: Position) -> &[Comment] {
        self.slots.get(&position).map_or(&[], Vec::as_slice)
    }

    pub(super) fn trailing(&self) -> &[Comment] {
        &self.trailing
    }

    pub(super) fn has_comments(&self, position: Position) -> bool {
        self.slots
            .get(&position)
            .is_some_and(|comments| !comments.is_empty())
    }

    /// Compare comments in source order. Positions change after formatting,
    /// so only the comments themselves are comparable.
    pub(super) fn same_comments(&self, other: &Self) -> bool {
        self.in_source_order().eq(other.in_source_order())
    }

    fn in_source_order(&self) -> impl Iterator<Item = &Comment> {
        self.slots.values().flatten().chain(&self.trailing)
    }
}

/// Walks an AST and collects the positions where comments may be inserted.
///
/// The walk itself is source-ordered. Each call to [`Self::list`] appends one
/// anchor before every item and, when the list owns trailing trivia, one after
/// the last item. The collector does not place comments; it only describes the
/// available slots for [`attach`].
#[derive(Default)]
struct Collector {
    anchors: Vec<SlotAnchor>,
    /// End line of the last item seen anywhere in the AST. Used for comments
    /// after the final declaration.
    last_end_line: Option<usize>,
}

/// One insertion slot for comments.
struct SlotAnchor {
    /// Where the next item starts, or the end of the list for a trailing slot.
    position: Position,
    /// End line of the previous item, or the initial baseline for the first
    /// slot. Used to decide whether a comment is inline.
    previous_end_line: Option<usize>,
}

impl Collector {
    /// Record the insertion slots of one list.
    ///
    /// `end` is the position just after the list's final item, if the list
    /// owns trailing comments. `initial_end_line` is only set for switch case
    /// bodies: it is the colon line, so a comment immediately after
    /// `case x:` is treated as inline with the label.
    fn list<T: Located>(
        &mut self,
        items: &[T],
        end: Option<Position>,
        initial_end_line: Option<usize>,
    ) {
        let mut previous_end_line = initial_end_line;
        for item in items {
            let location = item.location();
            self.anchors.push(SlotAnchor {
                position: start_position(location),
                previous_end_line,
            });
            previous_end_line = Some(location.end_line.0);
            self.last_end_line = Some(location.end_line.0);
        }
        if let Some(end) = end {
            self.anchors.push(SlotAnchor {
                position: end,
                previous_end_line,
            });
        }
    }

    fn declarations(&mut self, declarations: &[Stmt]) {
        self.list(declarations, None, None);
        for statement in declarations {
            self.statement(statement);
        }
    }

    #[allow(clippy::too_many_lines)]
    fn statement(&mut self, statement: &Stmt) {
        match &statement.kind {
            StmtKind::Expression { expr } | StmtKind::Throw { expr, .. } => self.expr(expr),
            StmtKind::VarDecl {
                initializer: Some(initializer),
                ..
            } => self.expr(initializer),
            StmtKind::VarDecl { .. }
            | StmtKind::Break { .. }
            | StmtKind::Continue { .. }
            | StmtKind::Import { .. }
            | StmtKind::ImportFrom { .. } => {}
            StmtKind::Block { stmts } => {
                self.list(stmts, Some(end_position(statement.location)), None);
                for statement in stmts {
                    self.statement(statement);
                }
            }
            StmtKind::FunctionDecl {
                fn_lit, decorators, ..
            } => {
                for decorator in decorators {
                    self.expr(decorator);
                }
                self.function_def(fn_lit, end_position(statement.location));
            }
            StmtKind::Class(class) => {
                if let Some((superclass, _)) = &class.superclass {
                    self.expr(superclass);
                }
                for decorator in &class.decorators {
                    self.expr(decorator);
                }
                self.list(&class.members, Some(end_position(statement.location)), None);
                for member in &class.members {
                    self.class_member(member);
                }
            }
            StmtKind::Conditional {
                condition,
                then_block,
                else_block,
                then_close_location,
                else_close_location,
                ..
            } => {
                self.expr(condition);
                self.list(then_block, Some(end_position(*then_close_location)), None);
                for statement in then_block {
                    self.statement(statement);
                }
                if let Some(else_block) = else_block {
                    self.list(else_block, Some(end_position(*else_close_location)), None);
                    for statement in else_block {
                        self.statement(statement);
                    }
                }
            }
            StmtKind::Loop {
                condition, body, ..
            } => {
                self.expr(condition);
                self.list(body, Some(end_position(statement.location)), None);
                for statement in body {
                    self.statement(statement);
                }
            }
            StmtKind::For {
                init,
                condition,
                increment,
                body,
                ..
            } => {
                if let Some(init) = init {
                    self.statement_kind(init);
                }
                if let Some(condition) = condition {
                    self.expr(condition);
                }
                if let Some(increment) = increment {
                    self.expr(increment);
                }
                self.list(body, Some(end_position(statement.location)), None);
                for statement in body {
                    self.statement(statement);
                }
            }
            StmtKind::Foreach { iterable, body, .. } => {
                self.expr(iterable);
                self.list(body, Some(end_position(statement.location)), None);
                for statement in body {
                    self.statement(statement);
                }
            }
            StmtKind::Return { value } => {
                if let Some(value) = value {
                    self.expr(value);
                }
            }
            StmtKind::Try(try_stmt) => self.try_stmt(try_stmt),
            StmtKind::Switch { subject, cases, .. } => {
                self.expr(subject);
                self.list(cases, Some(end_position(statement.location)), None);
                for case in cases {
                    self.switch_case(case);
                }
            }
        }
    }

    fn statement_kind(&mut self, statement: &StmtKind) {
        match statement {
            StmtKind::Expression { expr } => self.expr(expr),
            StmtKind::VarDecl {
                initializer: Some(initializer),
                ..
            } => self.expr(initializer),
            StmtKind::For {
                init,
                condition,
                increment,
                ..
            } => {
                if let Some(init) = init {
                    self.statement_kind(init);
                }
                if let Some(condition) = condition {
                    self.expr(condition);
                }
                if let Some(increment) = increment {
                    self.expr(increment);
                }
            }
            _ => {}
        }
    }

    fn function_def(&mut self, function: &super::ast::FunctionDef, close: Position) {
        self.list(&function.body, Some(close), None);
        for statement in &function.body {
            self.statement(statement);
        }
    }

    fn lambda(&mut self, lambda: &LambdaLit, close: Position) {
        match &lambda.body {
            LambdaBody::Block(statements) => {
                self.list(statements, Some(close), None);
                for statement in statements {
                    self.statement(statement);
                }
            }
            LambdaBody::Expression(expr) => self.expr(expr),
        }
    }

    fn class_member(&mut self, member: &ClassMember) {
        match &member.kind {
            ClassMemberKind::Variable(variable) => {
                if let Some(annotation) = &variable.annotation {
                    self.expr(annotation);
                }
                if let Some(default) = &variable.default {
                    self.expr(default);
                }
            }
            ClassMemberKind::Method(method) => {
                self.function_def(&method.fn_lit, end_position(member.location));
            }
        }
    }

    fn try_stmt(&mut self, try_stmt: &TryStmt) {
        self.list(
            &try_stmt.body,
            Some(end_position(try_stmt.body_location)),
            None,
        );
        for statement in &try_stmt.body {
            self.statement(statement);
        }
        for catch in &try_stmt.catches {
            for matcher in &catch.matchers {
                self.expr(matcher);
            }
            self.list(&catch.body, Some(end_position(catch.body_location)), None);
            for statement in &catch.body {
                self.statement(statement);
            }
        }
        if let (Some(else_block), Some(else_location)) =
            (&try_stmt.else_block, try_stmt.else_location)
        {
            self.list(else_block, Some(end_position(else_location)), None);
            for statement in else_block {
                self.statement(statement);
            }
        }
    }

    fn switch_case(&mut self, case: &SwitchCase) {
        if let Some(value) = &case.value {
            self.expr(value);
        }
        self.list(&case.body, None, Some(case.body_start_location.end_line.0));
        for statement in &case.body {
            self.statement(statement);
        }
    }

    fn expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Literal(_) | ExprKind::Variable { .. } | ExprKind::Super { .. } => {}
            ExprKind::Property { object, .. } => self.expr(object),
            ExprKind::Subscript { object, index, .. } => {
                self.expr(object);
                self.expr(index);
            }
            ExprKind::Call { callee, args, .. } => {
                self.expr(callee);
                for arg in args {
                    self.expr(&arg.value);
                }
            }
            ExprKind::Unary { operand, .. } => self.expr(operand),
            ExprKind::Binary { lhs, rhs, .. } | ExprKind::Logical { lhs, rhs, .. } => {
                self.expr(lhs);
                self.expr(rhs);
            }
            ExprKind::Ternary {
                condition,
                then,
                else_,
                ..
            } => {
                self.expr(condition);
                self.expr(then);
                self.expr(else_);
            }
            ExprKind::Grouping { inner } => self.expr(inner),
            ExprKind::Tuple { elements }
            | ExprKind::ListLit { items: elements }
            | ExprKind::SetLit { items: elements } => {
                for element in elements {
                    self.expr(element);
                }
            }
            ExprKind::DictLit { entries } => {
                for (key, value) in entries {
                    self.expr(key);
                    self.expr(value);
                }
            }
            ExprKind::Fstring { parts } => {
                for part in parts {
                    if let super::ast::FstringPart::Interp(expr) = part {
                        self.expr(expr);
                    }
                }
            }
            ExprKind::Tstring(tstring) => {
                for interpolation in &tstring.interpolations {
                    self.expr(&interpolation.value);
                }
            }
            ExprKind::Lambda(lambda) => self.lambda(lambda, end_position(expr.location)),
            ExprKind::Yield { operand } => {
                if let Some(operand) = operand {
                    self.expr(operand);
                }
            }
            ExprKind::Assign { target, value, .. } | ExprKind::AugAssign { target, value, .. } => {
                self.expr(target);
                self.expr(value);
            }
        }
    }
}

/// Place every comment at the nearest following AST slot.
///
/// Anchors are sorted by source position. A comment always belongs to the
/// first anchor after it; comments after the final anchor become trailing
/// program comments. Within one slot, only the first comment can be inline:
/// later comments compare against the previous comment's end line.
pub(super) fn attach(declarations: &[Stmt], comments: Vec<ScannedComment>) -> CommentMap {
    let mut collector = Collector::default();
    collector.declarations(declarations);
    collector.anchors.sort_by_key(|anchor| anchor.position);

    let mut map = CommentMap::default();
    let mut current_slot = None;
    let mut current_line = None;
    let mut trailing_line = collector.last_end_line;
    for comment in comments {
        let index = collector
            .anchors
            .partition_point(|anchor| anchor.position <= start_position(comment.location));
        let anchor = collector.anchors.get(index);
        let inline = if let Some(anchor) = anchor {
            let previous_end_line = if current_slot == Some(anchor.position) {
                current_line
            } else {
                current_slot = Some(anchor.position);
                anchor.previous_end_line
            };
            current_line = Some(comment.location.end_line.0);
            previous_end_line == Some(comment.location.start_line.0)
        } else {
            let inline = trailing_line == Some(comment.location.start_line.0);
            trailing_line = Some(comment.location.end_line.0);
            inline
        };
        let comment = Comment {
            text: comment.text.trim_end().to_string(),
            inline,
        };
        if let Some(anchor) = anchor {
            map.slots.entry(anchor.position).or_default().push(comment);
        } else {
            map.trailing.push(comment);
        }
    }
    map
}
