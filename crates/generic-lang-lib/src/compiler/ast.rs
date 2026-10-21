//! The syntax tree's node types.
//!
//! A node's `location` spans its whole extent (first .. last token).
//! Extra positions are kept only where a user-visible error can point
//! at them; they always come last in a node.

use crate::types::{ConditionType, Location, LoopType, Mutability};
use generic_lang_derive::LocationLessEq;
use num_bigint::BigInt;

/// A parsed program: the top-level declarations.
#[derive(Debug, LocationLessEq)]
pub(crate) struct Program {
    pub declarations: Vec<StmtKind>,
}

/// Injected source (`eval`/`exec`): a plain function whose leading local
/// slots are pre-declared (filled by the caller like arguments) and whose
/// kind decides the body shape.
#[derive(Debug)]
pub(crate) struct InjectedProgram {
    /// The caller-provided local names, in slot order.
    pub locals: Vec<String>,
    pub body: InjectedBody,
}

#[derive(Debug)]
pub(crate) enum InjectedBody {
    /// `eval`: a single expression the function returns raw.
    Eval(Box<Expr>),
    /// `exec`: plain declarations, implicit `return nil`.
    Exec(Vec<StmtKind>),
}

/// The kinds a top-level function declaration can have.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FunctionKind {
    /// A `fun` declaration.
    Function,
    /// A `gen` declaration.
    Generator,
}

/// The kinds a class member can have.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MethodKind {
    /// An `__init__` method.
    Initializer,
    /// Any other method.
    Method,
}

/// Characterizes the types of functions.
///
/// - Function is a `fun` declaration: it binds its name in the enclosing
///   scope, which stays uninitialized through the parameter list (the
///   defaults run before the closure exists) and initializes ahead of
///   the body so it can self-reference for recursion.
/// - Generator is a `gen` declaration: like Function, plus the
///   generator return protocol.
/// - Lambda is a `->` literal: the only form whose body may be a bare
///   expression, and it binds no name.
/// - Initializer is a constructor and is special because it returns `this` on exit without
///   a return statement or on a bare `return`. Returns with values are not allowed.
/// - Method is a method on a class. It is special because the local slot 0 is always `this`.
/// - Script is the top-level code in a file (main script or imported modules).
///   Does not allow `return` statements.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FunKind {
    Function,
    Generator,
    Lambda,
    Initializer,
    Method,
    Script,
}

impl FunKind {
    /// Whether a function of this kind binds its name in the enclosing
    /// scope ahead of its own body (self-recursion).
    #[must_use]
    pub(crate) const fn initializes_enclosing_name(self) -> bool {
        matches!(self, Self::Function | Self::Generator)
    }
}

impl From<FunctionKind> for FunKind {
    fn from(kind: FunctionKind) -> Self {
        match kind {
            FunctionKind::Function => Self::Function,
            FunctionKind::Generator => Self::Generator,
        }
    }
}

impl From<MethodKind> for FunKind {
    fn from(kind: MethodKind) -> Self {
        match kind {
            MethodKind::Initializer => Self::Initializer,
            MethodKind::Method => Self::Method,
        }
    }
}

/// A single call argument. `` *expr `` spreads: the iterable's elements
/// become the arguments.
#[derive(Debug, LocationLessEq)]
pub(crate) struct Arg {
    pub value: Expr,
    pub is_spread: bool,
}

/// The operators of two-operand expressions: everything that sits
/// between its operands, from `+` to `..=` to `:`. Compound assignment
/// (`+=` and friends) reuses the arithmetic subset of this enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Mod,
    Exp,
    BitAnd,
    BitOr,
    BitXor,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    In,
    Is,
    RangeInclusive,
    RangeExclusive,
    Rational,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnaryOp {
    Neg,
    Not,
}

/// A literal value as parsed: nil, booleans, the stop-iteration
/// sentinel, numbers, and strings.
#[derive(Debug, LocationLessEq)]
pub(crate) enum Literal {
    Nil,
    True,
    False,
    StopIteration,
    Int(i64),
    BigInt(BigInt),
    Float(f64),
    Str(String),
}

/// How a property or subscript access reacts to a nil receiver.
///
/// `NullSafe` marks the `?.` / `?[` links of an optional chain: if the
/// value arriving at the link is nil, the rest of the chain is skipped
/// and the nil becomes the chain's value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AccessMode {
    /// A plain `.name` / `[index]`.
    Direct,
    /// A `?.name` / `?[index]` link inside an optional chain.
    NullSafe,
}

/// Whether an expression names something to read or something to write.
/// Only `Variable`, `Property`, and `Subscript` can hold `Store`; they
/// appear that way as the target of an assignment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ExprContext {
    Load,
    Store,
}

/// The short-circuiting boolean operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BoolOperator {
    And,
    Or,
}

/// One part of an f-string: a literal text run or an interpolated
/// expression.
#[derive(Debug, LocationLessEq)]
pub(crate) enum FstringPart {
    /// Literal text. The final part carries the closing quote stripped.
    Str(String),
    Interp(Box<Expr>),
}

/// A t-string: its static text and its interpolations, kept in two
/// parallel lists (they interleave in the source).
#[derive(Debug, LocationLessEq)]
pub(crate) struct Tstring {
    /// Static text boundaries, in order. This always contains exactly one
    /// more entry than `interpolations`: the text before each interpolation,
    /// including empty strings for adjacent interpolations, plus the trailing
    /// text. The final part carries its closing quote stripped.
    pub parts: Vec<String>,
    pub interpolations: Vec<TstringInterp>,
}

#[derive(Debug, LocationLessEq)]
pub(crate) struct TstringInterp {
    /// The interpolated expression.
    pub value: Box<Expr>,
    /// The interpolation's source text, kept in the template for
    /// display.
    pub source_text: String,
}

/// An expression.
///
/// `location` covers the expression's whole extent in the source (first
/// token .. last token).
#[derive(Debug, LocationLessEq)]
pub(crate) struct Expr {
    pub kind: ExprKind,
    pub location: Location,
}

#[derive(Debug, LocationLessEq)]
pub(crate) enum ExprKind {
    Literal(Literal),

    /// A bare identifier, like `` x ``.
    Variable {
        name: String,
        ctx: ExprContext,
    },

    /// A property read or write, like `` object.name `` or
    /// `` object?.name ``. `access_location` covers `` .name ``.
    Property {
        object: Box<Expr>,
        name: String,
        mode: AccessMode,
        ctx: ExprContext,
        access_location: Location,
    },

    /// An index access, like `` object[index] `` or `` object?[index] ``.
    /// `access_location` covers the bracketed `` [index] `` access; the
    /// expression's own location covers the whole `` object[index] ``.
    Subscript {
        object: Box<Expr>,
        index: Box<Expr>,
        mode: AccessMode,
        ctx: ExprContext,
        access_location: Location,
    },

    /// A call, like `` callee(args) ``. The callee can be any
    /// expression; a property callee names a method
    /// (`` object.name(args) ``) and a `super` callee names a
    /// superclass method (`` super.name(args) ``). `args_location`
    /// covers `` ( ... ) ``.
    Call {
        callee: Box<Expr>,
        args: Vec<Arg>,
        args_location: Location,
    },

    /// A unary operation, like `` -x ``.
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
        op_location: Location,
    },

    /// A binary operation, like `` a + b ``.
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op_location: Location,
    },

    /// Short-circuiting `and` / `or`.
    Logical {
        op: BoolOperator,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op_location: Location,
    },

    /// `` condition ? then : else ``.
    Ternary {
        condition: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
        /// The `?`: the origin of the branch jumps.
        question_location: Location,
    },

    /// ``(expr)`` - pure grouping.
    Grouping {
        inner: Box<Expr>,
    },

    /// A tuple, like `` a, b `` or the empty `` () ``.
    Tuple {
        elements: Vec<Expr>,
    },

    ListLit {
        items: Vec<Expr>,
    },
    SetLit {
        items: Vec<Expr>,
    },
    DictLit {
        entries: Vec<(Expr, Expr)>,
    },

    /// `` f"a${expr}b" `` - literal text runs and interpolated
    /// expressions, in order.
    Fstring {
        parts: Vec<FstringPart>,
    },

    /// `` t"a${expr}b" `` - see [`Tstring`].
    Tstring(Box<Tstring>),

    /// A `` -> params body `` lambda literal.
    Lambda(Box<LambdaLit>),

    /// `` super.name `` - the method lookup through the superclass.
    /// `` super.name(args) `` is a `Call` with this node as the callee.
    /// `super_location` is the `super` keyword, `access_location`
    /// covers `` .name ``; the expression's own location covers
    /// `` super..name ``.
    Super {
        method: String,
        super_location: Location,
        access_location: Location,
    },

    /// `` yield expr? ``.
    Yield {
        operand: Option<Box<Expr>>,
    },

    /// `` target = value ``. The target is a `Variable`, `Property`, or
    /// `Subscript` in `Store` context. `op_location` is the `` = ``
    /// token.
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
        op_location: Location,
    },

    /// `` target op= value `` with `op` one of the arithmetic compound
    /// operators (`+=`, `-=`, `*=`, `/=`, `^=`, `|=`, `&=`, `%=`).
    /// Same target shapes as [`ExprKind::Assign`].
    AugAssign {
        target: Box<Expr>,
        op: BinaryOp,
        value: Box<Expr>,
        op_location: Location,
    },
}

impl ExprKind {
    /// Whether the node is a link a null-safe chain can pass through
    /// (property, subscript, call): such a node entered from a non-link
    /// context starts a chain.
    #[must_use]
    pub(crate) const fn is_nullsafe_chain_link(&self) -> bool {
        matches!(
            self,
            Self::Property { .. } | Self::Subscript { .. } | Self::Call { .. }
        )
    }
}

#[derive(Debug, LocationLessEq)]
pub(crate) struct Param {
    pub name: String,
    /// Evaluated once, when the function is defined.
    pub default: Option<Box<Expr>>,
    pub location: Location,
}

/// A function's parameters: the named parameters plus the trailing
/// `*rest` parameter, if any.
#[derive(Debug, LocationLessEq)]
pub(crate) struct ParamList {
    pub params: Vec<Param>,
    pub rest: Option<Param>,
}

/// A declared function's shared shape: name, parameters, and a block
/// body. The kind is named at the declaration site: [`FunctionDecl`]
/// holds a [`FunctionKind`], [`Method`] a [`MethodKind`]. A lambda is
/// its own type ([`LambdaLit`]).
///
/// `fn_location` is the function's name token.
#[derive(Debug, LocationLessEq)]
pub(crate) struct FunctionDef {
    pub name: String,
    pub parameters: ParamList,
    pub body: Vec<StmtKind>,
    pub fn_location: Location,
}

/// A `` -> `` lambda literal: parameters plus either a block body or a
/// bare expression (an implicit `return`). Unlike a function it binds no
/// name. `fn_location` is the `` -> `` token.
#[derive(Debug, LocationLessEq)]
pub(crate) struct LambdaLit {
    pub parameters: ParamList,
    pub body: LambdaBody,
    pub fn_location: Location,
}

#[derive(Debug, LocationLessEq)]
pub(crate) enum LambdaBody {
    Block(Vec<StmtKind>),
    /// A bare expression (implicit `` return <expr> ``).
    Expression(Box<Expr>),
}

#[derive(Debug, LocationLessEq)]
pub(crate) enum StmtKind {
    /// `` expr; ``
    Expression {
        expr: Box<Expr>,
    },

    /// `` var ``/`` const `` declaration (also the initializer clause of
    /// a `for` loop).
    VarDecl {
        name: String,
        mutability: Mutability,
        initializer: Option<Box<Expr>>,
        name_location: Location,
        /// The `;`: the loop-variable size-limit error site when this
        /// declares a `for` initializer.
        semicolon_location: Location,
    },

    /// `` { ... } ``
    Block {
        stmts: Vec<Self>,
    },

    /// `` fun name(...) {...} `` / `` gen name(...) {...} ``, with its
    /// `` @decorator `` expressions in source order.
    FunctionDecl {
        name: String,
        kind: FunctionKind,
        fn_lit: FunctionDef,
        decorators: Vec<Expr>,
    },

    Class(Box<ClassDecl>),

    /// `` if (cond) { then } else { else } `` /
    /// `` unless (cond) { then } else { else } ``.
    Conditional {
        condition_type: ConditionType,
        condition: Box<Expr>,
        then_block: Vec<Self>,
        else_block: Option<Vec<Self>>,
        then_close_location: Location,
        else_close_location: Location,
        /// The `if`/`unless` keyword: the origin of the branch jumps.
        keyword_location: Location,
    },

    /// `` while (cond) { body } `` / `` until (cond) { body } ``.
    Loop {
        loop_type: LoopType,
        condition: Box<Expr>,
        body: Vec<Self>,
        label: Option<String>,
        body_close_location: Location,
        /// The `while`/`until` keyword: the origin of the loop jumps
        /// and the `continue` target.
        keyword_location: Location,
    },

    /// `` break; `` / `` continue; `` (optionally `` 'label``).
    Break {
        label: Option<String>,
    },
    Continue {
        label: Option<String>,
        /// The `continue` keyword.
        location: Location,
    },

    /// C-style `` for (init; cond; incr) { body } ``; every clause may
    /// be empty. `init` is a `var`/`const` declaration or an expression
    /// statement.
    For {
        label: Option<String>,
        init: Option<Box<Self>>,
        condition: Option<Box<Expr>>,
        increment: Option<Box<Expr>>,
        body: Vec<Self>,
        right_paren_location: Location,
        body_close_location: Location,
        /// The `for` keyword: the origin of the loop jumps and the
        /// `continue` target.
        keyword_location: Location,
    },

    /// `` foreach (var x in iterable) { body } ``.
    Foreach {
        label: Option<String>,
        mutability: Mutability,
        var_name: String,
        iterable: Box<Expr>,
        body: Vec<Self>,
        var_name_location: Location,
        in_location: Location,
        close_location: Location,
        body_close_location: Location,
        /// The `foreach` keyword: the origin of the loop jumps and the
        /// `continue` target.
        keyword_location: Location,
    },

    Return {
        value: Option<Box<Expr>>,
    },

    Throw {
        expr: Box<Expr>,
        throw_location: Location,
    },

    Try(Box<TryStmt>),

    Switch {
        subject: Box<Expr>,
        cases: Vec<SwitchCase>,
        /// The subject's position (first token .. the token after it,
        /// usually the `{`).
        subject_location: Location,
        /// The `switch` keyword: the dispatch-jump error site.
        keyword_location: Location,
    },

    Import {
        path: String,
        /// `` import "path" as name ``.
        alias: Option<String>,
        path_location: Location,
    },

    ImportFrom {
        path: String,
        /// The imported names, each with its own span.
        names: Vec<(String, Location)>,
        /// The quoted path: the path-constant error site.
        path_location: Location,
        /// `` from ... import <last name> ``: the runtime pause/blame
        /// span of the whole statement.
        statement_location: Location,
    },
}

/// A class declaration.
///
/// `superclass` holds the superclass expression (a plain identifier
/// read) and the `<` token. `members` keeps class variables and methods
/// in source order: codegen evaluates their expressions in that order.
#[derive(Debug, LocationLessEq)]
pub(crate) struct ClassDecl {
    pub name: String,
    pub superclass: Option<(Box<Expr>, Location)>,
    pub members: Vec<ClassMember>,
    /// The `` @decorator `` expressions in source order.
    pub decorators: Vec<Expr>,
    pub name_location: Location,
}

/// One item of a class body, in source order.
#[derive(Debug, LocationLessEq)]
pub(crate) enum ClassMember {
    Variable(ClassVar),
    Method(Method),
}

/// A `` var name (: annotation)? (= default)?; `` line inside a class
/// body. `default` distinguishes an explicit `` = nil `` from an omitted
/// default: it is `Some` of a nil literal exactly when `` = nil `` was
/// written.
#[derive(Debug, LocationLessEq)]
pub(crate) struct ClassVar {
    pub name: String,
    pub annotation: Option<Box<Expr>>,
    pub default: Option<Box<Expr>>,
    pub name_location: Location,
    pub semicolon_location: Location,
}

/// A class member: name plus the function literal.
#[derive(Debug, LocationLessEq)]
pub(crate) struct Method {
    pub name: String,
    pub kind: MethodKind,
    pub fn_lit: FunctionDef,
    pub close_location: Location,
}

/// `` try``/``catch``/``else``. `try_location` is the `try` keyword:
/// the jump-over size-limit sites of the emitted jump ladder.
#[derive(Debug, LocationLessEq)]
pub(crate) struct TryStmt {
    pub body: Vec<StmtKind>,
    pub catches: Vec<CatchClause>,
    pub else_block: Option<Vec<StmtKind>>,
    pub try_location: Location,
}

/// One `` catch `` clause. `matchers` are the exception-class filters:
/// one entry for `` catch (X) `` / `` catch X ``, several for
/// `` catch (X, Y) `` (the parser always requires at least one). The
/// pending exception is compared against each matcher in order.
#[derive(Debug, LocationLessEq)]
pub(crate) struct CatchClause {
    pub matchers: Vec<Expr>,
    /// `` Some((name, token)) `` for `` catch ... as name ``.
    pub binding: Option<(String, Location)>,
    pub body: Vec<StmtKind>,
}

#[derive(Debug, LocationLessEq)]
pub(crate) struct SwitchCase {
    /// `None` for the `default:` branch.
    pub value: Option<Box<Expr>>,
    pub body: Vec<StmtKind>,
}
