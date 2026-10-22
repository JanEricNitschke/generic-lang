//! Parser for expressions while repsecting operator precedence
//!
//! Uses Vaughan Pratt's "top-down operator precedence parsing".

use super::ast::{
    AccessMode, BinaryOp, BoolOperator, Expr, ExprContext, ExprKind, FstringPart, FunKind, Literal,
    Tstring, TstringInterp, UnaryOp,
};
use super::{back::Codegen, front::Parser};
use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::{
    chunk::OpCode,
    scanner::TokenKind as TK,
    types::{Location, NumberEncoding, OpcodeLocation},
    value::utils::{ParsedInteger, parse_float_compiler, parse_integer_compiler},
};

// The precedence of the different operators in the language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub(crate) enum Precedence {
    None,
    Assignment, // =
    Tuple,      // ,
    Ternary,    // ?:
    Or,         // or
    And,        // and
    In,         // in
    Equality,   // == !=
    Comparison, // < > <= >=
    BitOr,      // |
    BitXor,     // ^
    BitAnd,     // &
    Term,       // + -
    Factor,     // * / // %
    Range,      // ..< ..=
    Unary,      // ! -
    Exponent,   // **
    Call,       // . () []
    Rational,   // :
    Primary,
}

impl Precedence {
    pub(crate) const fn non_assigning() -> Self {
        Self::Tuple
    }
}

#[derive(Debug)]
enum ParseResult {
    /// A prefix rule was found and executed.
    Success(Expr),
    /// No prefix rule was found for the current token.
    NoPrefix,
}

// Typedef for the functions that parse the different types of expressions
type PrefixFn = fn(&mut Parser<'_>, bool, &[TK]) -> Expr;
type InfixFn = fn(&mut Parser<'_>, bool, &[TK], Expr) -> Expr;

// This specifies the functions that handle the parsing of an operator as prefix or infix,
// as well as its precedence. There will be one such struct for each Token.
#[derive(Clone, Copy)]
struct Rule {
    prefix: Option<PrefixFn>,
    infix: Option<InfixFn>,
    precedence: Precedence,
}

/// Rules used for parsing expressions by precedence climbing.
/// The parse table: one entry per token kind. Built once in a `const`
/// now - the rule functions are the free shims above, so the table no
/// longer needs the per-instance build the lifetimed old one forced.
macro_rules! make_rules {
    (@parse_fn None) => { None };
    (@parse_fn $prefix:ident) => { Some($prefix) };

    ($($token:ident = [$prefix:ident, $infix:ident, $precedence:ident]),* $(,)?) => {{
        let mut rules = [Rule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        }; 94];
        $(
            rules[TK::$token as usize] = Rule {
                prefix: make_rules!(@parse_fn $prefix),
                infix: make_rules!(@parse_fn $infix),
                precedence: Precedence::$precedence,
            };
        )*
        rules
    }};
}

#[rustfmt::skip]
const fn make_rules() -> [Rule; 94] {
    make_rules!(
        LeftParen                = [grouping,        call,               Call       ],
        RightParen               = [None,            None,               None       ],
        LeftBrace                = [hash_collection, None,               None       ],
        RightBrace               = [None,            None,               None       ],
        LeftBracket              = [list,            subscript,          Call       ],
        RightBracket             = [None,            None,               None       ],
        Comma                    = [None,            tuple,              Tuple      ],
        Dot                      = [None,            dot,                Call       ],
        Semicolon                = [None,            None,               None       ],
        Plus                     = [None,            binary,             Term       ],
        PlusEqual                = [None,            None,               None       ],
        Minus                    = [unary,           binary,             Term       ],
        MinusEqual               = [None,            None,               None       ],
        Star                     = [None,            binary,             Factor     ],
        StarEqual                = [None,            None,               None       ],
        StarStar                 = [None,            binary,             Exponent   ],
        Slash                    = [None,            binary,             Factor     ],
        SlashEqual               = [None,            None,               None       ],
        SlashSlash               = [None,            binary,             Factor     ],
        Percent                  = [None,            binary,             Factor     ],
        PercentEqual             = [None,            None,               None       ],
        Amper                    = [None,            binary,             BitAnd     ],
        AmperEqual               = [None,            None,               None       ],
        Pipe                     = [None,            binary,             BitOr      ],
        PipeEqual                = [None,            None,               None       ],
        Hat                      = [None,            binary,             BitXor     ],
        HatEqual                 = [None,            None,               None       ],
        DotDotLess               = [None,            binary,             Range      ],
        DotDotEqual              = [None,            binary,             Range      ],
        Colon                    = [None,            binary,             Rational   ],
        Bang                     = [unary,           None,               None       ],
        Equal                    = [None,            None,               None       ],
        QuestionMark             = [None,            ternary,            Ternary    ],
        QuestionDot              = [None,            optional_dot,       Call       ],
        QuestionBracket          = [None,            optional_subscript, Call       ],
        Is                       = [None,            binary,             Equality   ],
        EqualEqual               = [None,            binary,             Equality   ],
        BangEqual                = [None,            binary,             Equality   ],
        Less                     = [None,            binary,             Comparison ],
        LessEqual                = [None,            binary,             Comparison ],
        Greater                  = [None,            binary,             Comparison ],
        GreaterEqual             = [None,            binary,             Comparison ],
        And                      = [None,            and,                And        ],
        Or                       = [None,            or,                 Or         ],
        In                       = [None,            binary,             In         ],
        Identifier               = [variable,        None,               None       ],
        String                   = [string,          None,               None       ],
        Integer                  = [integer,         None,               None       ],
        Float                    = [float,           None,               None       ],
        FstringStart             = [fstring,         None,               None       ],
        TstringStart             = [tstring,         None,               None       ],
        InterpolationStringPart  = [None,            None,               None       ],
        InterpolationStart       = [None,            None,               None       ],
        InterpolationEnd         = [None,            None,               None       ],
        InterpolationStringEnd   = [None,            None,               None       ],
        Nil                      = [literal,         None,               None       ],
        True                     = [literal,         None,               None       ],
        False                    = [literal,         None,               None       ],
        StopIteration            = [literal,         None,               None       ],
        Var                      = [None,            None,               None       ],
        Const                    = [None,            None,               None       ],
        At                       = [None,            None,               None       ],
        Class                    = [None,            None,               None       ],
        This                     = [this,            None,               None       ],
        Super                    = [super_,          None,               None       ],
        Fun                      = [None,            None,               None       ],
        Gen                      = [None,            None,               None       ],
        RightArrow               = [lambda,          None,               None       ],
        Return                   = [None,            None,               None       ],
        Yield                    = [yield_,          None,               None       ],
        If                       = [None,            None,               None       ],
        Unless                   = [None,            None,               None       ],
        Else                     = [None,            None,               None       ],
        Switch                   = [None,            None,               None       ],
        Case                     = [None,            None,               None       ],
        Default                  = [None,            None,               None       ],
        For                      = [None,            None,               None       ],
        ForEach                  = [None,            None,               None       ],
        While                    = [None,            None,               None       ],
        Until                    = [None,            None,               None       ],
        Break                    = [None,            None,               None       ],
        Continue                 = [None,            None,               None       ],
        Apostrophe               = [None,            None,               None       ],
        Import                   = [None,            None,               None       ],
        From                     = [None,            None,               None       ],
        As                       = [None,            None,               None       ],
        Try                      = [None,            None,               None       ],
        Catch                    = [None,            None,               None       ],
        Finally                  = [None,            None,               None       ],
        Throw                    = [None,            None,               None       ],
        Async                    = [None,            None,               None       ],
        Await                    = [None,            None,               None       ],
        Error                    = [None,            None,               None       ],
        Eof                      = [None,            None,               None       ],
    )
}

static RULES: [Rule; 94] = make_rules();

fn rule(kind: TK) -> Rule {
    RULES[kind as usize]
}

// Expressions.
impl Parser<'_> {
    /// Whether the current token could begin an expression (it has a
    /// prefix parsing rule). Used to recognize a probably-forgotten
    /// comma: the next element is already there.
    pub(super) fn current_starts_expression(&self) -> bool {
        self.current
            .as_ref()
            .is_some_and(|token| rule(token.kind).prefix.is_some())
    }

    /// Parse an infix expression to the end
    fn parse_infix_chain(
        &mut self,
        precedence: Precedence,
        can_assign: bool,
        ignore_operators: &[TK],
        mut expr: Expr,
    ) -> Expr {
        while precedence <= rule(self.current.as_ref().unwrap().kind).precedence
            && !ignore_operators.contains(&self.current.as_ref().unwrap().kind)
        {
            self.advance();
            let infix_rule = rule(self.previous.as_ref().unwrap().kind).infix.unwrap();
            expr = infix_rule(self, can_assign, ignore_operators, expr);
        }
        expr
    }

    /// The actual precedence parsing function.
    ///
    /// Based on Vaughan Pratt's "top-down operator precedence parsing".
    /// See: [Crafting Interpreters](https://craftinginterpreters.com/compiling-expressions.html)
    fn try_parse_precedence_ignoring(
        &mut self,
        precedence: Precedence,
        ignore_operators: &[TK],
    ) -> ParseResult {
        if let Some(prefix_rule) = rule(self.current.as_ref().unwrap().kind).prefix {
            self.advance();
            let can_assign = precedence <= Precedence::Assignment;
            let expr = prefix_rule(self, can_assign, ignore_operators);
            let expr = self.parse_infix_chain(precedence, can_assign, ignore_operators, expr);

            if can_assign {
                if self.match_(TK::Equal) {
                    // An `=` after a non-assignable expression is almost
                    // always a mistyped comparison.
                    self.error("Invalid assignment target. Maybe you meant '==' instead of '='?");
                } else if self.match_aug_operator() {
                    self.error("Invalid assignment target.");
                }
            }
            ParseResult::Success(expr)
        } else {
            ParseResult::NoPrefix
        }
    }

    pub(super) fn parse_precedence_ignoring(
        &mut self,
        precedence: Precedence,
        ignore_operators: &[TK],
    ) -> Expr {
        match self.try_parse_precedence_ignoring(precedence, ignore_operators) {
            ParseResult::Success(expr) => expr,
            ParseResult::NoPrefix => {
                self.advance();
                self.error("Expect expression.");
                self.dummy_expr()
            }
        }
    }

    pub(super) fn parse_precedence(&mut self, precedence: Precedence) -> Expr {
        self.parse_precedence_ignoring(precedence, &[])
    }

    /// Parse a unary operator and its operand.
    fn unary(&mut self, _can_assign: bool, ignore_operators: &[TK]) -> Expr {
        let operator = self.previous.as_ref().unwrap().kind;
        let operator_location = self.location();
        let op = match operator {
            TK::Minus => UnaryOp::Neg,
            TK::Bang => UnaryOp::Not,
            _ => unreachable!("Unknown unary operator: {operator}"),
        };
        let operand = Box::new(self.parse_precedence_ignoring(Precedence::Unary, ignore_operators));
        Expr {
            location: operator_location.merge_ordered(&operand.location),
            kind: ExprKind::Unary {
                op,
                operand,
                op_location: operator_location,
            },
        }
    }

    /// Parse a binary operator's right operand. The left operand was parsed
    /// by the caller and is passed in.
    fn binary(&mut self, _can_assign: bool, ignore_operators: &[TK], lhs: Expr) -> Expr {
        let operator = self.previous.as_ref().unwrap().kind;
        let operator_location = self.location();
        let rule = rule(operator);
        let op = match operator {
            TK::BangEqual => BinaryOp::NotEqual,
            TK::EqualEqual => BinaryOp::Equal,
            TK::Greater => BinaryOp::Greater,
            TK::GreaterEqual => BinaryOp::GreaterEqual,
            TK::Less => BinaryOp::Less,
            TK::LessEqual => BinaryOp::LessEqual,
            TK::Plus => BinaryOp::Add,
            TK::Minus => BinaryOp::Sub,
            TK::Star => BinaryOp::Mul,
            TK::Slash => BinaryOp::Div,
            TK::Hat => BinaryOp::BitXor,
            TK::Pipe => BinaryOp::BitOr,
            TK::Amper => BinaryOp::BitAnd,
            TK::Percent => BinaryOp::Mod,
            TK::StarStar => BinaryOp::Exp,
            TK::SlashSlash => BinaryOp::FloorDiv,
            TK::In => BinaryOp::In,
            TK::Is => BinaryOp::Is,
            TK::Colon => BinaryOp::Rational,
            TK::DotDotEqual => BinaryOp::RangeInclusive,
            TK::DotDotLess => BinaryOp::RangeExclusive,
            _ => unreachable!("Unknown binary operator: {operator}"),
        };
        let precedence = Precedence::try_from_primitive(u8::from(rule.precedence) + 1)
            .expect("Invalid precedence in 'binary'");
        let rhs = Box::new(self.parse_precedence_ignoring(precedence, ignore_operators));
        Expr {
            location: lhs.location.merge_ordered(&rhs.location),
            kind: ExprKind::Binary {
                op,
                lhs: Box::new(lhs),
                rhs,
                op_location: operator_location,
            },
        }
    }

    /// Parse a (the) ternary expression
    ///
    /// a ? b : c
    /// Evaluates `a`, if it is truthy, then `b` is evaluated and returned,
    /// otherwise `c` is evaluated and returned.
    ///
    /// When we enter this function, the condition (`a`) has already been
    /// parsed by the caller. This mirrors `conditional_statement`, except
    /// both branches are expressions and there is always an `else`.
    fn ternary(&mut self, _can_assign: bool, ignore_operators: &[TK], condition: Expr) -> Expr {
        let question_location = self.location();
        let then_branch = Box::new(self.parse_precedence_ignoring(
            Precedence::Ternary,
            &[&[TK::Colon], ignore_operators].concat(),
        ));
        self.consume(
            TK::Colon,
            "Expect ':' after 'then' branch of ternary operator.",
        );
        let else_branch = Box::new(self.parse_precedence_ignoring(
            Precedence::Ternary,
            &[&[TK::Colon], ignore_operators].concat(),
        ));
        Expr {
            location: condition.location.merge_ordered(&else_branch.location),
            kind: ExprKind::Ternary {
                condition: Box::new(condition),
                then: then_branch,
                else_: else_branch,
                question_location,
            },
        }
    }

    fn lambda(&mut self, _can_assign: bool, _ignore_operators: &[TK]) -> Expr {
        let fn_lit = self.parse_lambda();
        let end = self.location();
        Expr {
            location: fn_lit.fn_location.merge_ordered(&end),
            kind: ExprKind::Lambda(Box::new(fn_lit)),
        }
    }

    /// Parse a call's argument list. The callee was parsed by the caller.
    fn call(&mut self, _can_assign: bool, _ignore_operators: &[TK], callee: Expr) -> Expr {
        let open_location = self.location();
        let args = self.argument_list();
        let close_location = self.location();
        let args_location = open_location.merge_ordered(&close_location);
        Expr {
            location: callee.location.merge_ordered(&args_location),
            kind: ExprKind::Call {
                callee: Box::new(callee),
                args,
                args_location,
            },
        }
    }

    /// Parse property access.
    ///
    /// This is actually fairly complicated, as cases like
    /// `a.b;`, `a.b = c;` and `a.b()` all have to be handled correctly here.
    /// A following `(` is left to the call rule: `` object.name(args) ``
    /// becomes a `Call` whose callee is this property, which codegen
    /// turns into an `Invoke`.
    fn dot(&mut self, can_assign: bool, _ignore_operators: &[TK], lhs: Expr) -> Expr {
        // The `.` (or `?.` when entered through optional chaining) that
        // triggered this rule, already consumed by the caller.
        self.finish_dot(can_assign, lhs, AccessMode::Direct)
    }

    #[allow(clippy::too_many_lines)] // one cohesive property rule
    fn finish_dot(&mut self, can_assign: bool, lhs: Expr, mode: AccessMode) -> Expr {
        // An optional link anywhere in the chain's postfix spine makes
        // the whole chain unassignable.
        let can_assign = can_assign && !chain_has_optional_link(&lhs);
        let operator_location = self.location();
        self.consume(TK::Identifier, "Expect property name after '.'.");
        let identifier_location = self.location();
        let name = self.previous.as_ref().unwrap().as_str().to_string();
        let target_location = lhs.location.merge_ordered(&identifier_location);
        // Blame the access itself (`.foo` / `?.foo`) with the receiver as
        // context, mirroring the subscript getter.
        let access_location = operator_location.merge_ordered(&identifier_location);

        if can_assign && self.match_(TK::Equal) {
            let op_location = self.location();
            let value = Box::new(self.expression());
            return Expr {
                location: target_location.merge_ordered(&value.location),
                kind: ExprKind::Assign {
                    target: Box::new(Expr {
                        location: target_location,
                        kind: ExprKind::Property {
                            object: Box::new(lhs),
                            name,
                            mode,
                            ctx: ExprContext::Store,
                            access_location,
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
                location: target_location.merge_ordered(&value.location),
                kind: ExprKind::AugAssign {
                    target: Box::new(Expr {
                        location: target_location,
                        kind: ExprKind::Property {
                            object: Box::new(lhs),
                            name,
                            mode,
                            ctx: ExprContext::Store,
                            access_location,
                        },
                    }),
                    op,
                    value,
                    op_location,
                },
            };
        }
        Expr {
            location: target_location,
            kind: ExprKind::Property {
                object: Box::new(lhs),
                name,
                mode,
                ctx: ExprContext::Load,
                access_location,
            },
        }
    }

    /// `` ?[index] `` - the optional-chaining link; never an assignment
    /// target. The chain keeps climbing afterwards.
    fn optional_subscript(
        &mut self,
        _can_assign: bool,
        _ignore_operators: &[TK],
        lhs: Expr,
    ) -> Expr {
        self.finish_subscript(false, lhs, AccessMode::NullSafe)
    }

    /// `` ?.name `` - the optional-chaining link. Parses like `dot()`,
    /// never an assignment target; the chain keeps climbing afterwards.
    fn optional_dot(&mut self, _can_assign: bool, _ignore_operators: &[TK], lhs: Expr) -> Expr {
        self.finish_dot(false, lhs, AccessMode::NullSafe)
    }

    /// Handles the four tokens that directly corresponds to values.
    fn literal(&self, _can_assign: bool, _ignore_operators: &[TK]) -> Expr {
        let kind = self.previous.as_ref().unwrap().kind;
        let location = self.location();
        let lit = match kind {
            TK::False => Literal::False,
            TK::Nil => Literal::Nil,
            TK::StopIteration => Literal::StopIteration,
            TK::True => Literal::True,
            _ => unreachable!("Unknown literal: {kind}"),
        };
        Expr {
            kind: ExprKind::Literal(lit),
            location,
        }
    }

    /// Used for grouping expressions to overwrite default precedence.
    ///
    /// The full expression within the grouping will be parsed as one.
    /// Empty parens create an empty tuple instead.
    fn grouping(&mut self, _can_assign: bool, _ignore_operators: &[TK]) -> Expr {
        let open_location = self.location();
        if self.match_(TK::RightParen) {
            let close_location = self.location();
            return Expr {
                location: open_location.merge_ordered(&close_location),
                kind: ExprKind::Tuple {
                    elements: Vec::new(),
                },
            };
        }
        let inner = Box::new(self.expression());
        // An expression already finished but the next token starts a new
        // one, so the user most likely meant a tuple and forgot the
        // comma.
        if !self.check(TK::RightParen) && self.current_starts_expression() {
            self.error_at_current("Expect ')' after expression. Perhaps you forgot a comma?");
        } else {
            self.consume(TK::RightParen, "Expect ')' after expression.");
        }
        Expr {
            location: open_location.merge_ordered(&self.location()),
            kind: ExprKind::Grouping { inner },
        }
    }

    /// Emit a float literal constant.
    ///
    /// The value is token from the last token, extracts the characters
    /// and parses them to a float.
    /// The constant gets loaded into the current chunks constant table
    /// and the index is pushed after the corresponding `OpCode`.
    /// The VM then loads the constant from the constant table using that index.
    fn float(&mut self, _can_assign: bool, _ignore_operators: &[TK]) -> Expr {
        let string = self.previous.as_ref().unwrap().as_str();
        let value = match parse_float_compiler(string) {
            Ok(value) => value,
            Err(msg) => {
                self.error(&msg);
                return self.dummy_expr();
            }
        };
        Expr {
            kind: ExprKind::Literal(Literal::Float(value)),
            location: self.location(),
        }
    }

    /// Emit an integer literal.
    ///
    /// Works equivalent to [`Self::float`].
    fn integer(&mut self, _can_assign: bool, _ignore_operators: &[TK]) -> Expr {
        let integer_str = self.previous.as_ref().unwrap().as_str();
        let parsed = match parse_integer_compiler(integer_str) {
            Ok(parsed) => parsed,
            Err(msg) => {
                self.error(&msg);
                return self.dummy_expr();
            }
        };
        let lit = match parsed {
            ParsedInteger::Small(value) => Literal::Int(value),
            ParsedInteger::Big(bigint) => Literal::BigInt(bigint),
        };
        Expr {
            kind: ExprKind::Literal(lit),
            location: self.location(),
        }
    }

    /// Emit a string constant.
    ///
    /// Here, the string is taken from the lexeme of the token with the last and first
    /// character (`"`) stripped. Rest works like for [`Self::float`].
    fn string(&self, _can_assign: bool, _ignore_operators: &[TK]) -> Expr {
        let lexeme = self.previous.as_ref().unwrap().as_str();
        let value = lexeme[1..lexeme.len() - 1].to_string();
        Expr {
            kind: ExprKind::Literal(Literal::Str(value)),
            location: self.location(),
        }
    }

    /// Emit fstring parts.
    ///
    /// Fstrings consist of 5 types of tokens.
    /// - `FstringStart` `f"`: which is used to enter the fstring compilation
    /// - `InterpolationStringPart`: The raw non-interpolation contents without any quotes
    /// - `InterpolationStringEnd`: The final non-interpolation part WITH closing quotes
    /// - `InterpolationStart` `${`: Only indicate that an interpolation comes next
    /// - `InterpolationEnd` `}`: Indicates that the interpolation is over
    fn fstring(&mut self, _can_assign: bool, _ignore_operators: &[TK]) -> Expr {
        let open_location = self.location();
        let mut parts: Vec<FstringPart> = Vec::new();
        let mut part_count = 0;
        loop {
            self.advance();
            let token = self.previous.clone().unwrap();
            part_count += 1;
            if part_count == 255 {
                self.error("Can't have more than 255 parts in an f-string.");
                break;
            }
            match token.kind {
                TK::InterpolationStringPart => {
                    // raw slice of string part, no quotes
                    parts.push(FstringPart::Str(token.as_str().to_string()));
                }

                TK::InterpolationStart => {
                    // parse an expression until the matching `}`
                    let expr = self.expression();
                    self.consume(TK::InterpolationEnd, "Expected '}' after interpolation.");
                    parts.push(FstringPart::Interp(Box::new(expr)));
                }

                TK::InterpolationStringEnd => {
                    // remove the trailing quote from the slice
                    let lexeme = token.as_str();
                    parts.push(FstringPart::Str(lexeme[..lexeme.len() - 1].to_string()));
                    break; // done with the fstring
                }

                _ => {
                    // In case the scanner yields something unexpected
                    self.error("Unexpected token in fstring.");
                    break;
                }
            }
        }
        Expr {
            location: open_location.merge_ordered(&self.location()),
            kind: ExprKind::Fstring { parts },
        }
    }

    /// Emit tstring parts.
    ///
    /// Tstrings consist of 5 types of tokens.
    /// - `TstringStart` `t"`: which is used to enter the tstring compilation
    /// - `InterpolationStringPart`: The raw non-interpolation contents without any quotes
    /// - `InterpolationStringEnd`: The final non-interpolation part WITH closing quotes
    /// - `InterpolationStart` `${`: Only indicate that an interpolation comes next
    /// - `InterpolationEnd` `}`: Indicates that the interpolation is over
    ///
    /// Unlike an f-string, the interpolated values keep their source text
    /// (for display) and the static parts are carried by the template as a
    /// prebuilt tuple.
    fn tstring(&mut self, _can_assign: bool, _ignore_operators: &[TK]) -> Expr {
        let open_location = self.location();
        let mut parts: Vec<String> = Vec::new();
        let mut interpolations: Vec<TstringInterp> = Vec::new();
        let mut interpolation_count = 0;

        if self.current_token_kind() == Some(TK::InterpolationStart) {
            // First part is empty.
            parts.push(String::new());
        }

        loop {
            self.advance();
            let token = self.previous.clone().unwrap();
            match token.kind {
                TK::InterpolationStringPart => {
                    // raw slice of string part, no quotes
                    parts.push(token.as_str().to_string());
                }

                TK::InterpolationStart => {
                    interpolation_count += 1;
                    if interpolation_count == 255 {
                        self.error("Can't have more than 255 interpolations in a t-string.");
                        break;
                    }
                    // parse an expression until the matching `}`
                    // Get the start position of the expression
                    let expr_start = self.current_location();

                    // parse an expression until the matching `}`
                    let expr = self.expression();

                    // Get the end position of the expression
                    let expr_end = self.current_location();

                    // Get the lexeme of the expression from the scanner's source
                    let source_text = self
                        .scanner
                        .source_slice(expr_start.index, expr_end.index)
                        .to_string();
                    interpolations.push(TstringInterp {
                        value: Box::new(expr),
                        source_text,
                    });

                    self.consume(TK::InterpolationEnd, "Expected '}' after interpolation.");
                }

                TK::InterpolationStringEnd => {
                    // remove the trailing quote from the slice
                    let lexeme = token.as_str();
                    parts.push(lexeme[..lexeme.len() - 1].to_string());
                    let end_location = token.location;
                    return Expr {
                        location: open_location.merge_ordered(&end_location),
                        kind: ExprKind::Tstring(Box::new(Tstring {
                            parts,
                            interpolations,
                        })),
                    };
                }

                _ => {
                    // In case the scanner yields something unexpected
                    self.error("Unexpected token in tstring.");
                    break;
                }
            }
        }
        let end = self.location();
        Expr {
            location: open_location.merge_ordered(&end),
            kind: ExprKind::Tstring(Box::new(Tstring {
                parts,
                interpolations,
            })),
        }
    }

    /// Parse a list literal ([a, b, c(,)])
    ///
    /// Handles optional trailing commas.
    fn list(&mut self, _can_assign: bool, _ignore_operators: &[TK]) -> Expr {
        let open_location = self.location();
        let mut items = Vec::new();
        // Handle trailing comma
        while !self.check(TK::RightBracket) {
            // No assignments
            let item = self.parse_precedence_ignoring(Precedence::non_assigning(), &[TK::Comma]);
            // The check fires on the 256th parsed item, which is then
            // discarded.
            if items.len() == 255 {
                self.error("Can't have more than 255 items in a list literal.");
                break;
            }
            items.push(item);
            if !self.match_(TK::Comma) {
                // The next token starts a new element, so the comma
                // between them was probably forgotten.
                if !self.check(TK::RightBracket) && self.current_starts_expression() {
                    self.error_at_current(
                        "Expect ',' or ']' after list element. Perhaps you forgot a comma?",
                    );
                }
                break;
            }
        }
        self.consume(TK::RightBracket, "Expect ']' after list literal.");
        let close_location = self.location();
        Expr {
            location: open_location.merge_ordered(&close_location),
            kind: ExprKind::ListLit { items },
        }
    }

    /// Parse a tuple literal 'a, b, c(,)'
    ///
    /// Handles optional trailing commas.
    /// Empty tuples are '()' and parsed by [`Self::grouping`].
    /// This only does tuples with at least one element.
    // This is infix, so the first expression has already been parsed.
    fn tuple(&mut self, _can_assign: bool, ignore_operators: &[TK], lhs: Expr) -> Expr {
        let start_location = lhs.location;
        let mut elements = vec![lhs];
        while let ParseResult::Success(item) = self.try_parse_precedence_ignoring(
            Precedence::Ternary,
            &[&[TK::Comma], ignore_operators].concat(),
        ) {
            if elements.len() == 255 {
                self.error("Can't have more than 255 items in a tuple literal.");
                break;
            }
            elements.push(item);
            if !self.match_(TK::Comma) {
                break;
            }
        }
        let end_location = self.location();
        Expr {
            location: start_location.merge_ordered(&end_location),
            kind: ExprKind::Tuple { elements },
        }
    }

    /// Sets effectively work equivalent to [`Self::list`], except
    /// that curly braces are used instead of square ones.
    /// Dicts work similar, instead that each entry is of the form:
    /// `key: value`. Whether the code is parsed as set or dict is determined
    /// by the presence of a colon after the first token.
    ///
    /// Empty braces `{}` are parsed as an empty set.
    /// Empty dict literal is `{:}`.
    fn hash_collection(&mut self, _can_assign: bool, _ignore_operators: &[TK]) -> Expr {
        let open_location = self.location();

        // Empty set literal `{}`.
        if self.check(TK::RightBrace) {
            return self.finish_set_literal(Vec::new(), open_location);
        }
        // Empty dict literal {:}
        if self.match_(TK::Colon) {
            return self.finish_dict_literal(Vec::new(), open_location);
        }

        // The colon after the first element decides the kind.
        let key = self.parse_hash_element();
        if self.match_(TK::Colon) {
            let value = self.parse_hash_element();
            self.parse_dict_literal(key, value, open_location)
        } else {
            self.parse_set_literal(key, open_location)
        }
    }

    fn parse_hash_element(&mut self) -> Expr {
        self.parse_precedence_ignoring(Precedence::non_assigning(), &[TK::Colon, TK::Comma])
    }

    fn parse_set_literal(&mut self, first: Expr, open_location: Location) -> Expr {
        let mut items = vec![first];
        let mut item_count = 1usize;
        loop {
            if !self.match_(TK::Comma) {
                // The next token starts a new element, so the comma
                // between them was probably forgotten.
                if !self.check(TK::RightBrace) && self.current_starts_expression() {
                    self.error_at_current(
                        "Expect ',' or '}' after set element. Perhaps you forgot a comma?",
                    );
                }
                break;
            }
            if self.check(TK::RightBrace) {
                break;
            }
            items.push(self.parse_hash_element());
            if item_count == 255 {
                self.error("Can't have more than 255 items in a set literal.");
                break;
            }
            item_count += 1;
        }
        self.finish_set_literal(items, open_location)
    }

    fn parse_dict_literal(
        &mut self,
        first_key: Expr,
        first_value: Expr,
        open_location: Location,
    ) -> Expr {
        let mut entries = vec![(first_key, first_value)];
        let mut item_count = 1usize;
        loop {
            if !self.match_(TK::Comma) {
                // The next token starts a new element, so the comma
                // between them was probably forgotten.
                if !self.check(TK::RightBrace) && self.current_starts_expression() {
                    self.error_at_current(
                        "Expect ',' or '}' after dict element. Perhaps you forgot a comma?",
                    );
                }
                break;
            }
            if self.check(TK::RightBrace) {
                break;
            }
            entries.push(self.parse_dict_entry());
            if item_count == 255 {
                self.error("Can't have more than 255 items in a dict literal.");
                break;
            }
            item_count += 1;
        }
        self.finish_dict_literal(entries, open_location)
    }

    /// Parse a `key: value` dict entry.
    fn parse_dict_entry(&mut self) -> (Expr, Expr) {
        let key = self.parse_hash_element();
        self.consume(TK::Colon, "Expect ':' after key.");
        let value = self.parse_hash_element();
        (key, value)
    }

    fn finish_set_literal(&mut self, items: Vec<Expr>, open_location: Location) -> Expr {
        self.consume(TK::RightBrace, "Expect '}' after set literal.");
        let close_location = self.location();
        Expr {
            location: open_location.merge_ordered(&close_location),
            kind: ExprKind::SetLit { items },
        }
    }

    fn finish_dict_literal(&mut self, entries: Vec<(Expr, Expr)>, open_location: Location) -> Expr {
        self.consume(TK::RightBrace, "Expect '}' after dict literal.");
        let close_location = self.location();
        Expr {
            location: open_location.merge_ordered(&close_location),
            kind: ExprKind::DictLit { entries },
        }
    }

    /// Parse subscript (`a[b]`) expressions.
    ///
    /// Implemented similar to [`Self::dot`] to handle getting
    /// and setting.
    /// Also works by invoking `__getitem__` or `__setitem__`
    /// on the value to allow classes to overload how this operation works.
    fn subscript(&mut self, can_assign: bool, _ignore_operators: &[TK], lhs: Expr) -> Expr {
        self.finish_subscript(can_assign, lhs, AccessMode::Direct)
    }

    fn finish_subscript(&mut self, can_assign: bool, lhs: Expr, mode: AccessMode) -> Expr {
        // See `finish_dot`: an optional link in the spine blocks
        // assignment to the whole chain.
        let can_assign = can_assign && !chain_has_optional_link(&lhs);
        let operator_location = self.location();
        let index = Box::new(self.parse_precedence(Precedence::non_assigning()));
        self.consume(TK::RightBracket, "Expect ']' after index.");
        let target_end_location = self.location();
        let target_location = operator_location.merge_ordered(&target_end_location);

        if can_assign && self.match_(TK::Equal) {
            let op_location = self.location();
            let value = Box::new(self.expression());
            return Expr {
                location: target_location.merge_ordered(&value.location),
                kind: ExprKind::Assign {
                    target: Box::new(Expr {
                        location: target_location,
                        kind: ExprKind::Subscript {
                            object: Box::new(lhs),
                            index,
                            mode,
                            ctx: ExprContext::Store,
                            target_location,
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
                location: target_location.merge_ordered(&value.location),
                kind: ExprKind::AugAssign {
                    target: Box::new(Expr {
                        location: target_location,
                        kind: ExprKind::Subscript {
                            object: Box::new(lhs),
                            index,
                            mode,
                            ctx: ExprContext::Store,
                            target_location,
                        },
                    }),
                    op,
                    value,
                    op_location,
                },
            };
        }
        Expr {
            location: lhs.location.merge_ordered(&target_location),
            kind: ExprKind::Subscript {
                object: Box::new(lhs),
                index,
                mode,
                ctx: ExprContext::Load,
                target_location,
            },
        }
    }

    fn logical(
        &mut self,
        op: BoolOperator,
        ignore_operators: &[TK],
        lhs: Expr,
        precedence: Precedence,
    ) -> Expr {
        let op_location = self.location();
        let rhs = Box::new(self.parse_precedence_ignoring(precedence, ignore_operators));
        Expr {
            location: lhs.location.merge_ordered(&rhs.location),
            kind: ExprKind::Logical {
                op,
                lhs: Box::new(lhs),
                rhs,
                op_location,
            },
        }
    }

    /// Short circuiting `and`.
    ///
    /// The result of such an expression is the first operand that evaluates
    /// falsey or the last operand if all are truthy.
    /// The second expression is not evaluated if the first is already false.
    fn and(&mut self, _can_assign: bool, ignore_operators: &[TK], lhs: Expr) -> Expr {
        self.logical(BoolOperator::And, ignore_operators, lhs, Precedence::And)
    }

    /// Short circuiting `or`.
    ///
    /// Work equivalently to [`Self::and`].
    fn or(&mut self, _can_assign: bool, ignore_operators: &[TK], lhs: Expr) -> Expr {
        self.logical(BoolOperator::Or, ignore_operators, lhs, Precedence::Or)
    }

    /// Handle `this`.
    ///
    /// If inside a class this simply works on the local variable
    /// `this` which is initialized to the specific instance.
    ///
    /// Outside of a class context this is a syntax error.
    fn this(&mut self, _can_assign: bool, _ignore_operators: &[TK]) -> Expr {
        if self.current_class().is_none() {
            self.error("Can't use 'this' outside of a class.");
            return self.dummy_expr();
        }
        Expr {
            kind: ExprKind::Variable {
                name: String::from("this"),
                ctx: ExprContext::Load,
            },
            location: self.location(),
        }
    }

    /// `` super.name `` - the superclass method lookup. A following `(`
    /// is left to the call rule (`` super.name(args) `` becomes a
    /// `Call` with this node as the callee, which codegen turns into a
    /// `SuperInvoke`).
    /// Handle `super` expressions that interact with the superlcass.
    ///
    /// Like `this`, `super` also only work when used inside class.
    /// Additionally, the class is required to have a superclass.
    /// Both are checked statically at compile time.
    ///
    /// Unlike `this`, only method access either in the form of a call
    /// or to create a bound method is possible.
    fn super_(&mut self, _can_assign: bool, _ignore_operators: &[TK]) -> Expr {
        match self.current_class() {
            None => {
                self.error("Can't use 'super' outside of a class.");
            }
            Some(class) if !class.has_superclass => {
                self.error("Can't use 'super' in a class with no superclass.");
            }
            _ => {}
        }
        let super_location = self.location();
        self.consume(TK::Dot, "Expect '.' after 'super'.");
        let dot_location = self.location();
        self.consume(TK::Identifier, "Expect superclass method name.");
        let identifier_location = self.location();
        let method = self.previous.as_ref().unwrap().as_str().to_string();

        let access_location = dot_location.merge_ordered(&identifier_location);

        Expr {
            location: super_location.merge_ordered(&identifier_location),
            kind: ExprKind::Super {
                method,
                super_location,
                access_location,
            },
        }
    }

    fn yield_(&mut self, _can_assign: bool, ignore_operators: &[TK]) -> Expr {
        let yield_location = self.location();
        let context = self.function_stack.last().unwrap().kind;
        if context == FunKind::Script {
            self.error("Can't yield from top-level code.");
        } else if context != FunKind::Generator {
            self.error("Can only yield from a generator function.");
        }

        match self.try_parse_precedence_ignoring(Precedence::Assignment, ignore_operators) {
            ParseResult::Success(operand) => Expr {
                location: yield_location.merge_ordered(&operand.location),
                kind: ExprKind::Yield {
                    operand: Some(Box::new(operand)),
                },
            },
            ParseResult::NoPrefix => Expr {
                location: yield_location,
                kind: ExprKind::Yield { operand: None },
            },
        }
    }
}

impl Codegen<'_, '_> {
    /// Helper function to deal with operators that delegate to overloaded methods.
    /// Emits `Invoke` with a name constant index.
    /// `error_*` names the token for the error raised when the
    /// name-constant index overflows a byte operand.
    pub(super) fn invoke_fixed(
        &mut self,
        name: &str,
        arg_count: u8,
        error_message: &str,
        location: OpcodeLocation,
        error_location: Location,
        error_token: &str,
    ) {
        let name_constant = self.identifier_constant(name);
        self.emit_byte(OpCode::Invoke, location);
        if !self.emit_number(name_constant.0, NumberEncoding::Short, location) {
            self.error_at(error_location, error_token, error_message);
        }
        self.emit_byte(arg_count, location);
    }
}

// Free-fn shims over the rule methods: a method's `Self` lifetime is
// early-bound and cannot coerce to the higher-ranked fn-pointer aliases,
// while plain functions are fully late-ranked.
macro_rules! prefix_shim {
    ($($name:ident),* $(,)?) => {
        $(
            fn $name(parser: &mut Parser<'_>, can_assign: bool, ignore_operators: &[TK]) -> Expr {
                parser.$name(can_assign, ignore_operators)
            }
        )*
    };
}
macro_rules! infix_shim {
    ($($name:ident),* $(,)?) => {
        $(
            fn $name(
                parser: &mut Parser<'_>,
                can_assign: bool,
                ignore_operators: &[TK],
                lhs: Expr,
            ) -> Expr {
                parser.$name(can_assign, ignore_operators, lhs)
            }
        )*
    };
}

prefix_shim!(
    grouping,
    hash_collection,
    list,
    unary,
    literal,
    string,
    integer,
    float,
    fstring,
    tstring,
    variable,
    this,
    super_,
    lambda,
    yield_,
);
infix_shim!(
    binary,
    call,
    dot,
    subscript,
    tuple,
    ternary,
    and,
    or,
    optional_dot,
    optional_subscript
);
/// Whether the direct postfix spine of `expr` contains an optional
/// (`?.` / `?[`) link. Assignment is rejected in that case, matching
/// JavaScript: grouping and non-postfix operators end the spine, so
/// `(a?.b).c = 1` is an ordinary assignment expression.
fn chain_has_optional_link(mut expr: &Expr) -> bool {
    loop {
        match &expr.kind {
            ExprKind::Property { mode, object, .. } | ExprKind::Subscript { mode, object, .. } => {
                if *mode == AccessMode::NullSafe {
                    return true;
                }
                expr = object;
            }
            ExprKind::Call { callee, .. } => expr = callee,
            _ => return false,
        }
    }
}
