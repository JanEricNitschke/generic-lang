//! Parser to expressions while respecting operator precedence.
//!
//! Uses Vaughan Pratt's "top-down operator precedence parsing".

use num_enum::{IntoPrimitive, TryFromPrimitive};

use super::{Compiler, FunctionType};
use crate::chunk::OpCode;
use crate::config::LAMBDA_NAME;
use crate::scanner::TokenKind as TK;
use crate::types::{CollectionType, Location, NumberEncoding, OpcodeLocation};
use crate::value::utils::{ParsedInteger, parse_float_compiler, parse_integer_compiler};

// The precedence of the different operators in the language
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub(super) enum Precedence {
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
    const fn non_assigning() -> Self {
        Self::Tuple
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ParseResult {
    /// A prefix rule was found and executed
    Success,
    /// No prefix rule was found for the current token
    NoPrefix,
}

// Typedef for the functions that parse the different types of expressions
type PrefixFn<'scanner, 'arena> = fn(&mut Compiler<'scanner, 'arena>, bool, &[TK]) -> ();
type InfixFn<'scanner, 'arena> = fn(&mut Compiler<'scanner, 'arena>, bool, &[TK], Location) -> ();

// This  specifies the functions that handle the parsing of an operator as prefix or infix,
// as well as its precedence. There will be one such struct for each Token.
#[derive(Clone)]
pub(super) struct Rule<'scanner, 'arena> {
    prefix: Option<PrefixFn<'scanner, 'arena>>,
    infix: Option<InfixFn<'scanner, 'arena>>,
    precedence: Precedence,
}

impl Default for Rule<'_, '_> {
    fn default() -> Self {
        Self {
            prefix: Option::default(),
            infix: Option::default(),
            precedence: Precedence::None,
        }
    }
}

macro_rules! make_rules {
    (@parse_fn None) => { None };
    (@parse_fn $prefix:ident) => { Some(Compiler::$prefix) };

    ($($token:ident = [$prefix:ident, $infix:ident, $precedence:ident]),* $(,)?) => {{
        // Horrible hack to pre-fill the array with *something* before assigning the right values based on the macro input
        // Needed because `Rule` cannot be `Copy` (due to `fn`s)
        // If the tokens get input into the macro in the same order
        // That they appear in the enum then the loop is not needed.
        let mut rules = [$(Rule { prefix: make_rules!(@parse_fn $prefix), infix: make_rules!(@parse_fn $infix), precedence: Precedence::$precedence }),*];
        $(
            rules[TK::$token as usize] = Rule {
                prefix: make_rules!(@parse_fn $prefix),
                infix: make_rules!(@parse_fn $infix),
                precedence: Precedence::$precedence
            };
        )*
        rules
    }};
}

pub(super) type Rules<'scanner, 'arena> = [Rule<'scanner, 'arena>; 90];

// Can't be static because the associated function types include lifetimes
#[rustfmt::skip]
pub(super) fn make_rules<'scanner, 'arena>() -> Rules<'scanner, 'arena> {
    make_rules!(
        LeftParen          = [grouping,        call,      Call      ],
        RightParen         = [None,            None,      None      ],
        LeftBrace          = [hash_collection, None,      None      ],
        RightBrace         = [None,            None,      None      ],
        Colon              = [None,            binary,    Rational  ],
        LeftBracket        = [list,            subscript, Call      ],
        RightBracket       = [None,            None,      None      ],
        Comma              = [None,            tuple,     Tuple    ],
        Default            = [None,            None,      None      ],
        Dot                = [None,            dot,       Call      ],
        Minus              = [unary,           binary,    Term      ],
        MinusEqual         = [None,            None,      None      ],
        Plus               = [None,            binary,    Term      ],
        PlusEqual          = [None,            None,      None      ],
        Pipe               = [None,            binary,    BitOr     ],
        PipeEqual          = [None,            None,      None      ],
        Percent            = [None,            binary,    Factor    ],
        PercentEqual       = [None,            None,      None      ],
        Amper              = [None,            binary,    BitAnd    ],
        AmperEqual         = [None,            None,      None      ],
        Hat                = [None,            binary,    BitXor    ],
        HatEqual           = [None,            None,      None      ],
        Semicolon          = [None,            None,      None      ],
        Slash              = [None,            binary,    Factor    ],
        SlashEqual         = [None,            None,      None      ],
        SlashSlash         = [None,            binary,    Factor    ],
        Star               = [None,            binary,    Factor    ],
        StarEqual          = [None,            None,      None      ],
        StarStar           = [None,            binary,    Exponent  ],
        Bang               = [unary,           None,      None      ],
        BangEqual          = [None,            binary,    Equality  ],
        Equal              = [None,            None,      None      ],
        EqualEqual         = [None,            binary,    Equality  ],
        Greater            = [None,            binary,    Comparison],
        GreaterEqual       = [None,            binary,    Comparison],
        Less               = [None,            binary,    Comparison],
        LessEqual          = [None,            binary,    Comparison],
        Identifier         = [variable,        None,      None      ],
        In                 = [None,            binary,    In        ],
        String             = [string,          None,      None      ],
        Float              = [float,           None,      None      ],
        Integer            = [integer,         None,      None      ],
        And                = [None,            and,       And       ],
        Case               = [None,            None,      None      ],
        Class              = [None,            None,      None      ],
        Const              = [None,            None,      None      ],
        Continue           = [None,            None,      None      ],
        Break              = [None,            None,      None      ],
        Else               = [None,            None,      None      ],
        False              = [literal,         None,      None      ],
        For                = [None,            None,      None      ],
        Apostrophe         = [None,            None,      None      ],
        At                 = [None,            None,      None      ],
        Fun                = [None,            None,      None      ],
        Gen                = [None,            None,      None      ],
        RightArrow         = [lambda,          None,      None      ],
        QuestionMark       = [None,            ternary,   Ternary   ],
        If                 = [None,            None,      None      ],
        Unless             = [None,            None,      None      ],
        Nil                = [literal,         None,      None      ],
        Or                 = [None,            or,        Or        ],
        Switch             = [None,            None,      None      ],
        Super              = [super_,          None,      None      ],
        This               = [this,            None,      None      ],
        True               = [literal,         None,      None      ],
        Var                = [None,            None,      None      ],
        While              = [None,            None,      None      ],
        Until              = [None,            None,      None      ],
        From               = [None,            None,      None      ],
        Import             = [None,            None,      None      ],
        From               = [None,            None,      None      ],
        As                 = [None,            None,      None      ],
        Error              = [None,            None,      None      ],
        Eof                = [None,            None,      None      ],
        Return             = [None,            None,      None      ],
        Yield              = [yield_,          None,      None      ],
        Await              = [None,            None,      None      ],
        Async              = [None,            None,      None      ],
        StopIteration      = [literal,         None,      None      ],
        Try                = [None,            None,      None      ],
        Catch              = [None,            None,      None      ],
        Finally            = [None,            None,      None      ],
        Throw              = [None,            None,      None      ],
        DotDotLess         = [None,            binary,    Range     ],
        DotDotEqual        = [None,            binary,    Range     ],
        FstringStart       = [fstring,         None,      None      ],
        FstringEnd         = [None,            None,      None      ],
        FstringPart        = [None,            None,      None      ],
        InterpolationStart = [None,            None,      None      ],
        InterpolationEnd   = [None,            None,      None      ],
    )
}

impl<'scanner, 'arena> Compiler<'scanner, 'arena> {
    const fn get_rule(&self, operator: TK) -> &Rule<'scanner, 'arena> {
        &self.rules[operator as usize]
    }

    /// The actual precedence parsing function.
    ///
    /// Based on Vaughan Pratt's "top-down operator precedence parsing".
    /// See: [Crafting Interpreters](https://craftinginterpreters.com/compiling-expressions.html)
    pub(super) fn try_parse_precedence_ignoring(
        &mut self,
        precedence: Precedence,
        ignore_operators: &[TK],
    ) -> ParseResult {
        if let Some(prefix_rule) = self.get_rule(self.current.as_ref().unwrap().kind).prefix {
            self.advance();
            let lhs_start_location = self.location();
            let can_assign = precedence <= Precedence::Assignment;
            prefix_rule(self, can_assign, ignore_operators);

            while precedence
                <= self
                    .get_rule(self.current.as_ref().unwrap().kind)
                    .precedence
                && !ignore_operators.contains(&self.current.as_ref().unwrap().kind)
            {
                let lhs_end_location = self.location();
                self.advance();
                let infix_rule = self
                    .get_rule(self.previous.as_ref().unwrap().kind)
                    .infix
                    .unwrap();
                infix_rule(
                    self,
                    can_assign,
                    ignore_operators,
                    lhs_start_location.merge_ordered(&lhs_end_location),
                );
            }

            if can_assign
                && (self.match_(TK::Equal)
                    | self.match_(TK::PlusEqual)
                    | self.match_(TK::MinusEqual)
                    | self.match_(TK::StarEqual)
                    | self.match_(TK::SlashEqual)
                    | self.match_(TK::HatEqual)
                    | self.match_(TK::PipeEqual)
                    | self.match_(TK::AmperEqual)
                    | self.match_(TK::PercentEqual))
            {
                self.error("Invalid assignment target.");
            }
            ParseResult::Success
        } else {
            ParseResult::NoPrefix
        }
    }

    pub(super) fn parse_precedence_ignoring(
        &mut self,
        precedence: Precedence,
        ignore_operators: &[TK],
    ) {
        match self.try_parse_precedence_ignoring(precedence, ignore_operators) {
            ParseResult::Success => {}
            ParseResult::NoPrefix => {
                self.advance();
                self.error("Expect expression.");
            }
        }
    }

    pub(super) fn parse_precedence(&mut self, precedence: Precedence) {
        self.parse_precedence_ignoring(precedence, &[]);
    }

    /// Parse the expression which will leave its value on the stack.
    /// Then emit the bytecode for the respective operation which will act on the value on the stack.
    fn unary(&mut self, _can_assign: bool, ignore_operators: &[TK]) {
        let operator = self.previous.as_ref().unwrap().kind;
        let operator_location = self.location();

        let start_location = self.current_location();
        self.parse_precedence_ignoring(Precedence::Unary, ignore_operators);
        let end_location = self.location();
        let location = OpcodeLocation {
            preceding: None,
            source: operator_location,
            following: Some(start_location.merge_ordered(&end_location)),
        };
        match operator {
            TK::Minus => self.emit_byte(OpCode::Negate, location),
            TK::Bang => self.emit_byte(OpCode::Not, location),
            _ => unreachable!("Unknown unary operator: {}", operator),
        }
    }

    /// For a binary operator, we need to parse the right operand and then emit the correct bytecode.
    /// The left operand is already on the stack.
    /// The final order on the stack will be that the right operand is on top of the left one.
    /// This is then handled correctly in the VM when the bytecode of a binary operator is encountered.
    fn binary(&mut self, _can_assign: bool, ignore_operators: &[TK], lhs_location: Location) {
        // First operand is already on the stack
        let operator = self.previous.as_ref().unwrap().kind;
        let operator_location = self.location();

        let rule = self.get_rule(operator);

        let rhs_start_location = self.current_location();
        // Correctly put the second operand on the stack
        self.parse_precedence_ignoring(
            Precedence::try_from_primitive(u8::from(rule.precedence) + 1).expect(
                "Invalid precedence in 'binary', should never be called for `Primary expression`.",
            ),
            ignore_operators,
        );
        let rhs_end_location = self.location();

        let location = OpcodeLocation {
            preceding: Some(lhs_location),
            source: operator_location,
            following: Some(rhs_start_location.merge_ordered(&rhs_end_location)),
        };

        // Emit the correct byte code to perform the operation on the two values
        match operator {
            TK::BangEqual => self.emit_byte(OpCode::NotEqual, location),
            TK::EqualEqual => self.emit_byte(OpCode::Equal, location),
            TK::Greater => self.emit_byte(OpCode::Greater, location),
            TK::GreaterEqual => self.emit_byte(OpCode::GreaterEqual, location),
            TK::Less => self.emit_byte(OpCode::Less, location),
            TK::LessEqual => self.emit_byte(OpCode::LessEqual, location),
            TK::Plus => self.emit_byte(OpCode::Add, location),
            TK::Minus => self.emit_byte(OpCode::Subtract, location),
            TK::Star => self.emit_byte(OpCode::Multiply, location),
            TK::Slash => self.emit_byte(OpCode::Divide, location),
            TK::Hat => self.emit_byte(OpCode::BitXor, location),
            TK::Pipe => self.emit_byte(OpCode::BitOr, location),
            TK::Amper => self.emit_byte(OpCode::BitAnd, location),
            TK::Percent => self.emit_byte(OpCode::Mod, location),
            TK::StarStar => self.emit_byte(OpCode::Exp, location),
            TK::SlashSlash => self.emit_byte(OpCode::FloorDiv, location),
            TK::In => self.in_(location),
            TK::Colon => self.emit_byte(OpCode::BuildRational, location),
            //  Could think about making these one opcode with a boolean operand
            TK::DotDotEqual => self.emit_byte(OpCode::BuildRangeInclusive, location),
            TK::DotDotLess => self.emit_byte(OpCode::BuildRangeExclusive, location),
            _ => unreachable!("Unknown binary operator: {}", operator),
        }
    }

    /// Parse a (the) ternary expression
    ///
    /// a ? b : c
    /// Evaluates `a`, if it is truthy, then `b` is evaluated and returned,
    /// otherwise `c` is evaluated and returned.
    ///
    /// When we enter this function, the first operand (`a`) is already on the stack.
    /// For the rest of the logic have a look at [`Compiler::conditional_statement`].
    /// We work exactly like that except we always have a "then" and we only have expressions
    /// instead of blocks.
    fn ternary(
        &mut self,
        _can_assign: bool,
        ignore_operators: &[TK],
        condition_location: Location,
    ) {
        let location = OpcodeLocation::new(condition_location);
        let then_jump = self.emit_jump(OpCode::PopJumpIfFalse, location);

        // First value: We parse the "then" expression and jump over the "else" expression.
        // The condition has already been popped by PopJumpIfFalse.
        self.parse_precedence_ignoring(
            Precedence::Ternary,
            &[&[TK::Colon], ignore_operators].concat(),
        );

        let else_jump = self.emit_jump(OpCode::Jump, location);

        self.consume(
            TK::Colon,
            "Expect ':' after 'then' branch of ternary operator.",
        );

        // Patch the jump over the "else" expression to continue after the end of the "then" expression.
        self.patch_jump(then_jump);

        self.parse_precedence_ignoring(
            Precedence::Ternary,
            &[&[TK::Colon], ignore_operators].concat(),
        );

        self.patch_jump(else_jump);
    }

    /// Parse lambda expressions.
    ///
    /// We support either single expressions, where no `return` is needed,
    /// or full blocks with a `return` statement.
    fn lambda(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        self.function(&LAMBDA_NAME, FunctionType::Function, true);
    }

    /// Parse `X in Y`
    ///
    /// Work by invoking `Y.contains(X)`.
    ///
    /// Order on the stack is element -- container
    /// To call a method container.contains(element)
    // We need container -- element
    /// Then call `OP_INVOKE` with "contains" and `1`
    fn in_(&mut self, operator_centered_location: OpcodeLocation) {
        // Swap the order
        let collection_location = OpcodeLocation {
            preceding: Some(
                operator_centered_location
                    .preceding
                    .unwrap()
                    .merge_ordered(&operator_centered_location.source),
            ),
            source: operator_centered_location.following.unwrap(),
            following: None,
        };
        self.emit_byte(OpCode::Swap, collection_location);
        self.invoke_fixed(
            &"contains",
            1,
            "Too many constants created for OP_IN.",
            collection_location,
        );
    }

    /// Parsing any call just means parsing the arguments and then emitting the correct bytecode.
    fn call(&mut self, _can_assign: bool, _ignore_operators: &[TK], lhs_location: Location) {
        let argument_list_start_location = self.location();
        let arg_count = self.argument_list();
        let argument_list_end_location = self.location();
        let location = OpcodeLocation {
            preceding: Some(lhs_location),
            source: argument_list_start_location.merge_ordered(&argument_list_end_location),
            following: None,
        };
        self.emit_bytes(OpCode::Call, arg_count, location);
    }

    /// Parse property access.
    ///
    /// This is actually fairly complicated, as cases like
    /// `a.b;`, `a.b = c;` and `a.b()` all have to be handled correctly here.
    fn dot(&mut self, can_assign: bool, _ignore_operators: &[TK], lhs_location: Location) {
        self.consume(TK::Identifier, "Expect property name after '.'.");
        let identifier_location = self.location();
        let name_constant =
            self.identifier_constant(&self.previous.as_ref().unwrap().as_str().to_string());
        let target_location = lhs_location.merge_ordered(&identifier_location);
        let getter_location = OpcodeLocation::new(target_location);

        if can_assign
            && (self.match_(TK::Equal)
                | self.match_(TK::PlusEqual)
                | self.match_(TK::MinusEqual)
                | self.match_(TK::StarEqual)
                | self.match_(TK::SlashEqual)
                | self.match_(TK::HatEqual)
                | self.match_(TK::PipeEqual)
                | self.match_(TK::AmperEqual)
                | self.match_(TK::PercentEqual))
        {
            let previous_kind = self.previous.as_ref().unwrap().kind;
            let operator_location = self.location();
            if matches!(previous_kind, TK::Equal) {
                self.expression();
            } else {
                self.emit_byte(OpCode::Dup, getter_location);
                self.emit_byte(OpCode::GetProperty, getter_location);
                if !self.emit_number(name_constant.0, NumberEncoding::Short, getter_location) {
                    self.error("Too many constants created for OP_GET_PROPERTY.");
                }
                let rhs_start_location = self.current_location();
                self.expression();
                let rhs_end_location = self.location();
                let operator_centered_location = OpcodeLocation {
                    preceding: Some(target_location),
                    source: operator_location,
                    following: Some(rhs_start_location.merge_ordered(&rhs_end_location)),
                };
                match previous_kind {
                    TK::PlusEqual => self.emit_byte(OpCode::Add, operator_centered_location),
                    TK::MinusEqual => self.emit_byte(OpCode::Subtract, operator_centered_location),
                    TK::StarEqual => self.emit_byte(OpCode::Multiply, operator_centered_location),
                    TK::SlashEqual => self.emit_byte(OpCode::Divide, operator_centered_location),
                    TK::HatEqual => self.emit_byte(OpCode::BitXor, operator_centered_location),
                    TK::PipeEqual => self.emit_byte(OpCode::BitOr, operator_centered_location),
                    TK::AmperEqual => self.emit_byte(OpCode::BitAnd, operator_centered_location),
                    TK::PercentEqual => self.emit_byte(OpCode::Mod, operator_centered_location),
                    _ => unreachable!("Unexpected byte code "),
                }
            }
            let setter_location = OpcodeLocation {
                preceding: None,
                source: target_location,
                following: Some(operator_location),
            };
            self.emit_byte(OpCode::SetProperty, setter_location);
            if !self.emit_number(name_constant.0, NumberEncoding::Short, setter_location) {
                self.error("Too many constants created for OP_SET_PROPERTY");
            }
        } else if self.match_(TK::LeftParen) {
            let args_start_location = self.location();
            let arg_count = self.argument_list();
            let args_end_location = self.location();
            let invoke_location = OpcodeLocation {
                preceding: Some(target_location),
                source: args_start_location.merge_ordered(&args_end_location),
                following: None,
            };
            self.emit_byte(OpCode::Invoke, invoke_location);
            if !self.emit_number(name_constant.0, NumberEncoding::Short, invoke_location) {
                self.error("Too many constants created for OP_INVOKE");
            }
            self.emit_byte(arg_count, invoke_location);
        } else {
            self.emit_byte(OpCode::GetProperty, getter_location);
            if !self.emit_number(name_constant.0, NumberEncoding::Short, getter_location) {
                self.error("Too many constants created for OP_GET_PROPERTY.");
            }
        }
    }

    /// Handles the four tokens that directly corresponds to values.
    fn literal(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        let literal = self.previous.as_ref().unwrap().kind;
        let location = self.op_location();
        match literal {
            TK::False => self.emit_byte(OpCode::False, location),
            TK::Nil => self.emit_byte(OpCode::Nil, location),
            TK::StopIteration => self.emit_byte(OpCode::StopIteration, location),
            TK::True => self.emit_byte(OpCode::True, location),
            _ => unreachable!("Unknown literal: {}", literal),
        }
    }

    /// Used for grouping expressions to overwrite default precedence.
    ///
    /// The full expression within the grouping will be parsed as one.
    /// Empty parens create an empty tuple instead.
    fn grouping(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        if self.match_(TK::RightParen) {
            self.emit_bytes(OpCode::BuildTuple, 0, self.op_location());
            return;
        }
        self.expression();
        self.consume(TK::RightParen, "Expect ')' after expression.");
    }

    /// Emit a float literal constant.
    ///
    /// The value is token from the last token, extracts the characters
    /// and parses them to a float.
    /// The constant gets loaded into the current chunks constant table
    /// and the index is pushed after the corresponding `OpCode`.
    /// The VM then loads the constant from the constant table using that index.
    fn float(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        let string = self.previous.as_ref().unwrap().as_str();
        let value = parse_float_compiler(string).unwrap();
        self.emit_constant(value, self.op_location());
    }

    /// Emit an integer literal.
    ///
    /// Works equivalent to [`Compiler::float`].
    fn integer(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        let integer_str = self.previous.as_ref().unwrap().as_str();
        match parse_integer_compiler(integer_str).unwrap() {
            ParsedInteger::Small(value) => self.emit_constant(value, self.op_location()),
            ParsedInteger::Big(bigint) => {
                let bigint_id = self.heap.add_big_int(bigint);
                self.emit_constant(bigint_id, self.op_location());
            }
        }
    }

    /// Emit a string constant.
    ///
    /// Here, the string is taken from the lexeme of the token with the last and first
    /// character (`"`) stripped. Rest works like for [`Compiler::float`].
    fn string(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        let lexeme = self.previous.as_ref().unwrap().as_str();
        let value = lexeme[1..lexeme.len() - 1].to_string();
        let string_id = self.heap.string_id(&value);
        self.emit_constant(string_id, self.op_location());
    }

    /// Emit fstring parts.
    ///
    /// Fstrings consist of 5 types of tokens.
    /// - `FstringStart` `f"`: which is used to enter the fstring compilation
    /// - `FstringPart`: The raw non-interpolation contents without any quotes
    /// - `FstringEnd`: The final non-interpolation part WITH closing quotes
    /// - `InterpolationStart` `${`: Only indicate that an interpolation comes next
    /// - `InterpolationEnd` `}`: Indicates that the interpolation is over
    fn fstring(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        let mut part_count = 0;
        loop {
            self.advance();
            let token = self.previous.as_ref().unwrap();
            part_count += 1;
            if part_count == 255 {
                self.error("Can't have more than 255 parts in an f-string.");
                break;
            }
            match token.kind {
                TK::FstringPart => {
                    // raw slice of string part, no quotes
                    let string_id = self.heap.string_id(&token.as_str().to_string());
                    self.emit_constant(string_id, self.op_location());
                }

                TK::InterpolationStart => {
                    // parse an expression until the matching `}`
                    self.expression();
                    self.consume(TK::InterpolationEnd, "Expected '}' after interpolation.");
                }

                TK::FstringEnd => {
                    // remove the trailing quote from the slice
                    let lexeme = token.as_str();
                    let value = lexeme[..lexeme.len() - 1].to_string();
                    let string_id = self.heap.string_id(&value);
                    self.emit_constant(string_id, self.op_location());

                    self.emit_bytes(OpCode::BuildFstring, part_count, self.op_location());
                    break; // done with the fstring
                }

                _ => {
                    // In case the scanner yields something unexpected
                    self.error("Unexpected token in fstring.");
                    break;
                }
            }
        }
    }

    /// Parse a list literal ([a, b, c(,)])
    ///
    /// Handles optional trailing commas.
    fn list(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        let mut item_count = 0;
        // Handle trailing comma
        while !self.check(TK::RightBracket) {
            // No assignments
            self.parse_precedence_ignoring(Precedence::non_assigning(), &[TK::Comma]);
            if item_count == 255 {
                self.error("Can't have more than 255 items in a list literal.");
                break;
            }
            item_count += 1;
            if !self.match_(TK::Comma) {
                break;
            }
        }
        self.consume(TK::RightBracket, "Expect ']' after list literal.");
        self.emit_bytes(OpCode::BuildList, item_count, self.op_location());
    }

    /// Parse a tuple literal 'a, b, c(,)'
    ///
    /// Handles optional trailing commas.
    /// Empty tuples are '()' and parsed by [`Compiler::grouping`].
    /// This only does tuples with at least one element.
    fn tuple(&mut self, _can_assign: bool, ignore_operators: &[TK], _lhs_location: Location) {
        // This is infix, so the first expression has already been parsed.

        let mut item_count = 1;
        // Handle trailing comma
        while self.try_parse_precedence_ignoring(
            Precedence::Ternary,
            &[&[TK::Comma], ignore_operators].concat(),
        ) == ParseResult::Success
        {
            if item_count == 255 {
                self.error("Can't have more than 255 items in a tuple literal.");
                break;
            }
            item_count += 1;
            if !self.match_(TK::Comma) {
                break;
            }
        }

        self.emit_bytes(OpCode::BuildTuple, item_count, self.op_location());
    }

    /// Handle the hashed collection `set`and `dict``.`
    ///
    /// Sets effectively work equivalent to [`Compiler::list`], except
    /// that curly braces are used instead of square ones.
    /// Dicts work similar, instead that each entry is of the form:
    /// `key: value`. Whether the code is parsed as set or dict is determined
    /// by the presence of a colon after the first token.
    ///
    /// Empty braces `{}` are parsed as an empty set.
    /// Empty dict literal is `{:}`.
    fn hash_collection(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        // Empty set literal
        if self.check(TK::RightBrace) {
            return self.finish_hash_collection(CollectionType::Set, 0);
        }

        // Empty dict literal {:}
        if self.match_(TK::Colon) {
            return self.finish_hash_collection(CollectionType::Dict, 0);
        }

        // First element
        let mut item_count = 0;
        self.parse_precedence_ignoring(Precedence::non_assigning(), &[TK::Colon, TK::Comma]);
        let collection_type: CollectionType = if self.match_(TK::Colon) {
            CollectionType::Dict
        } else {
            CollectionType::Set
        };
        if collection_type == CollectionType::Dict {
            self.parse_precedence_ignoring(Precedence::non_assigning(), &[TK::Colon, TK::Comma]);
        }
        item_count += 1;
        self.match_(TK::Comma); // optional trailing comma after first

        // Remaining elements
        while !self.check(TK::RightBrace) {
            match collection_type {
                CollectionType::Dict => self.parse_dict_entry(),
                CollectionType::Set => self.parse_precedence_ignoring(
                    Precedence::non_assigning(),
                    &[TK::Colon, TK::Comma],
                ),
            }

            if item_count == 255 {
                self.error("Can't have more than 255 items in a set literal.");
                break;
            }
            item_count += 1;
            if !self.match_(TK::Comma) {
                break;
            }
        }

        self.finish_hash_collection(collection_type, item_count);
    }

    /// Helper function to consume the closing brace and emit the appropriate
    /// collection creation opcode (`BuildDict` or `BuildSet`) for dict or set literals.
    fn finish_hash_collection(&mut self, collection_type: CollectionType, item_count: u8) {
        self.consume(
            TK::RightBrace,
            &format!(
                "Expect '}}' after {} literal.",
                match collection_type {
                    CollectionType::Dict => "dict",
                    CollectionType::Set => "set",
                }
            ),
        );
        self.emit_bytes(
            match collection_type {
                CollectionType::Dict => OpCode::BuildDict,
                CollectionType::Set => OpCode::BuildSet,
            },
            item_count,
            self.op_location(),
        );
    }

    fn parse_dict_entry(&mut self) {
        self.parse_precedence_ignoring(Precedence::non_assigning(), &[TK::Colon, TK::Comma]);
        self.consume(TK::Colon, "Expect ':' after key.");
        self.parse_precedence_ignoring(Precedence::non_assigning(), &[TK::Colon, TK::Comma]);
    }

    /// Parse subscript (`a[b]`) expressions.
    ///
    /// Implemented similar to [`Compiler::dot`] to handle getting
    /// and setting.
    ///
    /// Also works by invoking `__getitem__` or `__setitem__`
    /// on the value to allow classes to overload how this operation works.
    fn subscript(&mut self, can_assign: bool, _ignore_operators: &[TK], lhs_location: Location) {
        let target_start_location = self.location();
        self.parse_precedence(Precedence::non_assigning());
        self.consume(TK::RightBracket, "Expect ']' after index.");
        let target_end_location = self.location();
        let target_location = target_start_location.merge_ordered(&target_end_location);
        let getter_location = OpcodeLocation {
            preceding: Some(lhs_location),
            source: target_location,
            following: None,
        };
        if can_assign
            && (self.match_(TK::Equal)
                | self.match_(TK::PlusEqual)
                | self.match_(TK::MinusEqual)
                | self.match_(TK::StarEqual)
                | self.match_(TK::SlashEqual)
                | self.match_(TK::HatEqual)
                | self.match_(TK::PipeEqual)
                | self.match_(TK::AmperEqual)
                | self.match_(TK::PercentEqual))
        {
            let previous_kind = self.previous.as_ref().unwrap().kind;
            let operator_location = self.location();
            if matches!(previous_kind, TK::Equal) {
                self.expression();
            } else {
                self.emit_bytes(OpCode::DupN, 2, getter_location);
                self.invoke_fixed(
                    &"__getitem__",
                    1,
                    "Too many constants created for OP_SUBSCRIPT.",
                    getter_location,
                );
                let rhs_start_location = self.current_location();
                self.expression();
                let rhs_end_location = self.location();
                let operator_centered_location = OpcodeLocation {
                    preceding: Some(lhs_location.merge_ordered(&target_location)),
                    source: operator_location,
                    following: Some(rhs_start_location.merge_ordered(&rhs_end_location)),
                };
                match previous_kind {
                    TK::PlusEqual => self.emit_byte(OpCode::Add, operator_centered_location),
                    TK::MinusEqual => self.emit_byte(OpCode::Subtract, operator_centered_location),
                    TK::StarEqual => self.emit_byte(OpCode::Multiply, operator_centered_location),
                    TK::SlashEqual => self.emit_byte(OpCode::Divide, operator_centered_location),
                    TK::HatEqual => self.emit_byte(OpCode::BitXor, operator_centered_location),
                    TK::PipeEqual => self.emit_byte(OpCode::BitOr, operator_centered_location),
                    TK::AmperEqual => self.emit_byte(OpCode::BitAnd, operator_centered_location),
                    TK::PercentEqual => self.emit_byte(OpCode::Mod, operator_centered_location),
                    _ => unreachable!("Unexpected byte code "),
                }
            }
            let setter_location = OpcodeLocation {
                preceding: None,
                source: target_location,
                following: Some(operator_location),
            };
            self.invoke_fixed(
                &"__setitem__",
                2,
                "Too many constants created for OP_SUBSCRIPT.",
                setter_location,
            );
        } else {
            self.invoke_fixed(
                &"__getitem__",
                1,
                "Too many constants created for OP_SUBSCRIPT.",
                getter_location,
            );
        }
    }

    /// Short circuiting `and`.
    ///
    /// The result of such an expression is the first operand that evaluates
    /// falsey or the last operand if all are truthy.
    /// The second expression is not evaluated if the first is already false.
    fn and(&mut self, _can_assign: bool, ignore_operators: &[TK], lhs_location: Location) {
        let end_jump = self.emit_jump(
            OpCode::JumpIfFalseOrPop,
            OpcodeLocation {
                preceding: None,
                source: lhs_location,
                following: Some(self.location()),
            },
        );
        self.parse_precedence_ignoring(Precedence::And, ignore_operators);
        self.patch_jump(end_jump);
    }

    /// Short circuiting `or`.
    ///
    /// Work equivalently to [`Compiler::and`].
    fn or(&mut self, _can_assign: bool, ignore_operators: &[TK], lhs_location: Location) {
        let end_jump = self.emit_jump(
            OpCode::JumpIfTrueOrPop,
            OpcodeLocation {
                preceding: None,
                source: lhs_location,
                following: Some(self.location()),
            },
        );
        self.parse_precedence_ignoring(Precedence::Or, ignore_operators);
        self.patch_jump(end_jump);
    }

    /// Handle `this`.
    ///
    /// If inside a class this simply works on the local variable
    /// `this` which is initialized to the specific instance.
    ///
    /// Outside of a class context this is a syntax error.
    fn this(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        if self.current_class().is_none() {
            self.error("Can't use 'this' outside of a class.");
            return;
        }
        self.variable(false, &[]);
    }

    /// Handle `super` expressions that interact with the superlcass.
    ///
    /// Like `this`, `super` also only work when used inside class.
    /// Additionally, the class is required to have a superclass.
    /// Both are checked statically at compile time.
    ///
    /// Unlike `this`, only method access either in the form of a call
    /// or to create a bound method is possible.
    fn super_(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
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

        self.consume(TK::Identifier, "Expect superclass method name.");
        let identifier_location = self.location();
        let name = self.identifier_constant(&self.previous.as_ref().unwrap().as_str().to_string());

        let target_location = super_location.merge_ordered(&identifier_location);

        self.named_variable(
            &self.synthetic_token(TK::This).as_str(),
            false,
            self.location(),
        );
        if self.match_(TK::LeftParen) {
            let args_start_location = self.location();
            let arg_count = self.argument_list();
            let args_end_location = self.location();
            let invoke_location = OpcodeLocation {
                preceding: Some(target_location),
                source: args_start_location.merge_ordered(&args_end_location),
                following: None,
            };
            self.named_variable(
                &self.synthetic_token(TK::Super).as_str(),
                false,
                super_location,
            );
            self.emit_byte(OpCode::SuperInvoke, invoke_location);
            if !self.emit_number(*name, NumberEncoding::Short, invoke_location) {
                self.error("Too many constants while compiling OP_SUPER_INVOKE");
            }
            self.emit_byte(arg_count, invoke_location);
        } else {
            let getter_location = OpcodeLocation::new(target_location);
            self.named_variable(
                &self.synthetic_token(TK::Super).as_str(),
                false,
                super_location,
            );
            self.emit_byte(OpCode::GetSuper, getter_location);
            if !self.emit_number(*name, NumberEncoding::Short, getter_location) {
                self.error("Too many constants while compiling OP_SUPER_INVOKE");
            }
        }
    }

    fn yield_(&mut self, _can_assign: bool, ignore_operators: &[TK]) {
        let yield_location = self.location();
        if self.function_type() == FunctionType::Script {
            self.error("Can't yield from top-level code.");
        } else if self.function_type() != FunctionType::Generator {
            self.error("Can only yield from a generator function.");
        }

        let start_location = self.current_location();
        match self.try_parse_precedence_ignoring(Precedence::Assignment, ignore_operators) {
            ParseResult::Success => {
                let end_location = self.current_location();
                self.emit_byte(
                    OpCode::Yield,
                    OpcodeLocation {
                        preceding: Some(yield_location),
                        source: start_location.merge_ordered(&end_location),
                        following: None,
                    },
                );
            }
            ParseResult::NoPrefix => {
                self.emit_bytes(
                    OpCode::Nil,
                    OpCode::Yield,
                    OpcodeLocation::new(yield_location),
                );
            }
        }
    }

    /// Helper function to deal with operators that delegate to overloaded methods.
    pub(super) fn invoke_fixed<S: ToString>(
        &mut self,
        name: &S,
        arg_count: u8,
        error_message: &str,
        location: OpcodeLocation,
    ) {
        let name_constant = self.identifier_constant(name);
        self.emit_byte(OpCode::Invoke, location);
        if !self.emit_number(name_constant.0, NumberEncoding::Short, location) {
            self.error(error_message);
        }
        self.emit_byte(arg_count, location);
    }
}
