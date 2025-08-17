//! Parser to expressions while respecting operator precedence.
//!
//! Uses Vaughan Pratt's "top-down operator precedence parsing".

use num_bigint::BigInt;
use num_enum::{IntoPrimitive, TryFromPrimitive};

use super::{Compiler, FunctionType};
use crate::chunk::OpCode;
use crate::config::LAMBDA_NAME;
use crate::scanner::TokenKind as TK;

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
type ParseFn<'scanner, 'arena> = fn(&mut Compiler<'scanner, 'arena>, bool, &[TK]) -> ();

// This  specifies the functions that handle the parsing of an operator as prefix or infix,
// as well as its precedence. There will be one such struct for each Token.
#[derive(Clone)]
pub(super) struct Rule<'scanner, 'arena> {
    prefix: Option<ParseFn<'scanner, 'arena>>,
    infix: Option<ParseFn<'scanner, 'arena>>,
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

pub(super) type Rules<'scanner, 'arena> = [Rule<'scanner, 'arena>; 89];

// Can't be static because the associated function types include lifetimes
#[rustfmt::skip]
pub(super) fn make_rules<'scanner, 'arena>() -> Rules<'scanner, 'arena> {
    make_rules!(
        LeftParen     = [grouping,        call,      Call      ],
        RightParen    = [None,            None,      None      ],
        LeftBrace     = [hash_collection, None,      None      ],
        RightBrace    = [None,            None,      None      ],
        Colon         = [None,            binary,    Rational  ],
        LeftBracket   = [list,            subscript, Call      ],
        RightBracket  = [None,            None,      None      ],
        Comma         = [None,            tuple,     Tuple    ],
        Default       = [None,            None,      None      ],
        Dot           = [None,            dot,       Call      ],
        Dollar        = [None,            None,      None      ],
        DollarLBrace  = [None,            None,      None      ],
        Minus         = [unary,           binary,    Term      ],
        MinusEqual    = [None,            None,      None      ],
        Plus          = [None,            binary,    Term      ],
        PlusEqual     = [None,            None,      None      ],
        Pipe          = [None,            binary,    BitOr     ],
        PipeEqual     = [None,            None,      None      ],
        Percent       = [None,            binary,    Factor    ],
        PercentEqual  = [None,            None,      None      ],
        Amper         = [None,            binary,    BitAnd    ],
        AmperEqual    = [None,            None,      None      ],
        Hat           = [None,            binary,    BitXor    ],
        HatEqual      = [None,            None,      None      ],
        Semicolon     = [None,            None,      None      ],
        Slash         = [None,            binary,    Factor    ],
        SlashEqual    = [None,            None,      None      ],
        SlashSlash    = [None,            binary,    Factor    ],
        Star          = [None,            binary,    Factor    ],
        StarEqual     = [None,            None,      None      ],
        StarStar      = [None,            binary,    Exponent  ],
        Bang          = [unary,           None,      None      ],
        BangEqual     = [None,            binary,    Equality  ],
        Equal         = [None,            None,      None      ],
        EqualEqual    = [None,            binary,    Equality  ],
        Greater       = [None,            binary,    Comparison],
        GreaterEqual  = [None,            binary,    Comparison],
        Less          = [None,            binary,    Comparison],
        LessEqual     = [None,            binary,    Comparison],
        Identifier    = [variable,        None,      None      ],
        In            = [None,            binary,    In        ],
        String        = [string,          None,      None      ],
        StringPart    = [None,            None,      None      ],
        FStringStart  = [fstring,         None,      None      ],
        FStringEnd    = [None,            None,      None      ],
        Float         = [float,           None,      None      ],
        Integer       = [integer,         None,      None      ],
        And           = [None,            and,       And       ],
        Case          = [None,            None,      None      ],
        Class         = [None,            None,      None      ],
        Const         = [None,            None,      None      ],
        Continue      = [None,            None,      None      ],
        Break         = [None,            None,      None      ],
        Else          = [None,            None,      None      ],
        False         = [literal,         None,      None      ],
        For           = [None,            None,      None      ],
        Apostrophe    = [None,            None,      None      ],
        At            = [None,            None,      None      ],
        Fun           = [None,            None,      None      ],
        RightArrow    = [lambda,          None,      None      ],
        QuestionMark  = [None,            ternary,   Ternary   ],
        If            = [None,            None,      None      ],
        Unless        = [None,            None,      None      ],
        Nil           = [literal,         None,      None      ],
        Or            = [None,            or,        Or        ],
        Return        = [None,            None,      None      ],
        Switch        = [None,            None,      None      ],
        Super         = [super_,          None,      None      ],
        This          = [this,            None,      None      ],
        True          = [literal,         None,      None      ],
        Var           = [None,            None,      None      ],
        While         = [None,            None,      None      ],
        Until         = [None,            None,      None      ],
        From          = [None,            None,      None      ],
        Import        = [None,            None,      None      ],
        From          = [None,            None,      None      ],
        As            = [None,            None,      None      ],
        Error         = [None,            None,      None      ],
        Eof           = [None,            None,      None      ],
        Yield         = [None,            None,      None      ],
        Await         = [None,            None,      None      ],
        Async         = [None,            None,      None      ],
        StopIteration = [literal,         None,      None      ],
        Try           = [None,            None,      None      ],
        Catch         = [None,            None,      None      ],
        Finally       = [None,            None,      None      ],
        Throw         = [None,            None,      None      ],
        DotDotLess    = [None,            binary,    Range     ],
        DotDotEqual   = [None,            binary,    Range     ],
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
            let can_assign = precedence <= Precedence::Assignment;
            prefix_rule(self, can_assign, ignore_operators);
            while precedence
                <= self
                    .get_rule(self.current.as_ref().unwrap().kind)
                    .precedence
                && !ignore_operators.contains(&self.current.as_ref().unwrap().kind)
            {
                self.advance();
                let infix_rule = self
                    .get_rule(self.previous.as_ref().unwrap().kind)
                    .infix
                    .unwrap();
                infix_rule(self, can_assign, ignore_operators);
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
        let line = self.line();

        self.parse_precedence_ignoring(Precedence::Unary, ignore_operators);

        match operator {
            TK::Minus => self.emit_byte(OpCode::Negate, line),
            TK::Bang => self.emit_byte(OpCode::Not, line),
            _ => unreachable!("Unknown unary operator: {}", operator),
        }
    }

    /// For a binary operator, we need to parse the right operand and then emit the correct bytecode.
    /// The left operand is already on the stack.
    /// The final order on the stack will be that the right operand is on top of the left one.
    /// This is then handled correctly in the VM when the bytecode of a binary operator is encountered.
    fn binary(&mut self, _can_assign: bool, ignore_operators: &[TK]) {
        // First operand is already on the stack
        let operator = self.previous.as_ref().unwrap().kind;
        let line = self.line();
        let rule = self.get_rule(operator);

        // Correctly put the second operand on the stack
        self.parse_precedence_ignoring(
            Precedence::try_from_primitive(u8::from(rule.precedence) + 1).expect(
                "Invalid precedence in 'binary', should never be called for `Primary expression`.",
            ),
            ignore_operators,
        );

        // Emit the correct byte code to perform the operation on the two values
        match operator {
            TK::BangEqual => self.emit_byte(OpCode::NotEqual, line),
            TK::EqualEqual => self.emit_byte(OpCode::Equal, line),
            TK::Greater => self.emit_byte(OpCode::Greater, line),
            TK::GreaterEqual => self.emit_byte(OpCode::GreaterEqual, line),
            TK::Less => self.emit_byte(OpCode::Less, line),
            TK::LessEqual => self.emit_byte(OpCode::LessEqual, line),
            TK::Plus => self.emit_byte(OpCode::Add, line),
            TK::Minus => self.emit_byte(OpCode::Subtract, line),
            TK::Star => self.emit_byte(OpCode::Multiply, line),
            TK::Slash => self.emit_byte(OpCode::Divide, line),
            TK::Hat => self.emit_byte(OpCode::BitXor, line),
            TK::Pipe => self.emit_byte(OpCode::BitOr, line),
            TK::Amper => self.emit_byte(OpCode::BitAnd, line),
            TK::Percent => self.emit_byte(OpCode::Mod, line),
            TK::StarStar => self.emit_byte(OpCode::Exp, line),
            TK::SlashSlash => self.emit_byte(OpCode::FloorDiv, line),
            TK::In => self.in_(),
            TK::Colon => self.emit_byte(OpCode::BuildRational, line),
            //  Could think about making these one opcode with a boolean operand
            TK::DotDotEqual => self.emit_byte(OpCode::BuildRangeInclusive, line),
            TK::DotDotLess => self.emit_byte(OpCode::BuildRangeExclusive, line),
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
    fn ternary(&mut self, _can_assign: bool, ignore_operators: &[TK]) {
        let then_jump = self.emit_jump(OpCode::PopJumpIfFalse);

        // First value: We parse the "then" expression and jump over the "else" expression.
        // The condition has already been popped by PopJumpIfFalse.
        self.parse_precedence_ignoring(
            Precedence::Ternary,
            &[&[TK::Colon], ignore_operators].concat(),
        );
        let else_jump = self.emit_jump(OpCode::Jump);

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
    fn in_(&mut self) {
        let line = self.line();
        // Swap the order
        self.emit_byte(OpCode::Swap, line);
        self.invoke_fixed(&"contains", 1, "Too many constants created for OP_IN.");
    }

    /// Parsing any call just means parsing the arguments and then emitting the correct bytecode.
    fn call(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        let arg_count = self.argument_list();
        self.emit_bytes(OpCode::Call, arg_count, self.line());
    }

    /// Parse property access.
    ///
    /// This is actually fairly complicated, as cases like
    /// `a.b;`, `a.b = c;`, `a.b();` and `a.b() = c;` all have to be handled correctly here.
    fn dot(&mut self, can_assign: bool, _ignore_operators: &[TK]) {
        self.consume(TK::Identifier, "Expect property name after '.'.");
        let name_constant =
            self.identifier_constant(&self.previous.as_ref().unwrap().as_str().to_string());
        let line = self.line();
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
            if matches!(previous_kind, TK::Equal) {
                self.expression();
            } else {
                self.emit_byte(OpCode::Dup, line);
                self.emit_byte(OpCode::GetProperty, line);
                if !self.emit_number(name_constant.0, false) {
                    self.error("Too many constants created for OP_GET_PROPERTY.");
                }
                self.expression();
                match previous_kind {
                    TK::PlusEqual => self.emit_byte(OpCode::Add, line),
                    TK::MinusEqual => self.emit_byte(OpCode::Subtract, line),
                    TK::StarEqual => self.emit_byte(OpCode::Multiply, line),
                    TK::SlashEqual => self.emit_byte(OpCode::Divide, line),
                    TK::HatEqual => self.emit_byte(OpCode::BitXor, line),
                    TK::PipeEqual => self.emit_byte(OpCode::BitOr, line),
                    TK::AmperEqual => self.emit_byte(OpCode::BitAnd, line),
                    TK::PercentEqual => self.emit_byte(OpCode::Mod, line),
                    _ => unreachable!("Unexpected byte code "),
                }
            }
            self.emit_byte(OpCode::SetProperty, line);
            if !self.emit_number(name_constant.0, false) {
                self.error("Too many constants created for OP_SET_PROPERTY");
            }
        } else if self.match_(TK::LeftParen) {
            let arg_count = self.argument_list();
            self.emit_byte(OpCode::Invoke, line);
            if !self.emit_number(name_constant.0, false) {
                self.error("Too many constants created for OP_INVOKE");
            }
            self.emit_byte(arg_count, line);
        } else {
            self.emit_byte(OpCode::GetProperty, line);
            if !self.emit_number(name_constant.0, false) {
                self.error("Too many constants created for OP_GET_PROPERTY.");
            }
        }
    }

    /// Handles the four tokens that directly corresponds to values.
    fn literal(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        let literal = self.previous.as_ref().unwrap().kind;
        match literal {
            TK::False => self.emit_byte(OpCode::False, self.line()),
            TK::Nil => self.emit_byte(OpCode::Nil, self.line()),
            TK::StopIteration => self.emit_byte(OpCode::StopIteration, self.line()),
            TK::True => self.emit_byte(OpCode::True, self.line()),
            _ => unreachable!("Unknown literal: {}", literal),
        }
    }

    // TODO: Make this also create tuples?
    /// Used for grouping expressions to overwrite default precedence.
    ///
    /// The full expression within the grouping will be parsed as one.
    fn grouping(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        if self.match_(TK::RightParen) {
            self.emit_bytes(OpCode::BuildTuple, 0, self.line());
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
        let value: f64 = self.previous.as_ref().unwrap().as_str().parse().unwrap();
        self.emit_constant(value);
    }

    /// Emit an integer literal.
    ///
    /// Works equivalent to [`Compiler::float`].
    fn integer(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        let integer_str = self.previous.as_ref().unwrap().as_str();
        if let Ok(value) = integer_str.parse::<i64>() {
            self.emit_constant(value);
        } else {
            let bigint_id = self
                .heap
                .add_big_int(integer_str.parse::<BigInt>().unwrap());
            self.emit_constant(bigint_id);
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
        self.emit_constant(string_id);
    }

    /// Parse an f-string literal with interpolation support
    fn fstring(&mut self, _can_assign: bool, _ignore_operators: &[TK]) {
        let mut part_count = 0u8;
        
        // Process tokens until we reach FStringEnd
        loop {
            // Check what the current token is
            if self.check(TK::FStringEnd) {
                self.advance(); // consume FStringEnd
                break;
            }
            
            if self.check(TK::StringPart) {
                // Handle string literal part
                let lexeme = self.current.as_ref().unwrap().as_str();
                let value = self.unescape_fstring_content(lexeme);
                let string_id = self.heap.string_id(&value);
                self.emit_constant(string_id);
                part_count += 1;
                self.advance();
            } else if self.match_(TK::DollarLBrace) {
                // Start of interpolation ${expr}
                
                // Parse the expression
                self.expression();
                
                // For now, let the VM handle string conversion automatically
                // TODO: Later we can emit explicit str() calls as globals if needed
                
                part_count += 1;
                
                if !self.match_(TK::RightBrace) {
                    self.error("Expected '}' after f-string expression.");
                    return;
                }
            } else {
                // Unexpected token
                self.error_at_current("Unexpected token in f-string.");
                return;
            }
        }

        // Emit BuildFString instruction with part count
        self.emit_buildfstring(part_count);
    }

    /// Unescape f-string content (handle \\$ sequences)
    fn unescape_fstring_content(&self, content: &str) -> String {
        let mut result = String::new();
        let mut chars = content.chars();
        
        while let Some(ch) = chars.next() {
            if ch == '\\' {
                if let Some(next_ch) = chars.next() {
                    match next_ch {
                        '$' => result.push('$'), // \$ -> $
                        '\\' => {
                            // If this is the last \\, and we're at the end, convert to $
                            // This handles the case where \\$ is split with \\ in content and $ in DollarLBrace token
                            if chars.as_str().is_empty() {
                                result.push('$'); // \\ at end -> $ (assumes followed by ${})
                            } else {
                                result.push('\\'); // \\ -> \
                            }
                        }
                        _ => {
                            result.push('\\');
                            result.push(next_ch);
                        }
                    }
                } else {
                    result.push('\\');
                }
            } else {
                result.push(ch);
            }
        }
        
        result
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
        self.emit_bytes(OpCode::BuildList, item_count, self.line());
    }

    /// Parse a tuple literal 'a, b, c(,)'
    ///
    /// Handles optional trailing commas.
    /// Empty tuples are '()' and parsed by [`Compiler::grouping`].
    /// This only does tuples with at least one element.
    fn tuple(&mut self, _can_assign: bool, ignore_operators: &[TK]) {
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

        self.emit_bytes(OpCode::BuildTuple, item_count, self.line());
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
            return self.finish_hash_collection(false, 0);
        }

        // Empty dict literal {:}
        if self.match_(TK::Colon) {
            return self.finish_hash_collection(true, 0);
        }

        // First element
        let mut item_count = 0;
        self.parse_precedence_ignoring(Precedence::non_assigning(), &[TK::Colon, TK::Comma]);
        let is_dict = self.match_(TK::Colon);
        if is_dict {
            self.parse_precedence_ignoring(Precedence::non_assigning(), &[TK::Colon, TK::Comma]);
        }
        item_count += 1;
        self.match_(TK::Comma); // optional trailing comma after first

        // Remaining elements
        while !self.check(TK::RightBrace) {
            if is_dict {
                self.parse_dict_entry();
            } else {
                self.parse_precedence_ignoring(
                    Precedence::non_assigning(),
                    &[TK::Colon, TK::Comma],
                );
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

        self.finish_hash_collection(is_dict, item_count);
    }

    /// Helper function to consume the closing brace and emit the appropriate
    /// collection creation opcode (`BuildDict` or `BuildSet`) for dict or set literals.
    fn finish_hash_collection(&mut self, is_dict: bool, item_count: u8) {
        self.consume(
            TK::RightBrace,
            &format!(
                "Expect '}}' after {} literal.",
                if is_dict { "dict" } else { "set" }
            ),
        );
        self.emit_bytes(
            if is_dict {
                OpCode::BuildDict
            } else {
                OpCode::BuildSet
            },
            item_count,
            self.line(),
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
    fn subscript(&mut self, can_assign: bool, _ignore_operators: &[TK]) {
        self.parse_precedence(Precedence::non_assigning());
        self.consume(TK::RightBracket, "Expect ']' after index.");
        let line = self.line();
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
            if matches!(previous_kind, TK::Equal) {
                self.expression();
            } else {
                self.emit_bytes(OpCode::DupN, 2, line);
                self.invoke_fixed(
                    &"__getitem__",
                    1,
                    "Too many constants created for OP_SUBSCRIPT.",
                );
                self.expression();
                match previous_kind {
                    TK::PlusEqual => self.emit_byte(OpCode::Add, line),
                    TK::MinusEqual => self.emit_byte(OpCode::Subtract, line),
                    TK::StarEqual => self.emit_byte(OpCode::Multiply, line),
                    TK::SlashEqual => self.emit_byte(OpCode::Divide, line),
                    TK::HatEqual => self.emit_byte(OpCode::BitXor, line),
                    TK::PipeEqual => self.emit_byte(OpCode::BitOr, line),
                    TK::AmperEqual => self.emit_byte(OpCode::BitAnd, line),
                    TK::PercentEqual => self.emit_byte(OpCode::Mod, line),
                    _ => unreachable!("Unexpected byte code "),
                }
            }
            self.invoke_fixed(
                &"__setitem__",
                2,
                "Too many constants created for OP_SUBSCRIPT.",
            );
        } else {
            self.invoke_fixed(
                &"__getitem__",
                1,
                "Too many constants created for OP_SUBSCRIPT.",
            );
        }
    }

    /// Short circuiting `and`.
    ///
    /// The result of such an expression is the first operand that evaluates
    /// falsey or the last operand if all are truthy.
    /// The second expression is not evaluated if the first is already false.
    fn and(&mut self, _can_assign: bool, ignore_operators: &[TK]) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalseOrPop);
        self.parse_precedence_ignoring(Precedence::And, ignore_operators);
        self.patch_jump(end_jump);
    }

    /// Short circuiting `or`.
    ///
    /// Work equivalently to [`Compiler::and`].
    fn or(&mut self, _can_assign: bool, ignore_operators: &[TK]) {
        let end_jump = self.emit_jump(OpCode::JumpIfTrueOrPop);
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
        self.consume(TK::Dot, "Expect '.' after 'super'.");
        self.consume(TK::Identifier, "Expect superclass method name.");
        let name = self.identifier_constant(&self.previous.as_ref().unwrap().as_str().to_string());

        let line = self.line();

        self.named_variable(&self.synthetic_token(TK::This).as_str(), false);
        if self.match_(TK::LeftParen) {
            let arg_count = self.argument_list();
            self.named_variable(&self.synthetic_token(TK::Super).as_str(), false);
            self.emit_byte(OpCode::SuperInvoke, line);
            if !self.emit_number(*name, false) {
                self.error("Too many constants while compiling OP_SUPER_INVOKE");
            }
            self.emit_byte(arg_count, line);
        } else {
            self.named_variable(&self.synthetic_token(TK::Super).as_str(), false);
            self.emit_byte(OpCode::GetSuper, self.line());
            if !self.emit_number(*name, false) {
                self.error("Too many constants while compiling OP_SUPER_INVOKE");
            }
        }
    }

    /// Helper function to deal with operators that delegate to overloaded methods.
    fn invoke_fixed<S: ToString>(&mut self, name: &S, arg_count: u8, error_message: &str) {
        let name_constant = self.identifier_constant(name);
        let line = self.line();
        self.emit_byte(OpCode::Invoke, line);
        if !self.emit_number(name_constant.0, false) {
            self.error(error_message);
        }
        self.emit_byte(arg_count, line);
    }
}
