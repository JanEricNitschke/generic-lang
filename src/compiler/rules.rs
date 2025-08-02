//! Parser to expressions while respecting operator precedence.
//!
//! Uses Vaughan Pratt's "top-down operator precedence parsing".

use num_bigint::BigInt;
use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::chunk::OpCode;
use crate::scanner::TokenKind as TK;

use super::Compiler;

// The precedence of the different operators in the language
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub(super) enum Precedence {
    None,
    Assignment, // =
    In,         // in
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    BitOr,      // |
    BitXor,     // ^
    BitAnd,     // &
    Term,       // + -
    Factor,     // * / // %
    Unary,      // ! -
    Exponent,   // **
    Call,       // . () []
    Primary,
}

// Typedef for the functions that parse the different types of expressions
type ParseFn<'scanner, 'arena> = fn(&mut Compiler<'scanner, 'arena>, bool) -> ();

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

pub(super) type Rules<'scanner, 'arena> = [Rule<'scanner, 'arena>; 75];

// Can't be static because the associated function types include lifetimes
#[rustfmt::skip]
pub(super) fn make_rules<'scanner, 'arena>() -> Rules<'scanner, 'arena> {
    make_rules!(
        LeftParen     = [grouping,        call,      Call      ],
        RightParen    = [None,            None,      None      ],
        LeftBrace     = [hash_collection, None,      None      ],
        RightBrace    = [None,            None,      None      ],
        Colon         = [None,            None,      None      ],
        LeftBracket   = [list,            subscript, Call      ],
        RightBracket  = [None,            None,      None      ],
        Comma         = [None,            None,      None      ],
        Default       = [None,            None,      None      ],
        Dot           = [None,            dot,       Call      ],
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
        At            = [None,            None,      None      ],
        Fun           = [None,            None,      None      ],
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
    pub(super) fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        if let Some(prefix_rule) = self.get_rule(self.previous.as_ref().unwrap().kind).prefix {
            let can_assign = precedence <= Precedence::Assignment;
            prefix_rule(self, can_assign);
            while precedence
                <= self
                    .get_rule(self.current.as_ref().unwrap().kind)
                    .precedence
            {
                self.advance();
                let infix_rule = self
                    .get_rule(self.previous.as_ref().unwrap().kind)
                    .infix
                    .unwrap();
                infix_rule(self, can_assign);
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
        } else {
            self.error("Expect expression.");
        }
    }

    /// Parse the expression which will leave its value on the stack.
    /// Then emit the bytecode for the respective operation which will act on the value on the stack.
    fn unary(&mut self, _can_assign: bool) {
        let operator = self.previous.as_ref().unwrap().kind;
        let line = self.line();

        self.parse_precedence(Precedence::Unary);

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
    fn binary(&mut self, _can_assign: bool) {
        // First operand is already on the stack
        let operator = self.previous.as_ref().unwrap().kind;
        let line = self.line();
        let rule = self.get_rule(operator);

        // Correctly put the second operand on the stack
        self.parse_precedence(
            Precedence::try_from_primitive(u8::from(rule.precedence) + 1).expect(
                "Invalid precedence in 'binary', should never be called for `Primary expression`.",
            ),
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
            _ => unreachable!("Unknown binary operator: {}", operator),
        }
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
    fn call(&mut self, _can_assign: bool) {
        let arg_count = self.argument_list();
        self.emit_bytes(OpCode::Call, arg_count, self.line());
    }

    /// Parse property access.
    ///
    /// This is actually fairly complicated, as cases like
    /// `a.b;`, `a.b = c;`, `a.b();` and `a.b() = c;` all have to be handled correctly here.
    fn dot(&mut self, can_assign: bool) {
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
    fn literal(&mut self, _can_assign: bool) {
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
    fn grouping(&mut self, _can_assign: bool) {
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
    fn float(&mut self, _can_assign: bool) {
        let value: f64 = self.previous.as_ref().unwrap().as_str().parse().unwrap();
        self.emit_constant(value);
    }

    /// Emit an integer literal.
    ///
    /// Works equivalent to [`Compiler::float`].
    fn integer(&mut self, _can_assign: bool) {
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
    fn string(&mut self, _can_assign: bool) {
        let lexeme = self.previous.as_ref().unwrap().as_str();
        let value = lexeme[1..lexeme.len() - 1].to_string();
        let string_id = self.heap.string_id(&value);
        self.emit_constant(string_id);
    }

    /// Parse a list literal ([a, b, c(,)])
    ///
    /// Handles optional trailing commas.
    fn list(&mut self, _can_assign: bool) {
        let mut item_count = 0;
        // Handle trailing comma
        while !self.check(TK::RightBracket) {
            // No assignments
            self.parse_precedence(Precedence::Or);
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

    /// Handle the hashed collection `set`and `dict``.`
    ///
    /// Sets effectively work equivalent to [`Compiler::list`], except
    /// that curly braces are used instead of square ones.
    /// Dicts work similar, instead that each entry is of the form:
    /// `key: value`. Whether the code is parsed as set or dict is determined
    /// by the presence of a colon after the first token.
    ///
    /// Another noteworthy point is that empty braces are parsed as a dict.
    fn hash_collection(&mut self, _can_assign: bool) {
        let mut item_count = 0;
        let mut is_dict = true;

        if !self.check(TK::RightBrace) {
            self.parse_precedence(Precedence::Or);
            is_dict = self.match_(TK::Colon);

            if is_dict {
                self.parse_precedence(Precedence::Or);
            }
            item_count += 1;
            // Handle potential trailing comma after (first/only) element
            self.match_(TK::Comma);

            while !self.check(TK::RightBrace) {
                if is_dict {
                    self.parse_dict_entry();
                } else {
                    self.parse_precedence(Precedence::Or);
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
        }
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
        self.parse_precedence(Precedence::Or);
        self.consume(TK::Colon, "Expect ':' after key.");
        self.parse_precedence(Precedence::Or);
    }

    /// Parse subscript (`a[b]`) expressions.
    ///
    /// Implemented similar to [`Compiler::dot`] to handle getting
    /// and setting.
    ///
    /// Also works by invoking `__getitem__` or `__setitem__`
    /// on the value to allow classes to overload how this operation works.
    fn subscript(&mut self, can_assign: bool) {
        self.parse_precedence(Precedence::Or);
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
    fn and(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop, self.line());
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    /// Short circuiting `or`.
    ///
    /// Work equivalently to [`Compiler::and`].
    fn or(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(OpCode::JumpIfTrue);
        self.emit_byte(OpCode::Pop, self.line());
        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    /// Handle `this`.
    ///
    /// If inside a class this simply works on the local variable
    /// `this`which is initialized to the specific instance.
    ///
    /// Outside of a class context this is a syntax error.
    fn this(&mut self, _can_assign: bool) {
        if self.current_class().is_none() {
            self.error("Can't use 'this' outside of a class.");
            return;
        }
        self.variable(false);
    }

    /// Handle `super` expressions that interact with the superlcass.
    ///
    /// Like `this`, `super` also only work when used inside class.
    /// Additionally, the class is required to have a superclass.
    /// Both are checked statically at compile time.
    ///
    /// Unlike `this`, only method access either in the form of a call
    /// or to create a bound method is possible.
    fn super_(&mut self, _can_assign: bool) {
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
