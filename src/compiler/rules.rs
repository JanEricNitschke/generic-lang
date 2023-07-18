use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::chunk::OpCode;
use crate::scanner::TokenKind as TK;

use super::Compiler;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub(super) enum Precedence {
    None,
    Assignment, // =
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

type ParseFn<'scanner, 'arena> = fn(&mut Compiler<'scanner, 'arena>, bool) -> ();

#[derive(Clone)]
pub(super) struct Rule<'scanner, 'arena> {
    prefix: Option<ParseFn<'scanner, 'arena>>,
    infix: Option<ParseFn<'scanner, 'arena>>,
    precedence: Precedence,
}

impl<'scanner, 'arena> Default for Rule<'scanner, 'arena> {
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
        // If the tokens get input into the makro in the same order
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

pub(super) type Rules<'scanner, 'arena> = [Rule<'scanner, 'arena>; 66];

// Can't be static because the associated function types include lifetimes
#[rustfmt::skip]
pub(super) fn make_rules<'scanner, 'arena>() -> Rules<'scanner, 'arena> {
    make_rules!(
        LeftParen    = [grouping, call,      Call      ],
        RightParen   = [None,     None,      None      ],
        LeftBrace    = [None,     None,      None      ],
        RightBrace   = [None,     None,      None      ],
        Colon        = [None,     None,      None      ],
        LeftBracket  = [list,     subscript, Call      ],
        RightBracket = [None,     None,      None      ],
        Comma        = [None,     None,      None      ],
        Default      = [None,     None,      None      ],
        Dot          = [None,     dot,       Call      ],
        Minus        = [unary,    binary,    Term      ],
        MinusEqual   = [None,     None,      None      ], // TODO
        Plus         = [None,     binary,    Term      ],
        PlusEqual    = [None,     None,      None      ], // TODO
        Pipe         = [None,     binary,    BitOr     ],
        PipeEqual    = [None,     None,      None      ], // TODO
        Percent      = [None,     binary,    Factor    ],
        PercentEqual = [None,     None,      None      ], // TODO
        Amper        = [None,     binary,    BitAnd    ],
        AmperEqual   = [None,     None,      None      ], // TODO
        Hat          = [None,     binary,    BitXor    ],
        HatEqual     = [None,     None,      None      ], // TODO
        Semicolon    = [None,     None,      None      ],
        Slash        = [None,     binary,    Factor    ],
        SlashEqual   = [None,     None,      None      ], // TODO
        SlashSlash   = [None,     binary,    Factor    ],
        Star         = [None,     binary,    Factor    ],
        StarEqual    = [None,     None,      None      ], // TODO
        StarStar     = [None,     binary,    Exponent  ],
        Bang         = [unary,    None,      None      ],
        BangEqual    = [None,     binary,    Equality  ],
        Equal        = [None,     None,      None      ],
        EqualEqual   = [None,     binary,    Equality  ],
        Greater      = [None,     binary,    Comparison],
        GreaterEqual = [None,     binary,    Comparison],
        Less         = [None,     binary,    Comparison],
        LessEqual    = [None,     binary,    Comparison],
        Identifier   = [variable, None,      None      ],
        In           = [None,     None,      None      ], // TODO
        String       = [string,   None,      None      ],
        Number       = [number,   None,      None      ],
        Integer      = [integer,  None,      None      ],
        And          = [None,     and,       And       ],
        Case         = [None,     None,      None      ],
        Class        = [None,     None,      None      ],
        Const        = [None,     None,      None      ],
        Continue     = [None,     None,      None      ],
        Break        = [None,     None,      None      ],
        Else         = [None,     None,      None      ],
        False        = [literal,  None,      None      ],
        For          = [None,     None,      None      ],
        Fun          = [None,     None,      None      ],
        If           = [None,     None,      None      ],
        Unless       = [None,     None,      None      ],
        Nil          = [literal,  None,      None      ],
        Or           = [None,     or,        Or        ],
        Return       = [None,     None,      None      ],
        Switch       = [None,     None,      None      ],
        Super        = [super_,   None,      None      ],
        This         = [this,     None,      None      ],
        True         = [literal,  None,      None      ],
        Var          = [None,     None,      None      ],
        While        = [None,     None,      None      ],
        Until        = [None,     None,      None      ],
        Error        = [None,     None,      None      ],
        Eof          = [None,     None,      None      ],
    )
}

impl<'scanner, 'arena> Compiler<'scanner, 'arena> {
    const fn get_rule(&self, operator: TK) -> &Rule<'scanner, 'arena> {
        &self.rules[operator as usize]
    }

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

    fn unary(&mut self, _can_assign: bool) {
        let operator = self.previous.as_ref().unwrap().kind;
        let line = self.line();

        self.parse_precedence(Precedence::Unary);

        match operator {
            TK::Minus => self.emit_byte(OpCode::Negate, line),
            TK::Bang => self.emit_byte(OpCode::Not, line),
            _ => unreachable!("Unkown unary operator: {}", operator),
        }
    }

    fn binary(&mut self, _can_assign: bool) {
        let operator = self.previous.as_ref().unwrap().kind;
        let line = self.line();
        let rule = self.get_rule(operator);

        self.parse_precedence(
            Precedence::try_from_primitive(u8::from(rule.precedence) + 1).unwrap(),
        );

        match operator {
            TK::BangEqual => self.emit_bytes(OpCode::Equal, OpCode::Not, line),
            TK::EqualEqual => self.emit_byte(OpCode::Equal, line),
            TK::Greater => self.emit_byte(OpCode::Greater, line),
            TK::GreaterEqual => self.emit_bytes(OpCode::Less, OpCode::Not, line),
            TK::Less => self.emit_byte(OpCode::Less, line),
            TK::LessEqual => self.emit_bytes(OpCode::Greater, OpCode::Not, line),
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
            _ => unreachable!("Unkown binary operator: {}", operator),
        }
    }

    fn call(&mut self, _can_assign: bool) {
        let arg_count = self.argument_list();
        self.emit_bytes(OpCode::Call, arg_count, self.line());
    }

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

    fn literal(&mut self, _can_assign: bool) {
        let literal = self.previous.as_ref().unwrap().kind;
        match literal {
            TK::False => self.emit_byte(OpCode::False, self.line()),
            TK::Nil => self.emit_byte(OpCode::Nil, self.line()),
            TK::True => self.emit_byte(OpCode::True, self.line()),
            _ => unreachable!("Unkown literal: {}", literal),
        }
    }

    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(TK::RightParen, "Expect ')' after expression.");
    }

    fn number(&mut self, _can_assign: bool) {
        let value: f64 = self.previous.as_ref().unwrap().as_str().parse().unwrap();
        self.emit_constant(value);
    }

    fn integer(&mut self, _can_assign: bool) {
        let value: i64 = self.previous.as_ref().unwrap().as_str().parse().unwrap();
        self.emit_constant(value);
    }

    fn string(&mut self, _can_assign: bool) {
        let lexeme = self.previous.as_ref().unwrap().as_str();
        let value = lexeme[1..lexeme.len() - 1].to_string();
        let string_id = self.heap.string_id(&value);
        self.emit_constant(string_id);
    }

    fn list(&mut self, _can_assign: bool) {
        let mut item_count = 0;
        if !self.check(TK::RightBracket) {
            loop {
                if self.check(TK::RightBracket) {
                    // Trailing comma
                    break;
                }
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
        }
        self.consume(TK::RightBracket, "Expect ']' after list literal.");
        self.emit_bytes(OpCode::BuildList, item_count, self.line());
    }

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
                self.emit_byte(OpCode::IndexSubscript, line);
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
            self.emit_byte(OpCode::StoreSubscript, line);
        } else {
            self.emit_byte(OpCode::IndexSubscript, line);
        }
    }

    fn and(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop, self.line());
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn or(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(OpCode::JumpIfTrue);
        self.emit_byte(OpCode::Pop, self.line());
        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn this(&mut self, _can_assign: bool) {
        if self.current_class().is_none() {
            self.error("Can't use 'this' outside of a class.");
            return;
        }
        self.variable(false);
    }

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
}
