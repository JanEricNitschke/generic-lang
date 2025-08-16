//! Defines the tokens and scanner that handles the transforming or the source to tokens.

use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::types::Line;

/// `Token` types that exist in the generic language.
#[derive(IntoPrimitive, TryFromPrimitive, PartialEq, Eq, Clone, Copy, Debug)]
#[repr(u8)]
pub enum TokenKind {
    // Character Tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    Colon,
    Comma,
    Dot,

    QuestionMark,

    Minus,
    MinusEqual,
    Plus,
    PlusEqual,
    Semicolon,
    Slash,
    SlashEqual,
    Star,
    StarEqual,
    Percent,
    PercentEqual,
    Pipe,
    PipeEqual,
    Amper,
    AmperEqual,
    Hat,
    HatEqual,
    StarStar,
    SlashSlash,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Float,
    Integer,
    False,
    True,
    Nil,
    StopIteration,

    // F-string tokens.
    FStringStart,
    FStringPart,
    FStringEnd,

    // Keywords.
    And,
    Or,
    If,
    Unless,
    Else,
    For,
    ForEach,
    While,
    Apostrophe,
    Until,
    Continue,
    Break,
    Switch,
    Default,
    Case,

    In,

    Const,
    Var,

    Class,
    This,
    Super,

    At,
    Fun,
    RightArrow,
    Return,

    Error,
    Eof,

    Import,
    From,
    As,

    Yield,
    Async,
    Await,

    Try,
    Catch,
    Finally,
    Throw,

    DotDotLess,
    DotDotEqual,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!("{self:?}"))
    }
}

/// Actual tokens emitted by the scanner.
///
/// Contains the `TokenKind` that it represents
/// together with the raw characters that comprise it
/// and the line that it originates from.
#[derive(Clone, Debug)]
pub struct Token<'a> {
    pub(super) kind: TokenKind,
    pub(super) lexeme: &'a [u8],
    pub(super) line: Line,
}

impl<'a> Token<'a> {
    pub(super) fn as_str(&'a self) -> &'a str {
        std::str::from_utf8(self.lexeme).unwrap()
    }
}

/// Main struct for parsing the source characters to tokens.
#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    source: &'a [u8],
    start: usize,
    /// Always points at the next character to be consumed.
    current: usize,
    line: Line,
    /// Tracks if we're inside an f-string and the nesting depth of braces
    fstring_brace_depth: Option<usize>,
}

impl<'a> Scanner<'a> {
    #[must_use]
    pub(super) const fn new(source: &'a [u8]) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: Line(1),
            fstring_brace_depth: None,
        }
    }

    /// Main scan that turns raw characters to tokens.
    ///
    /// Every call to this function parses enough of the source file
    /// to return exactly one token.
    ///
    /// Uses a trie strategy to identify tokens.
    #[allow(clippy::too_many_lines)]
    pub(super) fn scan(&mut self) -> Token<'a> {
        use TokenKind as TK;
        self.skip_whitespace();
        self.start = self.current;
        let token_kind = match self.advance() {
            None => TK::Eof,
            Some(c) => match c {
                b'\'' => TK::Apostrophe,
                b'?' => TK::QuestionMark,
                b':' => TK::Colon,
                b'(' => TK::LeftParen,
                b')' => TK::RightParen,
                b'{' => {
                    if let Some(depth) = self.fstring_brace_depth.as_mut() {
                        *depth += 1;
                    }
                    TK::LeftBrace
                }
                b'}' => {
                    if let Some(depth) = self.fstring_brace_depth.as_mut() {
                        if *depth > 0 {
                            *depth -= 1;
                        } else {
                            // This closes an interpolation expression, return to f-string mode
                            return self.fstring_part();
                        }
                    }
                    TK::RightBrace
                }
                b'[' => TK::LeftBracket,
                b']' => TK::RightBracket,
                b';' => TK::Semicolon,
                b',' => TK::Comma,
                b'.' => {
                    // Check for range operators ..= and ..<
                    if self.match_(b'.') {
                        if self.match_(b'=') {
                            TK::DotDotEqual
                        } else if self.match_(b'<') {
                            TK::DotDotLess
                        } else {
                            // This is an error - we've consumed two dots but can't make a valid token
                            return self
                                .error_token("Invalid token '..' - expected '..=' or '..<'");
                        }
                    } else {
                        TK::Dot
                    }
                }
                b'@' => TK::At,
                b'-' => {
                    if self.match_(b'=') {
                        TK::MinusEqual
                    } else if self.match_(b'>') {
                        TK::RightArrow
                    } else {
                        TK::Minus
                    }
                }
                b'+' => {
                    if self.match_(b'=') {
                        TK::PlusEqual
                    } else {
                        TK::Plus
                    }
                }
                b'/' => {
                    if self.match_(b'=') {
                        TK::SlashEqual
                    } else if self.match_(b'/') {
                        TK::SlashSlash
                    } else {
                        TK::Slash
                    }
                }
                b'*' => {
                    if self.match_(b'=') {
                        TK::StarEqual
                    } else if self.match_(b'*') {
                        TK::StarStar
                    } else {
                        TK::Star
                    }
                }
                b'%' => {
                    if self.match_(b'=') {
                        TK::PercentEqual
                    } else {
                        TK::Percent
                    }
                }
                b'&' => {
                    if self.match_(b'=') {
                        TK::AmperEqual
                    } else {
                        TK::Amper
                    }
                }
                b'|' => {
                    if self.match_(b'=') {
                        TK::PipeEqual
                    } else {
                        TK::Pipe
                    }
                }
                b'^' => {
                    if self.match_(b'=') {
                        TK::HatEqual
                    } else {
                        TK::Hat
                    }
                }
                b'!' => {
                    if self.match_(b'=') {
                        TK::BangEqual
                    } else {
                        TK::Bang
                    }
                }
                b'=' => {
                    if self.match_(b'=') {
                        TK::EqualEqual
                    } else {
                        TK::Equal
                    }
                }
                b'<' => {
                    if self.match_(b'=') {
                        TK::LessEqual
                    } else {
                        TK::Less
                    }
                }
                b'>' => {
                    if self.match_(b'=') {
                        TK::GreaterEqual
                    } else {
                        TK::Greater
                    }
                }
                b'"' => {
                    if self.fstring_brace_depth.is_some() {
                        // This should be the end of an f-string, but let fstring_part handle it
                        return self.fstring_part();
                    } else {
                        return self.string();
                    }
                }
                b'f' => {
                    // Check if this is an f-string (f followed by ")
                    if self.peek() == Some(&b'"') {
                        return self.fstring_start();
                    } else {
                        return self.identifier();
                    }
                }
                c if c.is_ascii_digit() => return self.number(),
                c if c.is_ascii_alphabetic() || c == &b'_' => return self.identifier(),
                _ => return self.error_token("Unexpected character."),
            },
        };
        self.make_token(token_kind)
    }

    fn advance(&mut self) -> Option<&u8> {
        self.current += 1;
        self.source.get(self.current - 1)
    }

    fn match_(&mut self, expected: u8) -> bool {
        match self.source.get(self.current) {
            Some(actual) if actual == &expected => {
                self.current += 1;
                true
            }
            _ => false,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                Some(b' ' | b'\r' | b'\t') => {
                    self.advance();
                }
                Some(b'\n') => {
                    *self.line += 1;
                    self.advance();
                }
                Some(b'#') => {
                    while !matches!(self.peek(), Some(b'\n') | None) {
                        self.advance();
                    }
                }
                _ => break,
            }
        }
    }

    /// Strings are sequences of any characters starting and ending
    /// with `"`. Strings can span multiple lines.
    fn string(&mut self) -> Token<'a> {
        while self.peek().is_some_and(|c| c != &b'"') {
            if self.peek() == Some(&b'\n') {
                *self.line += 1;
            }
            self.advance();
        }

        if !self.match_(b'"') {
            return self.error_token("Unterminated string.");
        }

        self.make_token(TokenKind::String)
    }

    /// Parse the start of an f-string (f").
    /// Consumes the 'f"' and enters f-string mode.
    fn fstring_start(&mut self) -> Token<'a> {
        // Skip the opening quote (we already consumed 'f')
        self.advance(); // consume the '"'
        self.fstring_brace_depth = Some(0);
        
        // Immediately start parsing the first string part
        self.fstring_part()
    }

    /// Parse a string part inside an f-string until we hit ${ or ".
    /// Handles escaping of \$ and \{.
    fn fstring_part(&mut self) -> Token<'a> {
        // Mark the start of this string part
        self.start = self.current;
        
        while let Some(c) = self.peek() {
            match c {
                b'"' => {
                    // End of f-string - this string part is done
                    break;
                }
                b'$' => {
                    if self.peek_next() == Some(&b'{') {
                        // Start of interpolation - this string part is done
                        break;
                    } else {
                        self.advance();
                    }
                }
                b'\\' => {
                    // Handle escaping
                    self.advance(); // consume backslash
                    if let Some(_escaped) = self.peek() {
                        self.advance(); // consume escaped character
                    }
                }
                b'\n' => {
                    *self.line += 1;
                    self.advance();
                }
                _ => {
                    self.advance();
                }
            }
        }
        
        // Check what ended the string part
        if self.peek() == Some(&b'$') && self.peek_next() == Some(&b'{') {
            // Next will be an expression - if this part is empty, handle ${ now
            if self.start == self.current {
                self.advance(); // consume '$'
                self.advance(); // consume '{'
                // Reset f-string mode to allow normal expression parsing
                self.fstring_brace_depth = Some(0);
                // Continue scanning normally for the expression
                self.scan()
            } else {
                // Return the string part we accumulated
                self.make_token(TokenKind::FStringPart)
            }
        } else if self.peek() == Some(&b'"') {
            // End of f-string
            if self.start == self.current {
                // Empty part before closing quote - return FStringEnd
                self.fstring_end()
            } else {
                // Return the final string part
                self.make_token(TokenKind::FStringPart)
            }
        } else {
            // Should not happen - unterminated f-string
            self.error_token("Unterminated f-string.")
        }
    }

    /// Parse the end of an f-string (").
    /// Exits f-string mode.
    fn fstring_end(&mut self) -> Token<'a> {
        self.advance(); // consume the '"'
        self.fstring_brace_depth = None;
        self.make_token(TokenKind::FStringEnd)
    }

    /// Numbers are any sequence of ascii digits with an optional decimal point in the middle.
    ///
    /// Decimal points at the end are not supported and neither is scientific notation.
    fn number(&mut self) -> Token<'a> {
        while self.peek().is_some_and(u8::is_ascii_digit) {
            self.advance();
        }

        // Fractions
        if self.peek() == Some(&b'.') && self.peek_next().is_some_and(u8::is_ascii_digit) {
            self.advance();
            while self.peek().is_some_and(u8::is_ascii_digit) {
                self.advance();
            }
        } else {
            return self.make_token(TokenKind::Integer);
        }

        self.make_token(TokenKind::Float)
    }

    /// Identifiers can contain alphanumeric characters and underscores.
    ///
    /// Although they have to start with an underscore or alphabetic character.
    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn is_identifier_char(c: &u8) -> bool {
        c.is_ascii_alphanumeric() || c == &b'_'
    }

    fn identifier(&mut self) -> Token<'a> {
        while self.peek().is_some_and(Self::is_identifier_char) {
            self.advance();
        }
        let token_kind = self.identifier_type();
        self.make_token(token_kind)
    }

    /// Parse identifiers using a `trie` strategy.
    fn identifier_type(&self) -> TokenKind {
        match self.source[self.start] {
            b'a' => match self.source.get(self.start + 1) {
                Some(b'n') => self.check_keyword(2, "d", TokenKind::And),
                Some(b's') => match self.check_keyword(2, "", TokenKind::As) {
                    TokenKind::As => TokenKind::As,
                    _ => match self.source.get(self.start + 2) {
                        Some(b'y') => self.check_keyword(3, "nc", TokenKind::Async),
                        _ => TokenKind::Identifier,
                    },
                },
                Some(b'w') => self.check_keyword(2, "ait", TokenKind::Await),
                _ => TokenKind::Identifier,
            },
            b'b' => self.check_keyword(1, "reak", TokenKind::Break),
            b'c' => match self.source.get(self.start + 1) {
                Some(b'a') => match self.source.get(self.start + 2) {
                    Some(b's') => self.check_keyword(3, "e", TokenKind::Case),
                    Some(b't') => self.check_keyword(3, "ch", TokenKind::Catch),
                    _ => TokenKind::Identifier,
                },
                Some(b'l') => self.check_keyword(2, "ass", TokenKind::Class),
                Some(b'o') => match self.source.get(self.start + 2) {
                    Some(b'n') => match self.source.get(self.start + 3) {
                        Some(b's') => self.check_keyword(4, "t", TokenKind::Const),
                        Some(b't') => self.check_keyword(4, "inue", TokenKind::Continue),
                        _ => TokenKind::Identifier,
                    },
                    _ => TokenKind::Identifier,
                },
                _ => TokenKind::Identifier,
            },
            b'd' => self.check_keyword(1, "efault", TokenKind::Default),
            b'e' => self.check_keyword(1, "lse", TokenKind::Else),
            b'f' => match self.source.get(self.start + 1) {
                Some(b'a') => self.check_keyword(2, "lse", TokenKind::False),
                Some(b'i') => self.check_keyword(2, "nally", TokenKind::Finally),
                Some(b'o') => match self.source.get(self.start + 2) {
                    Some(b'r') => match self.check_keyword(3, "", TokenKind::For) {
                        TokenKind::For => TokenKind::For,
                        _ => match self.source.get(self.start + 3) {
                            Some(b'e') => self.check_keyword(4, "ach", TokenKind::ForEach),
                            _ => TokenKind::Identifier,
                        },
                    },
                    _ => TokenKind::Identifier,
                },
                Some(b'r') => self.check_keyword(2, "om", TokenKind::From),
                Some(b'u') => self.check_keyword(2, "n", TokenKind::Fun),
                _ => TokenKind::Identifier,
            },
            b'i' => match self.source.get(self.start + 1) {
                Some(b'f') => self.check_keyword(2, "", TokenKind::If),
                Some(b'n') => self.check_keyword(2, "", TokenKind::In),
                Some(b'm') => self.check_keyword(2, "port", TokenKind::Import),
                _ => TokenKind::Identifier,
            },
            b'n' => self.check_keyword(1, "il", TokenKind::Nil),
            b'o' => self.check_keyword(1, "r", TokenKind::Or),
            b'r' => self.check_keyword(1, "eturn", TokenKind::Return),
            b's' => match self.source.get(self.start + 1) {
                Some(b'u') => self.check_keyword(2, "per", TokenKind::Super),
                Some(b'w') => self.check_keyword(2, "itch", TokenKind::Switch),
                _ => TokenKind::Identifier,
            },
            b'S' => self.check_keyword(1, "topIteration", TokenKind::StopIteration),
            b't' => match self.source.get(self.start + 1) {
                Some(b'h') => match self.source.get(self.start + 2) {
                    Some(b'i') => self.check_keyword(3, "s", TokenKind::This),
                    Some(b'r') => self.check_keyword(3, "ow", TokenKind::Throw),
                    _ => TokenKind::Identifier,
                },
                Some(b'r') => match self.source.get(self.start + 2) {
                    Some(b'u') => self.check_keyword(3, "e", TokenKind::True),
                    Some(b'y') => self.check_keyword(3, "", TokenKind::Try),
                    _ => TokenKind::Identifier,
                },
                _ => TokenKind::Identifier,
            },
            b'u' => match self.source.get(self.start + 1) {
                Some(b'n') => match self.source.get(self.start + 2) {
                    Some(b'l') => self.check_keyword(3, "ess", TokenKind::Unless),
                    Some(b't') => self.check_keyword(3, "il", TokenKind::Until),
                    _ => TokenKind::Identifier,
                },
                _ => TokenKind::Identifier,
            },
            b'v' => self.check_keyword(1, "ar", TokenKind::Var),
            b'w' => self.check_keyword(1, "hile", TokenKind::While),
            b'y' => self.check_keyword(1, "ield", TokenKind::Yield),
            _ => TokenKind::Identifier,
        }
    }

    fn check_keyword(&self, start: usize, rest: &str, kind: TokenKind) -> TokenKind {
        let from = self.source.len().min(self.start + start);
        let to = self.source.len().min(from + rest.len());
        if &self.source[from..to] == rest.as_bytes()
            && self
                .source
                .get(to)
                .is_none_or(|c| !Self::is_identifier_char(c))
        {
            kind
        } else {
            TokenKind::Identifier
        }
    }

    fn peek(&self) -> Option<&u8> {
        self.source.get(self.current)
    }

    fn peek_next(&self) -> Option<&u8> {
        self.source.get(self.current + 1)
    }

    fn make_token(&self, kind: TokenKind) -> Token<'a> {
        let to = self.current.min(self.source.len());
        let from = to.min(self.start);
        Token {
            kind,
            lexeme: &self.source[from..to],
            line: self.line,
        }
    }

    const fn error_token(&self, msg: &'static str) -> Token<'a> {
        Token {
            kind: TokenKind::Error,
            lexeme: msg.as_bytes(),
            line: self.line,
        }
    }
}
