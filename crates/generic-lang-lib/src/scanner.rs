//! Defines the tokens and scanner that handles the transforming or the source to tokens.

use num_enum::{IntoPrimitive, TryFromPrimitive};
use strum::{EnumIter, EnumProperty, IntoEnumIterator};

use crate::types::Location;

/// `Token` types that exist in the generic language.
#[derive(
    IntoPrimitive, TryFromPrimitive, PartialEq, Eq, Clone, Copy, Debug, EnumIter, EnumProperty,
)]
#[repr(u8)]
pub enum TokenKind {
    // Delimiters.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Semicolon,

    // Operators.
    Plus,
    PlusEqual,
    Minus,
    MinusEqual,
    Star,
    StarEqual,
    StarStar,
    Slash,
    SlashEqual,
    SlashSlash,
    Percent,
    PercentEqual,
    Amper,
    AmperEqual,
    Pipe,
    PipeEqual,
    Hat,
    HatEqual,
    DotDotLess,
    DotDotEqual,
    Colon,
    Bang,
    Equal,
    QuestionMark,
    QuestionDot,
    QuestionBracket,

    // Comparison.
    #[strum(props(keyword = "is"))]
    Is,
    EqualEqual,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Logical operators.
    #[strum(props(keyword = "and"))]
    And,
    #[strum(props(keyword = "or"))]
    Or,
    #[strum(props(keyword = "in"))]
    In,

    // Literals and string machinery.
    Identifier,
    String,
    Integer,
    Float,
    FstringStart,
    TstringStart,
    InterpolationStringPart,
    InterpolationStart,
    InterpolationEnd,
    InterpolationStringEnd,

    // Value keywords.
    #[strum(props(keyword = "nil"))]
    Nil,
    #[strum(props(keyword = "true"))]
    True,
    #[strum(props(keyword = "false"))]
    False,
    #[strum(props(keyword = "StopIteration"))]
    StopIteration,

    // Declarations, functions, and objects.
    #[strum(props(keyword = "var"))]
    Var,
    #[strum(props(keyword = "const"))]
    Const,
    At,
    #[strum(props(keyword = "class"))]
    Class,
    #[strum(props(keyword = "this"))]
    This,
    #[strum(props(keyword = "super"))]
    Super,
    #[strum(props(keyword = "fun"))]
    Fun,
    #[strum(props(keyword = "gen"))]
    Gen,
    RightArrow,
    #[strum(props(keyword = "return"))]
    Return,
    #[strum(props(keyword = "yield"))]
    Yield,

    // Control flow.
    #[strum(props(keyword = "if"))]
    If,
    #[strum(props(keyword = "unless"))]
    Unless,
    #[strum(props(keyword = "else"))]
    Else,
    #[strum(props(keyword = "switch"))]
    Switch,
    #[strum(props(keyword = "case"))]
    Case,
    #[strum(props(keyword = "default"))]
    Default,
    #[strum(props(keyword = "for"))]
    For,
    #[strum(props(keyword = "foreach"))]
    ForEach,
    #[strum(props(keyword = "while"))]
    While,
    #[strum(props(keyword = "until"))]
    Until,
    #[strum(props(keyword = "break"))]
    Break,
    #[strum(props(keyword = "continue"))]
    Continue,
    Apostrophe,

    // Imports.
    #[strum(props(keyword = "import"))]
    Import,
    #[strum(props(keyword = "from"))]
    From,
    #[strum(props(keyword = "as"))]
    As,

    // Exception handling.
    #[strum(props(keyword = "try"))]
    Try,
    #[strum(props(keyword = "catch"))]
    Catch,
    #[strum(props(keyword = "finally"))]
    Finally,
    #[strum(props(keyword = "throw"))]
    Throw,

    // Reserved.
    #[strum(props(keyword = "async"))]
    Async,
    #[strum(props(keyword = "await"))]
    Await,

    // Synthetic.
    Error,
    Eof,
}
#[cfg(test)]
#[test]
fn test_token_kind_size() {
    assert_eq!(size_of::<TokenKind>(), 1);
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
    pub(super) lexeme: &'a str,
    pub(super) location: Location,
}

#[cfg(test)]
#[test]
fn test_token_size() {
    // 64 on 64-bit (str is 16 bytes), 32 on 32-bit (str is 8 bytes).
    assert!(size_of::<Token<'_>>() <= 64);
}

impl<'a> Token<'a> {
    pub(super) fn as_str(&'a self) -> &'a str {
        self.lexeme
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
enum ScannerMode {
    Normal,
    InterpolationString,
    Interpolation(u8),
}

/// Main struct for parsing the source characters to tokens.
#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    /// Always points at the next character to be consumed.
    current: usize,
    location: Location,
    modes: Vec<ScannerMode>,
    #[cfg(feature = "debug_scanner")]
    is_builtin: bool,
}

impl<'a> Scanner<'a> {
    #[must_use]
    pub(super) fn new(source: &'a str, #[cfg(feature = "debug_scanner")] is_builtin: bool) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            location: Location::default(),
            modes: vec![ScannerMode::Normal],
            #[cfg(feature = "debug_scanner")]
            is_builtin,
        }
    }

    pub fn source_slice(&self, start: usize, end: usize) -> &'a str {
        &self.source[start..end]
    }

    fn mode(&self) -> &ScannerMode {
        self.modes
            .last()
            .expect("Internal Error: Scanner mode underflow")
    }

    fn mode_mut(&mut self) -> &mut ScannerMode {
        self.modes
            .last_mut()
            .expect("Internal Error: Scanner mode underflow")
    }

    fn unclosed_braces(&mut self) -> &mut u8 {
        match self.mode_mut() {
            ScannerMode::Interpolation(unclosed_braces) => unclosed_braces,
            _ => unreachable!("Should only call `unclosed_braces` while in Interpolation mode!"),
        }
    }

    /// Main scan that turns raw characters to tokens.
    ///
    /// Switch between `Normal` mode for most parts of source files,
    /// `Fstring` mode for the raw string parts of fstrings and
    /// `Interpolation` mode for the interpolations of fstrings.
    pub(super) fn scan(&mut self) -> Token<'a> {
        #[cfg(feature = "debug_scanner")]
        if cfg!(feature = "debug_scanner_builtin") || !self.is_builtin {
            println!(
                "Mode: {:<20} current char {:<10} at pos {}",
                format!("{:?}", self.mode()),
                format!(
                    "{:?}",
                    self.source.as_bytes().get(self.current).map(|&b| b as char)
                ),
                self.current,
            );
        }
        match self.mode() {
            ScannerMode::Normal => self.scan_normal(),
            ScannerMode::InterpolationString => self.scan_interpolation_string(),
            ScannerMode::Interpolation(_) => self.scan_interpolation(),
        }
    }

    /// Scan function for the string bodies of fstrings.
    ///
    /// Scans everything AFTER `f"` or `t"` up to and including the closing `"` or
    /// the start of the interpolation `${`
    ///
    /// Switches to `Normal` mode on closing quotes and
    /// `Interpolation` mode after consuming `${`.
    fn scan_interpolation_string(&mut self) -> Token<'a> {
        self.set_current();

        if self.peek() == Some(&b'$') {
            self.advance(); // consume $
            if self.match_(b'{') {
                self.modes.push(ScannerMode::Interpolation(1));
                return self.make_token(TokenKind::InterpolationStart);
            }
        }

        self.interpolation_string_part()
    }

    fn interpolation_string_part(&mut self) -> Token<'a> {
        while let Some(&c) = self.peek() {
            if c == b'"' {
                // End of t- or f-string
                self.advance(); // consume closing "
                self.modes.pop(); // Transition back to normal mode
                return self.make_token(TokenKind::InterpolationStringEnd);
            }
            if c == b'$' && self.peek_next() == Some(&b'{') {
                // Stop before interpolation
                return self.make_token(TokenKind::InterpolationStringPart);
            }
            if c == b'\n' {
                self.next_line();
            }

            self.advance();
        }

        // This way we get the proper EOF afterwards from the main loop.
        self.modes.pop();
        // Fell out of loop => EOF
        self.error_token("Unterminated t- or f-string.")
    }

    /// Scans function for the interpolation part of fstrings.
    ///
    /// Scans everything AFTER the interpolation opening `${`
    /// up to and including the closing `}`.
    ///
    /// Does so by keeping count of the opening and closing braces
    /// and switches back to fstring mode when they match.
    fn scan_interpolation(&mut self) -> Token<'a> {
        use TokenKind as TK;
        self.skip_whitespace();
        self.set_current();
        // In interpolation mode you already consumed the '{'
        let c = match self.advance() {
            None => {
                // This way we get the unterminated fstring and later the EOF from the other modes.
                self.modes.pop();
                return self.error_token("Unterminated interpolation.");
            }
            Some(b'{') => {
                *self.unclosed_braces() += 1;
                return self.make_token(TK::LeftBrace);
            }
            Some(b'}') => {
                *self.unclosed_braces() -= 1;
                let token_kind = if *self.unclosed_braces() == 0 {
                    self.modes.pop();
                    TK::InterpolationEnd
                } else {
                    TK::RightBrace
                };
                return self.make_token(token_kind);
            }
            Some(c) => *c,
        };
        // Reuse the same match logic
        self.scan_core(c)
    }

    /// Main scan that turns raw characters to tokens.
    ///
    /// Every call to this function parses enough of the source file
    /// to return exactly one token.
    ///
    /// Uses a trie strategy to identify tokens.
    ///
    /// Switches to fstring mode when it encounters an fstring start `f"`.
    fn scan_normal(&mut self) -> Token<'a> {
        self.skip_whitespace();
        self.set_current();
        let c = match self.advance() {
            None => return self.make_token(TokenKind::Eof),
            Some(c) => *c,
        };
        self.scan_core(c)
    }

    #[allow(clippy::too_many_lines)]
    fn scan_core(&mut self, c: u8) -> Token<'a> {
        use TokenKind as TK;
        let token_kind = match c {
            b'\'' => TK::Apostrophe,
            b'?' => {
                if self.match_(b'.') {
                    TK::QuestionDot
                } else if self.match_(b'[') {
                    TK::QuestionBracket
                } else {
                    TK::QuestionMark
                }
            }
            b':' => TK::Colon,
            b'(' => TK::LeftParen,
            b')' => TK::RightParen,
            b'{' => TK::LeftBrace,
            b'}' => TK::RightBrace,
            b'[' => TK::LeftBracket,
            b']' => TK::RightBracket,
            b';' => TK::Semicolon,
            b',' => TK::Comma,
            b'.' => {
                if self.match_(b'.') {
                    if self.match_(b'=') {
                        TK::DotDotEqual
                    } else if self.match_(b'<') {
                        TK::DotDotLess
                    } else {
                        return self.error_token("Invalid token '..' - expected '..=' or '..<'");
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
            b'"' => return self.string(),
            b'f' => {
                if self.match_(b'"') {
                    self.modes.push(ScannerMode::InterpolationString);
                    TK::FstringStart
                } else {
                    return self.identifier();
                }
            }
            b't' => {
                if self.match_(b'"') {
                    self.modes.push(ScannerMode::InterpolationString);
                    TK::TstringStart
                } else {
                    return self.identifier();
                }
            }
            c if c.is_ascii_digit() => return self.number(),
            c if c.is_ascii_alphabetic() || c == b'_' => return self.identifier(),
            _ => return self.error_token("Unexpected character."),
        };

        self.make_token(token_kind)
    }

    fn advance(&mut self) -> Option<&u8> {
        self.current += 1;
        *self.location.end_column += 1;
        self.source.as_bytes().get(self.current - 1)
    }

    fn match_(&mut self, expected: u8) -> bool {
        match self.source.as_bytes().get(self.current) {
            Some(actual) if actual == &expected => {
                self.current += 1;
                *self.location.end_column += 1;
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
                    self.next_line();
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
        while let Some(&c) = self.peek() {
            if c == b'"' {
                self.advance(); // consume closing "
                return self.make_token(TokenKind::String);
            }
            if c == b'\n' {
                self.next_line();
            }
            self.advance();
        }

        // Fell out of loop => EOF
        self.error_token("Unterminated string.")
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
        use TokenKind as TK;
        match self.source.as_bytes()[self.start] {
            b'a' => match self.source.as_bytes().get(self.start + 1) {
                Some(b'n') => self.check_keyword(2, "d", TK::And),
                Some(b's') => match self.check_keyword(2, "", TK::As) {
                    TK::As => TK::As,
                    _ => match self.source.as_bytes().get(self.start + 2) {
                        Some(b'y') => self.check_keyword(3, "nc", TK::Async),
                        _ => TK::Identifier,
                    },
                },
                Some(b'w') => self.check_keyword(2, "ait", TK::Await),
                _ => TK::Identifier,
            },
            b'b' => self.check_keyword(1, "reak", TK::Break),
            b'c' => match self.source.as_bytes().get(self.start + 1) {
                Some(b'a') => match self.source.as_bytes().get(self.start + 2) {
                    Some(b's') => self.check_keyword(3, "e", TK::Case),
                    Some(b't') => self.check_keyword(3, "ch", TK::Catch),
                    _ => TK::Identifier,
                },
                Some(b'l') => self.check_keyword(2, "ass", TK::Class),
                Some(b'o') => match self.source.as_bytes().get(self.start + 2) {
                    Some(b'n') => match self.source.as_bytes().get(self.start + 3) {
                        Some(b's') => self.check_keyword(4, "t", TK::Const),
                        Some(b't') => self.check_keyword(4, "inue", TK::Continue),
                        _ => TK::Identifier,
                    },
                    _ => TK::Identifier,
                },
                _ => TK::Identifier,
            },
            b'd' => self.check_keyword(1, "efault", TK::Default),
            b'e' => self.check_keyword(1, "lse", TK::Else),
            b'f' => match self.source.as_bytes().get(self.start + 1) {
                Some(b'a') => self.check_keyword(2, "lse", TK::False),
                Some(b'i') => self.check_keyword(2, "nally", TK::Finally),
                Some(b'o') => match self.source.as_bytes().get(self.start + 2) {
                    Some(b'r') => match self.check_keyword(3, "", TK::For) {
                        TK::For => TK::For,
                        _ => match self.source.as_bytes().get(self.start + 3) {
                            Some(b'e') => self.check_keyword(4, "ach", TK::ForEach),
                            _ => TK::Identifier,
                        },
                    },
                    _ => TK::Identifier,
                },
                Some(b'r') => self.check_keyword(2, "om", TK::From),
                Some(b'u') => self.check_keyword(2, "n", TK::Fun),
                _ => TK::Identifier,
            },
            b'g' => self.check_keyword(1, "en", TK::Gen),
            b'i' => match self.source.as_bytes().get(self.start + 1) {
                Some(b'f') => self.check_keyword(2, "", TK::If),
                Some(b'n') => self.check_keyword(2, "", TK::In),
                Some(b's') => self.check_keyword(2, "", TK::Is),
                Some(b'm') => self.check_keyword(2, "port", TK::Import),
                _ => TK::Identifier,
            },
            b'n' => self.check_keyword(1, "il", TK::Nil),
            b'o' => self.check_keyword(1, "r", TK::Or),
            b'r' => self.check_keyword(1, "eturn", TK::Return),
            b's' => match self.source.as_bytes().get(self.start + 1) {
                Some(b'u') => self.check_keyword(2, "per", TK::Super),
                Some(b'w') => self.check_keyword(2, "itch", TK::Switch),
                _ => TK::Identifier,
            },
            b'S' => self.check_keyword(1, "topIteration", TK::StopIteration),
            b't' => match self.source.as_bytes().get(self.start + 1) {
                Some(b'h') => match self.source.as_bytes().get(self.start + 2) {
                    Some(b'i') => self.check_keyword(3, "s", TK::This),
                    Some(b'r') => self.check_keyword(3, "ow", TK::Throw),
                    _ => TK::Identifier,
                },
                Some(b'r') => match self.source.as_bytes().get(self.start + 2) {
                    Some(b'u') => self.check_keyword(3, "e", TK::True),
                    Some(b'y') => self.check_keyword(3, "", TK::Try),
                    _ => TK::Identifier,
                },
                _ => TK::Identifier,
            },
            b'u' => match self.source.as_bytes().get(self.start + 1) {
                Some(b'n') => match self.source.as_bytes().get(self.start + 2) {
                    Some(b'l') => self.check_keyword(3, "ess", TK::Unless),
                    Some(b't') => self.check_keyword(3, "il", TK::Until),
                    _ => TK::Identifier,
                },
                _ => TK::Identifier,
            },
            b'v' => self.check_keyword(1, "ar", TK::Var),
            b'w' => self.check_keyword(1, "hile", TK::While),
            b'y' => self.check_keyword(1, "ield", TK::Yield),
            _ => TK::Identifier,
        }
    }

    fn check_keyword(&self, start: usize, rest: &str, kind: TokenKind) -> TokenKind {
        let from = self.source.len().min(self.start + start);
        let to = self.source.len().min(from + rest.len());
        if &self.source.as_bytes()[from..to] == rest.as_bytes()
            && self
                .source
                .as_bytes()
                .get(to)
                .is_none_or(|c| !Self::is_identifier_char(c))
        {
            kind
        } else {
            TokenKind::Identifier
        }
    }

    fn peek(&self) -> Option<&u8> {
        self.source.as_bytes().get(self.current)
    }

    fn peek_next(&self) -> Option<&u8> {
        self.source.as_bytes().get(self.current + 1)
    }

    fn make_token(&self, kind: TokenKind) -> Token<'a> {
        let to = self.current.min(self.source.len());
        let from = to.min(self.start);
        Token {
            kind,
            lexeme: &self.source[from..to],
            location: self.location,
        }
    }

    const fn error_token(&self, msg: &'static str) -> Token<'a> {
        Token {
            kind: TokenKind::Error,
            lexeme: msg,
            location: self.location,
        }
    }

    fn set_current(&mut self) {
        self.start = self.current;
        self.location.start_line = self.location.end_line;
        self.location.start_column = self.location.end_column;
        self.location.index = self.current;
    }

    fn next_line(&mut self) {
        *self.location.end_line += 1;
        *self.location.end_column = 0;
    }
}

/// The reserved words of the language, read once from the `keyword`
/// properties on [`TokenKind`] (zero cost for the scanner itself, which
/// never consults them). The scanner tests pin the tags against the
/// keyword trie in both directions.
pub fn keywords() -> &'static [&'static str] {
    static KEYWORDS: std::sync::LazyLock<Vec<&'static str>> = std::sync::LazyLock::new(|| {
        let mut keywords: Vec<&'static str> = TokenKind::iter()
            .filter_map(|kind| kind.get_str("keyword"))
            .collect();
        keywords.sort_unstable();
        keywords
    });
    &KEYWORDS
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenKind as TK;

    /// Forward drift guard: every tagged keyword lexeme scans to exactly
    /// its own token kind, so a tag without a matching trie arm fails.
    #[test]
    fn keyword_tags_match_the_trie() {
        assert_eq!(keywords().len(), 38);
        assert!(keywords().is_sorted());
        for kind in TokenKind::iter() {
            let Some(lexeme) = kind.get_str("keyword") else {
                continue;
            };
            let tokens = scan_tokens(lexeme);
            assert_eq!(
                (tokens.len(), tokens[0].kind),
                (1, kind),
                "tag `{lexeme}` does not scan to its own kind"
            );
        }
    }

    /// Reverse drift guard (convention-assisted): scanning the lowercased
    /// variant name of every untagged token kind must NOT hit the trie,
    /// so a trie keyword whose variant lost its tag fails here.
    #[test]
    fn untagged_kinds_are_not_keywords() {
        for kind in TokenKind::iter() {
            if kind.get_str("keyword").is_some() || kind == TK::Identifier {
                continue;
            }
            let lexeme = format!("{kind:?}").to_lowercase();
            if !lexeme.bytes().all(|byte| byte.is_ascii_alphabetic()) {
                continue;
            }
            let tokens = scan_tokens(&lexeme);
            assert_eq!(
                (tokens.len(), tokens[0].kind),
                (1, TK::Identifier),
                "`{lexeme}` hits the trie but its kind carries no keyword tag"
            );
        }
    }

    fn scan_tokens(source: &str) -> Vec<Token<'_>> {
        let mut scanner = Scanner::new(
            source,
            #[cfg(feature = "debug_scanner")]
            false,
        );
        let mut tokens = Vec::new();
        loop {
            let token = scanner.scan();
            if token.kind == TK::Eof {
                break;
            }
            // Skip EOF tokens to reduce clutter
            tokens.push(token);
        }
        tokens
    }

    fn assert_token_kinds(source: &str, expected: &[TokenKind]) {
        let actual: Vec<_> = scan_tokens(source).into_iter().map(|t| t.kind).collect();
        assert_eq!(actual, expected, "Mismatch for source: '{source}'");
    }

    #[test]
    fn test_numbers() {
        assert_token_kinds("123", &[TK::Integer]);
        assert_token_kinds("123.456", &[TK::Float]);
        assert_token_kinds("0", &[TK::Integer]);
        assert_token_kinds("0.5", &[TK::Float]);

        // Test number content parsing
        let tokens = scan_tokens("123");
        assert_eq!(tokens[0].as_str(), "123");
        let tokens = scan_tokens("456.789");
        assert_eq!(tokens[0].as_str(), "456.789");
    }

    #[test]
    fn test_identifiers() {
        assert_token_kinds("foo", &[TK::Identifier]);
        assert_token_kinds("_var", &[TK::Identifier]);
        assert_token_kinds("camelCase", &[TK::Identifier]);
        assert_token_kinds("snake_case", &[TK::Identifier]);

        // Test identifier content
        let tokens = scan_tokens("myVariable");
        assert_eq!(tokens[0].as_str(), "myVariable");
        let tokens = scan_tokens("test_123");
        assert_eq!(tokens[0].as_str(), "test_123");
    }

    #[test]
    fn test_strings() {
        assert_token_kinds("\"hello\"", &[TK::String]);
        assert_token_kinds("\"\"", &[TK::String]);
        assert_token_kinds("\"hello world\"", &[TK::String]);

        // Test string content preservation
        let tokens = scan_tokens("\"test string\"");
        assert_eq!(tokens[0].as_str(), "\"test string\"");
        let tokens = scan_tokens("\"simple\"");
        assert_eq!(tokens[0].as_str(), "\"simple\"");
    }

    #[test]
    fn test_fstring_parsing() {
        // F-string without expressions
        assert_token_kinds(
            "f\"hello\"",
            &[TK::FstringStart, TK::InterpolationStringEnd],
        );

        // Test f-string content
        assert_token_kinds(
            "f\"hello world, my name is ${name}.\"",
            &[
                TK::FstringStart,
                TK::InterpolationStringPart,
                TK::InterpolationStart,
                TK::Identifier,
                TK::InterpolationEnd,
                TK::InterpolationStringEnd,
            ],
        );
    }

    #[test]
    fn test_complex_expression() {
        let source = "var x = (a + b) * c / d;";
        let expected = &[
            TK::Var,
            TK::Identifier,
            TK::Equal,
            TK::LeftParen,
            TK::Identifier,
            TK::Plus,
            TK::Identifier,
            TK::RightParen,
            TK::Star,
            TK::Identifier,
            TK::Slash,
            TK::Identifier,
            TK::Semicolon,
        ];
        assert_token_kinds(source, expected);

        // Test that identifier content is preserved
        let tokens = scan_tokens(source);
        assert_eq!(tokens[1].as_str(), "x"); // variable name
        assert_eq!(tokens[4].as_str(), "a"); // first identifier in expression
    }

    #[test]
    fn test_keywords_and_operators() {
        assert_token_kinds("if else for while", &[TK::If, TK::Else, TK::For, TK::While]);

        assert_token_kinds(
            "== != <= >= < >",
            &[
                TK::EqualEqual,
                TK::BangEqual,
                TK::LessEqual,
                TK::GreaterEqual,
                TK::Less,
                TK::Greater,
            ],
        );
    }

    #[test]
    fn test_medium_program() {
        let source = "
            fun fibonacci(n) {
                if (n <= 1) return n;
                return fibonacci(n - 1) + fibonacci(n - 2);
            }
        ";

        assert_token_kinds(
            source,
            &[
                TK::Fun,
                TK::Identifier,
                TK::LeftParen,
                TK::Identifier,
                TK::RightParen,
                TK::LeftBrace,
                TK::If,
                TK::LeftParen,
                TK::Identifier,
                TK::LessEqual,
                TK::Integer,
                TK::RightParen,
                TK::Return,
                TK::Identifier,
                TK::Semicolon,
                TK::Return,
                TK::Identifier,
                TK::LeftParen,
                TK::Identifier,
                TK::Minus,
                TK::Integer,
                TK::RightParen,
                TK::Plus,
                TK::Identifier,
                TK::LeftParen,
                TK::Identifier,
                TK::Minus,
                TK::Integer,
                TK::RightParen,
                TK::Semicolon,
                TK::RightBrace,
            ],
        );
    }

    #[test]
    fn test_whitespace_and_newlines() {
        assert_token_kinds("  \t  ", &[]); // Only whitespace - no tokens
        assert_token_kinds(" + ", &[TK::Plus]);
        assert_token_kinds("\n+\n", &[TK::Plus]);

        // Multiple tokens with whitespace
        let tokens = scan_tokens("a   +   b");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].as_str(), "a");
        assert_eq!(tokens[1].kind, TK::Plus);
        assert_eq!(tokens[2].as_str(), "b");
    }

    #[test]
    fn test_null_safe_operators() {
        assert_token_kinds("a?.b", &[TK::Identifier, TK::QuestionDot, TK::Identifier]);
        assert_token_kinds(
            "a?[0]",
            &[
                TK::Identifier,
                TK::QuestionBracket,
                TK::Integer,
                TK::RightBracket,
            ],
        );
    }
}
