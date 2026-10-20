//! Error handling for the parser.
//!
//! Print nice error messages to the console when something goes wrong.
//! Also handles synchronization after an error to prevent cascading errors and
//! allow for multiple errors to be reported in a single run.

use super::front::Parser;
use crate::{
    scanner::{Token, TokenKind as TK},
    types::{OpcodeLocation, render_snippet_rows},
};

impl Parser<'_> {
    pub(super) fn error_at_current(&mut self, msg: &str) {
        self.error_at(self.current.clone().as_ref(), msg);
    }

    pub(super) fn error(&mut self, msg: &str) {
        self.error_at(self.previous.clone().as_ref(), msg);
    }

    fn error_at(&mut self, token: Option<&Token>, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        if let Some(token) = token.as_ref() {
            let position = if token.kind == TK::Eof {
                " at end".to_string()
            } else if token.kind == TK::Error {
                String::new()
            } else {
                format!(" at '{}'", token.as_str())
            };
            let mut message = format!("[line {}] Error{position}: {msg}", *token.location.end_line);
            // Underline the erroring token, spanning all of its lines.
            // End-of-file errors and tokens swallowed to the end of the
            // file (an unterminated string, say) have nothing to point at.
            let reaches_eof = token.kind == TK::Error && self.scanner.at_eof();
            if token.kind != TK::Eof && !reaches_eof {
                message.push('\n');
                render_snippet_rows(
                    &mut message,
                    self.scanner.source(),
                    &OpcodeLocation::new(token.location),
                );
            }
            self.errors.push(message);
        }
        self.had_error = true;
        #[cfg(feature = "debug_parser")]
        if cfg!(feature = "debug_parser_builtin") || !self.is_builtin {
            println!("Hit error, set panic_mode: {}", self.panic_mode);
        }
    }

    pub(super) fn synchronize(&mut self) {
        #[cfg(feature = "debug_parser")]
        if cfg!(feature = "debug_parser_builtin") || !self.is_builtin {
            println!("Synchronizing after error");
        }
        self.panic_mode = false;
        while !self.check(TK::Eof) {
            if self.check_previous(TK::Semicolon) {
                #[cfg(feature = "debug_parser")]
                if cfg!(feature = "debug_parser_builtin") || !self.is_builtin {
                    println!("Found semicolon, resuming parsing");
                }
                return;
            }
            if let Some(
                TK::Class
                | TK::Fun
                | TK::Gen
                | TK::At
                | TK::Const
                | TK::Var
                | TK::For
                | TK::ForEach
                | TK::If
                | TK::Unless
                | TK::While
                | TK::Until
                | TK::Switch
                | TK::Return
                | TK::Try
                | TK::Throw
                | TK::Import
                | TK::From,
            ) = self.current_token_kind()
            {
                #[cfg(feature = "debug_parser")]
                if cfg!(feature = "debug_parser_builtin") || !self.is_builtin {
                    println!(
                        "Found statement start at {:?}, resuming parsing",
                        self.current_token_kind()
                    );
                }
                return;
            }
            self.advance();
        }
    }
}
