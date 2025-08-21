//! Error handling for the compiler.
//!
//! Print nice error messages to the console when something goes wrong.
//! Also handles synchronization after an error to prevent cascading errors and
//! allow for multiple errors to be reported in a single run.

use super::Compiler;
use crate::scanner::{Token, TokenKind as TK};

impl Compiler<'_, '_> {
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
            eprint!("[line {}] Error", *token.line);
            if token.kind == TK::Eof {
                eprint!(" at end");
            } else if token.kind != TK::Error {
                eprint!(" at '{}'", token.as_str());
            }
            eprintln!(": {msg}");
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
                | TK::At
                | TK::Const
                | TK::Var
                | TK::For
                | TK::If
                | TK::While
                | TK::Return,
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
