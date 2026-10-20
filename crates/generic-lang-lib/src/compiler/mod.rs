//! The compiler: Text -> Tokens -> **AST** -> Bytecode.
//!
//! [`front`] and [`rules`] parse the scanner's tokens into the tree
//! defined in [`ast`]; [`back`] (with [`variables`]) walks the tree and
//! emits bytecode.

mod ast;
mod back;
pub(crate) mod cache;
mod error;
mod front;
mod rules;
mod variables;

use crate::{heap::Heap, scanner::Scanner, types::InjectedKind, value::Function};
use back::Codegen;
use front::Parser;

/// Main compiler struct that turns tokens into bytecode.
pub(crate) struct Compiler<'scanner, 'heap> {
    parser: Parser<'scanner>,
    codegen: Codegen<'heap, 'scanner>,
    name: String,
}

impl<'scanner, 'heap> Compiler<'scanner, 'heap> {
    #[must_use]
    pub(crate) fn new(
        scanner: Scanner<'scanner>,
        heap: &'heap mut Heap,
        name: &str,
        #[cfg(any(feature = "print_code", feature = "debug_parser"))] is_builtin: bool,
    ) -> Self {
        let source = scanner.source();
        Self {
            parser: Parser::new(
                scanner,
                #[cfg(feature = "debug_parser")]
                is_builtin,
            ),
            codegen: Codegen::new(
                heap,
                source,
                #[cfg(feature = "print_code")]
                is_builtin,
            ),
            name: name.to_string(),
        }
    }

    /// Compile the tokens provided by the scanner into a function,
    /// printing any compile errors to stderr.
    ///
    /// This is the main compilation loop of generic.
    pub(crate) fn compile(self) -> Option<Function> {
        match self.compile_collect() {
            Ok(function) => Some(function),
            Err(errors) => {
                for error in errors {
                    eprintln!("{error}");
                }
                None
            }
        }
    }

    /// Like [`Self::compile`], but return the compile errors instead of
    /// printing them.
    pub(crate) fn compile_collect(mut self) -> Result<Function, Vec<String>> {
        let program = self.parser.parse_program();
        if self.parser.had_error {
            return Err(std::mem::take(&mut self.parser.errors));
        }
        self.codegen.compile(&program, &self.name)
    }

    /// Compile injected (`eval`/`exec`) source. `locals` are pre-declared
    /// as the function's leading local slots, filled by the caller like
    /// arguments (so the function's arity is their count). `Eval` compiles
    /// a single expression whose value the function returns.
    /// Compile failures are *returned* (the `eval`/`exec` natives wrap
    /// them in a catchable `SyntaxError`).
    pub(crate) fn compile_injected(
        mut self,
        locals: &[String],
        kind: InjectedKind,
    ) -> Result<Function, Vec<String>> {
        let program = self.parser.parse_injected(locals, kind);
        if self.parser.had_error {
            return Err(std::mem::take(&mut self.parser.errors));
        }
        self.codegen.compile_injected(&program, &self.name)
    }
}
