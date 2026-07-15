//! # The generic programming language
//!
//! Core implementation of the generic interpreter: scanner, compiler,
//! bytecode VM, heap/GC, natives, and stdlib. The `generic-lang` crate wraps
//! this library into the `generic` binary.

#![allow(clippy::wrong_self_convention)]
#![allow(clippy::missing_const_for_fn)]
#![allow(clippy::option_if_let_else)]
// Pre-existing duplicate transitive dependencies (syn 1/2 via derivative and
// shrinkwraprs, rand 0.8/0.10, …); nothing actionable per crate.
#![allow(clippy::multiple_crate_versions)]

use std::{io::Write, path::PathBuf};

use vm::VM;

mod testing;

mod bitwise;
mod chunk;
mod compiler;
mod config;
mod heap;
mod natives;
mod scanner;
mod stdlib;
mod types;
mod utils;
mod value;
mod vm;

pub use testing::{TestRunResult, run_tests};
pub use vm::InterpretResult;

/// Run the REPL.
///
/// This function will loop, reading a line from stdin and passing it to the VM for interpretation.
/// Each line has to be fully valid by itself. Splitting statements across lines is not supported.
///
/// # Panics
///
/// If stdout can not be flushed or if `read_line` returns an error.
pub fn repl() {
    let mut vm = VM::new();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut line = String::new();
        if std::io::stdin().read_line(&mut line).unwrap() > 0 {
            vm.interpret(&line, PathBuf::from(""));
        } else {
            println!();
            break;
        }
    }
}

/// Run a file.
///
/// Will read the whole file into memory and pass it to the VM for interpretation.
/// The VM will compile and then interpret the code.
///
/// # Errors
///
/// Returns an error if the file cannot be read.
pub fn run_file(file: PathBuf) -> Result<InterpretResult, std::io::Error> {
    let contents = std::fs::read_to_string(&file)?;
    let mut vm = VM::new();
    Ok(vm.interpret(&contents, file))
}
