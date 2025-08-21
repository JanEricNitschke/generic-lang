//! # Main entry point for the generic interpreter
//!
//! Uses the `clap` crate to parse command line arguments and then calls the appropriate function to either run the REPL or run a file.
//! Not specifying a file will run the REPL, otherwise it will run the file.
//! There are currently no other command line options.

#![allow(clippy::wrong_self_convention)]
#![allow(clippy::missing_const_for_fn)]
#![allow(clippy::option_if_let_else)]

use std::{io::Write, path::PathBuf};

use clap::Parser;
use testing::run_tests;
use vm::{InterpretResult, VM};

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

#[allow(clippy::struct_excessive_bools)]
#[derive(Parser, Debug)]
#[command(version)]
struct Args {
    file: Option<PathBuf>,

    /// Run in test mode, discovering and executing test functions from a file or directory
    #[arg(short, long)]
    test: bool,
}

/// Main entry point for the generic interpreter
pub fn main() {
    let args = Args::parse();

    match (args.file, args.test) {
        (Some(file), true) => run_tests(&file),
        (Some(file), false) => run_file(file),
        (None, true) => {
            eprintln!("Error: --test flag requires a file or directory to be specified");
            std::process::exit(64);
        }
        (None, false) => repl(),
    }
}

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
/// Compile or Runtime errors will cause the program to exit with the appropriate error code.
pub fn run_file(file: PathBuf) {
    match std::fs::read_to_string(&file) {
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(74);
        }
        Ok(contents) => {
            let mut vm = VM::new();
            match vm.interpret(&contents, file) {
                InterpretResult::CompileError => std::process::exit(65),
                InterpretResult::RuntimeError => std::process::exit(70),
                InterpretResult::Ok => {}
            }
        }
    }
}
