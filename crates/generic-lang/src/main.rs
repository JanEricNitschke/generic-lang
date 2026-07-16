//! # Main entry point for the generic interpreter
//!
//! Uses the `clap` crate to parse command line arguments and then calls the appropriate function to either run the REPL or run a file.
//! Not specifying a file will run the REPL, otherwise it will run the file.
//! There are currently no other command line options.

#![forbid(unsafe_code)]
// Pre-existing duplicate transitive dependencies; nothing actionable per crate.
#![allow(clippy::multiple_crate_versions)]

use std::path::PathBuf;

use clap::Parser;
use generic_lang_lib::{InterpretResult, TestRunResult, repl, run_file, run_tests};

#[derive(Parser, Debug)]
#[command(version)]
struct Args {
    file: Option<PathBuf>,

    /// Run in test mode, discovering and executing test functions from a file or directory
    #[arg(short, long)]
    test: bool,
}

/// Main entry point for the generic interpreter
fn main() {
    let args = Args::parse();

    match (args.file, args.test) {
        (Some(file), true) => match run_tests(&file) {
            TestRunResult::AllPassed => {}
            TestRunResult::HadFailures => std::process::exit(1),
            TestRunResult::InvalidPath => std::process::exit(74),
        },
        (Some(file), false) => match run_file(file) {
            Err(e) => {
                eprintln!("{e}");
                std::process::exit(74);
            }
            Ok(InterpretResult::CompileError) => std::process::exit(65),
            Ok(InterpretResult::RuntimeError) => std::process::exit(70),
            Ok(InterpretResult::Ok) => {}
        },
        (None, true) => {
            eprintln!("Error: --test flag requires a file or directory to be specified");
            std::process::exit(64);
        }
        (None, false) => repl(),
    }
}
