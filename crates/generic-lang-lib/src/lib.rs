//! # The generic programming language
//!
//! Core implementation of the generic interpreter: scanner, compiler,
//! bytecode VM, heap/GC, natives, and stdlib. The `generic-lang` crate wraps
//! this library into the `generic` binary.

// The core interpreter contains no unsafe code; all FFI unsafety lives in
// the plugin machinery (`vm::plugins`, which scopes its own allow). With
// the feature off the crate forbids unsafe outright; with it on, the lint
// is deny so the plugin module can opt out locally.
#![cfg_attr(not(feature = "plugins"), forbid(unsafe_code))]
#![cfg_attr(feature = "plugins", deny(unsafe_code))]
#![allow(clippy::wrong_self_convention)]
#![allow(clippy::missing_const_for_fn)]
#![allow(clippy::option_if_let_else)]
// Pre-existing duplicate transitive dependencies (syn 1/2 via derivative and
// shrinkwraprs, rand 0.8/0.10, …); nothing actionable per crate.
#![allow(clippy::multiple_crate_versions)]
// Contradicts `unreachable_pub` on items in private modules - see the
// workspace lints table. Attribute, not the table allow alone: CI's pedantic
// pass re-enables nursery on the command line, which outranks `[lints]`.
#![allow(clippy::redundant_pub_crate)]

use std::{io::Write, path::PathBuf};

pub use compiler::{FormatError, FormatOptions, format_source, format_source_with_default};
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
mod suggestions;
mod types;
mod utils;
mod value;
mod vm;

pub use testing::{TestRunResult, run_tests};
pub use vm::InterpretResult;

/// Options controlling a VM run.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RunOptions {
    /// Whether imported modules may be loaded from the on-disk cache.
    pub cache_read: bool,
    /// Whether compiled imported modules may be written to the on-disk cache.
    pub cache_write: bool,
    /// Whether to run the optimizer. Defaults to `true`.
    pub optimize: bool,
}

impl RunOptions {
    const fn for_profile(debug_assertions: bool) -> Self {
        let enabled = !debug_assertions;
        Self {
            cache_read: enabled,
            cache_write: enabled,
            optimize: true,
        }
    }
}

impl Default for RunOptions {
    /// Debug builds disable the module cache by default so stale bytecode
    /// cannot outlive a compiler change during development.
    fn default() -> Self {
        Self::for_profile(cfg!(debug_assertions))
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
pub fn repl(options: RunOptions) {
    let mut vm = VM::new(options);
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
/// `script_args` are the command line arguments for the script itself;
/// together with the script path they become `os.argv`.
///
/// # Errors
///
/// Returns an error if the file cannot be read.
pub fn run_file(
    file: PathBuf,
    script_args: Vec<String>,
    options: RunOptions,
) -> Result<InterpretResult, std::io::Error> {
    let contents = std::fs::read_to_string(&file)?;
    let mut vm = VM::new(options);
    let mut argv = vec![file.to_string_lossy().into_owned()];
    argv.extend(script_args);
    vm.set_script_args(argv);
    Ok(vm.interpret(&contents, file))
}

#[cfg(test)]
mod tests {
    use super::RunOptions;

    #[test]
    fn cache_defaults_follow_the_build_profile() {
        let debug = RunOptions::for_profile(true);
        assert!(!debug.cache_read);
        assert!(!debug.cache_write);

        let release = RunOptions::for_profile(false);
        assert!(release.cache_read);
        assert!(release.cache_write);
    }
}
