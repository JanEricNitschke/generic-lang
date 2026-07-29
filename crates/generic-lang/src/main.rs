//! # Main entry point for the generic interpreter
//!
//! Uses the `clap` crate to parse command line arguments and then calls the appropriate function to either run the REPL or run a file.
//! Not specifying a file will run the REPL, otherwise it will run the file.
//! The file path ends interpreter option parsing: everything after it is
//! passed through to the script verbatim as `os.argv[1:]`.

#![forbid(unsafe_code)]
// Pre-existing duplicate transitive dependencies; nothing actionable per crate.
#![allow(clippy::multiple_crate_versions)]

use std::path::PathBuf;

use clap::Parser;
use generic_lang_lib::{InterpretResult, TestRunResult, repl, run_file, run_tests};

#[derive(Parser, Debug)]
#[command(version)]
struct Args {
    /// Run in test mode, discovering and executing test functions from a file or directory
    #[arg(short, long)]
    test: bool,

    /// The file to run followed by its arguments (`os.argv[1:]`). The
    /// file path is the first positional, and it ends option parsing:
    /// everything after it reaches the script verbatim, even `-t` or
    /// `--version`.
    #[arg(value_name = "FILE [ARGUMENTS]", trailing_var_arg = true)]
    file_and_arguments: Vec<String>,
}

/// Main entry point for the generic interpreter
fn main() {
    let args = Args::parse();
    let mut file_and_arguments = args.file_and_arguments.into_iter();
    let file = file_and_arguments.next().map(PathBuf::from);

    match (file, args.test) {
        (Some(file), true) => match run_tests(&file) {
            TestRunResult::AllPassed => {}
            TestRunResult::HadFailures => std::process::exit(1),
            TestRunResult::InvalidPath => std::process::exit(74),
        },
        (Some(file), false) => match run_file(file, file_and_arguments.collect()) {
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

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(line: &[&str]) -> Args {
        Args::try_parse_from(line).unwrap()
    }

    #[test]
    fn plain_script_arguments_pass_through() {
        let args = parse(&["generic", "script.gen", "alpha", "beta"]);
        assert!(!args.test);
        assert_eq!(args.file_and_arguments, ["script.gen", "alpha", "beta"]);
    }

    /// The file path ends option parsing: even the interpreter's own
    /// flags reach the script when they come after it.
    #[test]
    fn everything_after_the_file_goes_to_the_script() {
        let args = parse(&["generic", "script.gen", "-t", "--version", "--verbose"]);
        assert!(!args.test);
        assert_eq!(
            args.file_and_arguments,
            ["script.gen", "-t", "--version", "--verbose"]
        );
    }

    #[test]
    fn test_mode_before_the_file() {
        let args = parse(&["generic", "-t", "tests"]);
        assert!(args.test);
        assert_eq!(args.file_and_arguments, ["tests"]);
    }

    #[test]
    fn no_file_means_repl() {
        let args = parse(&["generic"]);
        assert!(!args.test);
        assert!(args.file_and_arguments.is_empty());
    }

    /// A leading `--` still lets a file whose name starts with a dash
    /// through.
    #[test]
    fn double_dash_escapes_a_dashed_file_name() {
        let args = parse(&["generic", "--", "-weird.gen", "alpha"]);
        assert!(!args.test);
        assert_eq!(args.file_and_arguments, ["-weird.gen", "alpha"]);
    }

    #[test]
    fn version_before_a_file_is_the_interpreter_version() {
        let error = Args::try_parse_from(["generic", "--version"]).unwrap_err();
        assert_eq!(error.kind(), clap::error::ErrorKind::DisplayVersion);
    }

    /// An unknown flag before any file is a clean usage error, not a
    /// "file not found" attempt.
    #[test]
    fn unknown_flag_before_a_file_is_a_usage_error() {
        let error = Args::try_parse_from(["generic", "--bogus"]).unwrap_err();
        assert_eq!(error.kind(), clap::error::ErrorKind::UnknownArgument);
    }
}
