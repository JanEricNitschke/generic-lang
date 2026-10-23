//! # Main entry point for the generic interpreter
//!
//! Uses the `clap` crate to parse command line arguments and then calls the appropriate function to either run the REPL or run a file.
//! Not specifying a file will run the REPL, otherwise it will run the file.
//! The file path ends interpreter option parsing: everything after it is
//! passed through to the script verbatim as `os.argv[1:]`.

#![forbid(unsafe_code)]
// Pre-existing duplicate transitive dependencies; nothing actionable per crate.
#![allow(clippy::multiple_crate_versions)]

use std::path::{Path, PathBuf};

use clap::{Parser, Subcommand};
use generic_lang_lib::{
    FormatOptions, InterpretResult, RunOptions, TestRunResult, format_source, repl, run_file,
    run_tests,
};

#[derive(Parser, Debug)]
#[command(version)]
#[command(args_conflicts_with_subcommands = true)]
#[allow(clippy::struct_excessive_bools)]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    /// Run in test mode, discovering and executing test functions from a file or directory
    #[arg(short, long)]
    test: bool,

    /// Ignore existing compiled-module cache files.
    #[arg(long)]
    no_cache_read: bool,

    /// Do not write compiled-module cache files.
    #[arg(long)]
    no_cache_write: bool,

    /// Disable both reading and writing compiled-module caches.
    #[arg(long)]
    no_cache: bool,

    /// Enable optimizations (the default). Must come before the file path;
    /// overrides an earlier `--no-optimize`.
    #[arg(long, overrides_with = "no_optimize")]
    optimize: bool,

    /// Disable optimizations. Must come before the file path.
    #[arg(long, overrides_with = "optimize")]
    no_optimize: bool,

    /// The file to run followed by its arguments (`os.argv[1:]`). The
    /// file path is the first positional, and it ends option parsing:
    /// everything after it reaches the script verbatim, even `-t` or
    /// `--version`.
    #[arg(value_name = "FILE [ARGUMENTS]", trailing_var_arg = true)]
    file_and_arguments: Vec<String>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Format files to stdout or rewrite them in place
    Fmt {
        /// Rewrite the files in place instead of printing to stdout
        #[arg(short, long)]
        write: bool,

        /// Number of spaces per indentation level (at least 1)
        #[arg(long, value_name = "N", default_value_t = 4, value_parser = clap::value_parser!(u16).range(1..))]
        indent_width: u16,

        /// Soft maximum line length; 0 or omitted means unlimited
        #[arg(long, value_name = "N")]
        line_length: Option<usize>,

        /// The files to format
        #[arg(value_name = "FILE", required = true)]
        files: Vec<PathBuf>,
    },
}

/// Main entry point for the generic interpreter
fn main() {
    let args = Args::parse();
    if let Some(command) = args.command {
        match command {
            Command::Fmt {
                write,
                indent_width,
                line_length,
                files,
            } => {
                let options = FormatOptions {
                    indent_width: usize::from(indent_width),
                    max_line_length: line_length.filter(|&length| length > 0),
                };
                if let Err(code) = format_files(&files, write, options) {
                    std::process::exit(code);
                }
            }
        }
        return;
    }

    let defaults = RunOptions::default();
    let options = RunOptions {
        cache_read: defaults.cache_read && !args.no_cache && !args.no_cache_read,
        cache_write: defaults.cache_write && !args.no_cache && !args.no_cache_write,
        optimize: args.optimize || !args.no_optimize,
    };
    let mut file_and_arguments = args.file_and_arguments.into_iter();
    let file = file_and_arguments.next().map(PathBuf::from);

    match (file, args.test) {
        (Some(file), true) => match run_tests(&file, options) {
            TestRunResult::AllPassed => {}
            TestRunResult::HadFailures => std::process::exit(1),
            TestRunResult::InvalidPath => std::process::exit(74),
        },
        (Some(file), false) => match run_file(file, file_and_arguments.collect(), options) {
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
        (None, false) => repl(options),
    }
}

fn format_files(files: &[PathBuf], write: bool, options: FormatOptions) -> Result<(), i32> {
    let mut formatted = Vec::with_capacity(files.len());
    for file in files {
        let contents = match std::fs::read_to_string(file) {
            Ok(contents) => contents,
            Err(error) => {
                eprintln!("{}: {error}", file.display());
                return Err(74);
            }
        };
        match format_source(&contents, options) {
            Ok(source) => formatted.push((file, source)),
            Err(errors) => {
                for error in errors {
                    eprintln!("{}: {error}", file.display());
                }
                return Err(65);
            }
        }
    }
    if write {
        for (file, source) in formatted {
            if let Err(error) = write_in_place(file, &source) {
                eprintln!("{}: {error}", file.display());
                return Err(74);
            }
        }
    } else {
        let multiple = formatted.len() > 1;
        for (file, source) in formatted {
            if multiple {
                println!("==> {} <==", file.display());
            }
            print!("{source}");
        }
    }
    Ok(())
}

fn write_in_place(file: &Path, source: &str) -> std::io::Result<()> {
    std::fs::write(file, source)
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
    fn cache_flags_before_the_file() {
        let args = parse(&[
            "generic",
            "--no-cache",
            "--no-cache-read",
            "--no-cache-write",
            "script.gen",
        ]);
        assert!(args.no_cache);
        assert!(args.no_cache_read);
        assert!(args.no_cache_write);
        assert_eq!(args.file_and_arguments, ["script.gen"]);
    }

    #[test]
    fn no_optimize_flag_before_the_file() {
        let args = parse(&["generic", "--no-optimize", "script.gen", "alpha"]);
        assert!(args.no_optimize);
        assert_eq!(args.file_and_arguments, ["script.gen", "alpha"]);
    }

    #[test]
    fn optimize_flag_overrides_no_optimize() {
        let args = parse(&["generic", "--no-optimize", "--optimize", "script.gen"]);
        assert!(!args.no_optimize);
        assert!(args.optimize);
        assert_eq!(args.file_and_arguments, ["script.gen"]);
    }
    #[test]
    fn no_optimize_flag_overrides_optimize() {
        let args = parse(&["generic", "--optimize", "--no-optimize", "script.gen"]);
        assert!(args.no_optimize);
        assert!(!args.optimize);
        assert_eq!(args.file_and_arguments, ["script.gen"]);
    }

    #[test]
    fn cache_flags_after_the_file_go_to_the_script() {
        let args = parse(&[
            "generic",
            "script.gen",
            "--no-cache",
            "--no-cache-read",
            "--no-cache-write",
        ]);
        assert!(!args.no_cache);
        assert!(!args.no_cache_read);
        assert!(!args.no_cache_write);
        assert_eq!(
            args.file_and_arguments,
            [
                "script.gen",
                "--no-cache",
                "--no-cache-read",
                "--no-cache-write"
            ]
        );
    }

    #[test]
    fn no_optimize_after_the_file_goes_to_the_script() {
        let args = parse(&["generic", "script.gen", "--no-optimize"]);
        assert!(!args.no_optimize);
        assert_eq!(args.file_and_arguments, ["script.gen", "--no-optimize"]);
    }

    #[test]
    fn no_file_means_repl() {
        let args = parse(&["generic"]);
        assert!(!args.test);
        let expected: [String; 0] = [];
        assert_eq!(args.file_and_arguments, expected);
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
    fn fmt_subcommand_takes_files_and_defaults_to_stdout() {
        let args = parse(&["generic", "fmt", "script.gen", "other.gen"]);
        let Some(Command::Fmt {
            write,
            indent_width,
            line_length,
            files,
        }) = args.command
        else {
            panic!("expected the fmt subcommand");
        };
        assert!(!write);
        assert_eq!(indent_width, 4);
        assert_eq!(line_length, None);
        assert_eq!(
            files,
            [PathBuf::from("script.gen"), PathBuf::from("other.gen")]
        );
        assert_eq!(args.file_and_arguments, Vec::<String>::new());
    }

    #[test]
    fn fmt_write_flags_parse() {
        for flag in ["-w", "--write"] {
            let args = parse(&["generic", "fmt", flag, "script.gen"]);
            let Some(Command::Fmt { write, files, .. }) = args.command else {
                panic!("expected the fmt subcommand");
            };
            assert!(write);
            assert_eq!(files, [PathBuf::from("script.gen")]);
        }
    }

    #[test]
    fn fmt_line_length_parses() {
        let args = parse(&["generic", "fmt", "--line-length", "40", "script.gen"]);
        let Some(Command::Fmt { line_length, .. }) = args.command else {
            panic!("expected the fmt subcommand");
        };
        assert_eq!(line_length, Some(40));
    }

    #[test]
    fn fmt_indent_width_requires_at_least_one_space() {
        let args = parse(&["generic", "fmt", "--indent-width", "1", "script.gen"]);
        let Some(Command::Fmt { indent_width, .. }) = args.command else {
            panic!("expected the fmt subcommand");
        };
        assert_eq!(indent_width, 1);

        let error = Args::try_parse_from(["generic", "fmt", "--indent-width", "0", "script.gen"])
            .unwrap_err();
        assert_eq!(error.kind(), clap::error::ErrorKind::ValueValidation);
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
