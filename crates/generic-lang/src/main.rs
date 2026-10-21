//! # Main entry point for the generic interpreter
//!
//! Uses the `clap` crate to parse command line arguments and then calls the appropriate function to either run the REPL or run a file.
//! Not specifying a file will run the REPL, otherwise it will run the file.
//! The file path ends interpreter option parsing: everything after it is
//! passed through to the script verbatim as `os.argv[1:]`.

#![forbid(unsafe_code)]
// Pre-existing duplicate transitive dependencies; nothing actionable per crate.
#![allow(clippy::multiple_crate_versions)]

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::LazyLock;

use clap::{Parser, Subcommand};
use generic_lang_lib::{
    FormatError, FormatOptions, InterpretResult, RunOptions, TestRunResult, format_source, repl,
    run_file, run_tests,
};
use walkdir::WalkDir;

/// Directories that recursive `fmt` discovery never descends into.
static EXCLUDED_DIRS: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    [
        ".bzr",
        ".direnv",
        ".eggs",
        ".git",
        ".git-rewrite",
        ".hg",
        ".mypy_cache",
        ".nox",
        ".pants.d",
        ".pytype",
        ".ruff_cache",
        ".svn",
        ".tox",
        ".venv",
        "__pypackages__",
        "__pycache__",
        "_build",
        "buck-out",
        "dist",
        "gen",
        "node_modules",
        "target",
        "venv",
    ]
    .into_iter()
    .collect()
});

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

        /// Files or directories to format. Directories are searched
        /// recursively for `.gen` files, skipping common build/VCS dirs.
        #[arg(value_name = "PATH", required = true)]
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
                files,
            } => {
                let options = FormatOptions {
                    indent_width: usize::from(indent_width),
                };
                let files = collect_format_files(&files);
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

/// Expand directories into the `.gen` files they contain, recursively.
/// Explicitly passed files are kept as-is. Duplicate paths are removed.
fn collect_format_files(paths: &[PathBuf]) -> Vec<PathBuf> {
    let mut files = Vec::new();
    let mut seen = HashSet::new();
    for path in paths {
        if path.is_dir() {
            let mut discovered: Vec<_> = WalkDir::new(path)
                .into_iter()
                .filter_entry(|entry| {
                    !entry.file_type().is_dir() || !is_excluded_name(entry.path())
                })
                .filter_map(Result::ok)
                .filter(|entry| entry.file_type().is_file())
                .map(walkdir::DirEntry::into_path)
                .filter(|path| {
                    path.extension()
                        .is_some_and(|extension| extension.eq_ignore_ascii_case("gen"))
                })
                .collect();
            discovered.sort();
            for file in discovered {
                if seen.insert(file.clone()) {
                    files.push(file);
                }
            }
        } else if seen.insert(path.clone()) {
            files.push(path.clone());
        }
    }
    files
}

fn is_excluded_name(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| EXCLUDED_DIRS.contains(name.to_ascii_lowercase().as_str()))
}

fn format_files(files: &[PathBuf], write: bool, options: FormatOptions) -> Result<(), i32> {
    let mut formatted = Vec::with_capacity(files.len());
    let mut errors = Vec::new();
    let mut had_io_error = false;
    let mut had_syntax_error = false;
    let mut had_internal_error = false;

    for file in files {
        match std::fs::read_to_string(file) {
            Ok(contents) => match format_source(&contents, options) {
                Ok(source) => {
                    let changed = source != contents;
                    formatted.push((file, source, changed));
                }
                Err(FormatError::Syntax(format_errors)) => {
                    had_syntax_error = true;
                    errors.extend(
                        format_errors
                            .into_iter()
                            .map(|error| format!("{}: {error}", file.display())),
                    );
                }
                Err(FormatError::Internal(format_errors)) => {
                    had_internal_error = true;
                    errors.extend(
                        format_errors
                            .into_iter()
                            .map(|error| format!("{}: {error}", file.display())),
                    );
                }
            },
            Err(error) => {
                had_io_error = true;
                errors.push(format!("{}: {error}", file.display()));
            }
        }
    }

    if write {
        for (file, source, changed) in formatted {
            if !changed {
                continue;
            }
            if let Err(error) = write_in_place(file, &source) {
                had_io_error = true;
                errors.push(format!("{}: {error}", file.display()));
            }
        }
    } else {
        let multiple = formatted.len() > 1;
        for (file, source, _) in formatted {
            if multiple {
                println!("==> {} <==", file.display());
            }
            print!("{source}");
        }
    }

    for error in errors {
        eprintln!("{error}");
    }
    if had_io_error {
        Err(74)
    } else if had_internal_error {
        // A formatter bug, not a problem with the input.
        Err(70)
    } else if had_syntax_error {
        Err(65)
    } else {
        Ok(())
    }
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
            files,
        }) = args.command
        else {
            panic!("expected the fmt subcommand");
        };
        assert!(!write);
        assert_eq!(indent_width, 4);
        assert_eq!(
            files,
            [PathBuf::from("script.gen"), PathBuf::from("other.gen")]
        );
        assert!(args.file_and_arguments.is_empty());
    }

    #[test]
    #[cfg(not(miri))]
    fn fmt_directory_discovers_gen_files_and_skips_excluded_dirs() {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path();
        for dir in [
            "a/nested",
            ".git",
            "__pycache__",
            "node_modules/pkg",
            "target",
            "gen",
            "dist",
            ".venv",
        ] {
            std::fs::create_dir_all(root.join(dir)).unwrap();
        }
        for file in [
            "a/one.gen",
            "a/nested/two.gen",
            ".git/skipped.gen",
            "__pycache__/skipped.gen",
            "node_modules/pkg/skipped.gen",
            "target/skipped.gen",
            "gen/skipped.gen",
            "dist/skipped.gen",
            ".venv/skipped.gen",
            "a/notes.txt",
        ] {
            std::fs::write(root.join(file), "").unwrap();
        }

        let files = collect_format_files(&[root.to_path_buf()]);
        let found: HashSet<_> = files
            .iter()
            .map(|path| path.strip_prefix(root).unwrap().to_path_buf())
            .collect();
        assert_eq!(
            found,
            HashSet::from([
                PathBuf::from("a/one.gen"),
                PathBuf::from("a/nested/two.gen"),
            ])
        );
    }

    #[test]
    #[cfg(not(miri))]
    fn fmt_explicit_files_are_kept_even_without_gen_extension() {
        let directory = tempfile::tempdir().unwrap();
        let file = directory.path().join("script.txt");
        std::fs::write(&file, "").unwrap();

        assert_eq!(collect_format_files(std::slice::from_ref(&file)), [file]);
    }

    #[test]
    #[cfg(not(miri))]
    fn fmt_invalid_input_exits_data_error() {
        let directory = tempfile::tempdir().unwrap();
        let file = directory.path().join("invalid.gen");
        std::fs::write(&file, "var").unwrap();

        assert_eq!(
            format_files(&[file], false, FormatOptions::default()),
            Err(65)
        );
    }

    #[test]
    #[cfg(not(miri))]
    fn fmt_missing_file_exits_io_error() {
        let directory = tempfile::tempdir().unwrap();
        let file = directory.path().join("missing.gen");

        assert_eq!(
            format_files(&[file], false, FormatOptions::default()),
            Err(74)
        );
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
