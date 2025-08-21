//! Unit testing functionality for Generic language.
//!
//! Provides functions to discover and run test files, both individually
//! and in directory hierarchies.

use crate::vm::{InterpretResult, TestResult, VM};
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

/// Run tests from a file or directory.
///
/// If a file is provided, compiles and executes it, then discovers and runs all test functions.
/// If a directory is provided, finds all .gen files whose names contain "test" and runs tests on each.
/// Test functions are those whose names start with "test_".
/// Provides a summary report of test results.
pub fn run_tests(path: &PathBuf) {
    if path.is_file() {
        run_tests_for_file(path);
    } else if path.is_dir() {
        run_tests_for_directory(path);
    } else {
        eprintln!(
            "Error: {} is not a valid file or directory",
            path.to_string_lossy()
                .replace('\\', "/")
                .trim_start_matches("./")
        );
        std::process::exit(74);
    }
}

/// Result of running tests in a single file
pub struct FileTestResult {
    pub test_results: TestResult,
    pub had_error: bool,
}

/// Run tests for a single file
fn run_tests_for_file(file: &PathBuf) {
    println!(
        "Running tests in {}...\n",
        file.to_string_lossy()
            .replace('\\', "/")
            .trim_start_matches("./")
    );
    let result = run_tests_for_single_file(file);

    // Print test summary
    print_test_summary(&result.test_results);

    // Exit with error code if any tests failed
    if result.had_error {
        std::process::exit(1);
    }
}

/// Run tests for all test files in a directory
fn run_tests_for_directory(dir: &PathBuf) {
    let test_files = find_test_files(dir);

    if test_files.is_empty() {
        println!(
            "No test files found in {}",
            dir.to_string_lossy()
                .replace('\\', "/")
                .trim_start_matches("./")
        );
        println!("Looking for .gen files that contain 'test' in the name");
        return;
    }

    println!(
        "Found {} test file(s) in {}...\n",
        test_files.len(),
        dir.to_string_lossy()
            .replace('\\', "/")
            .trim_start_matches("./")
    );

    let mut overall_results = TestResult::default();
    let mut any_failures = false;

    for test_file in test_files {
        println!(
            "Running tests in {}...",
            test_file
                .to_string_lossy()
                .replace('\\', "/")
                .trim_start_matches("./")
        );

        let result = run_tests_for_single_file(&test_file);

        // Accumulate results
        overall_results.passed += result.test_results.passed;
        overall_results.errors += result.test_results.errors;
        overall_results.failed += result.test_results.failed;

        if result.had_error {
            any_failures = true;
        }

        // Print file summary if there were tests
        if result.test_results.total() > 0 {
            print_file_test_summary(&result.test_results, &test_file);
        }
        println!();
    }

    // Print overall summary
    println!("=== Overall Test Results ===");
    print_test_summary(&overall_results);

    if any_failures {
        std::process::exit(1);
    }
}

/// Run tests for a single file and return the results
fn run_tests_for_single_file(file: &PathBuf) -> FileTestResult {
    match std::fs::read_to_string(file) {
        Err(e) => {
            eprintln!(
                "Error reading {}: {e}",
                file.to_string_lossy()
                    .replace('\\', "/")
                    .trim_start_matches("./")
            );
            FileTestResult {
                test_results: TestResult::default(),
                had_error: true,
            }
        }
        Ok(contents) => {
            let mut vm = VM::new();
            match vm.interpret(&contents, file.clone()) {
                InterpretResult::CompileError => {
                    eprintln!(
                        "Compile error in {}",
                        file.to_string_lossy()
                            .replace('\\', "/")
                            .trim_start_matches("./")
                    );
                    FileTestResult {
                        test_results: TestResult::default(),
                        had_error: true,
                    }
                }
                InterpretResult::RuntimeError => {
                    eprintln!(
                        "Runtime error in {}",
                        file.to_string_lossy()
                            .replace('\\', "/")
                            .trim_start_matches("./")
                    );
                    FileTestResult {
                        test_results: TestResult::default(),
                        had_error: true,
                    }
                }
                InterpretResult::Ok => {
                    let test_results = vm.run_tests();
                    let had_error = test_results.errors > 0;
                    FileTestResult {
                        test_results,
                        had_error,
                    }
                }
            }
        }
    }
}

/// Find all test files in a directory recursively (files that contain "test" in the name before the .gen extension)
fn find_test_files(dir: &PathBuf) -> Vec<PathBuf> {
    let mut files: Vec<_> = WalkDir::new(dir)
        .into_iter()
        .filter_map(std::result::Result::ok) // skip entries with errors
        .filter(|e| e.file_type().is_file())
        .filter_map(|e| {
            let path = e.path();
            let file_name = path.file_name()?.to_str()?;
            if file_name.starts_with("test_")
                && path
                    .extension()
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("gen"))
            {
                Some(path.to_path_buf())
            } else {
                None
            }
        })
        .collect();

    files.sort();
    files
}

/// Print a summary of test results for a specific file
fn print_file_test_summary(results: &TestResult, file: &Path) {
    println!(
        "  {} tests in {}: {} passed",
        results.total(),
        file.to_string_lossy()
            .replace('\\', "/")
            .trim_start_matches("./"),
        results.passed
    );

    if results.failed > 0 {
        println!("    {} failures", results.failed);
    }

    if results.errors > 0 {
        println!("    {} errors", results.errors);
    }
}

/// Print a summary of test results
pub fn print_test_summary(results: &TestResult) {
    println!("=== Test Results ===");

    if results.total() == 0 {
        println!("No test functions found.");
        return;
    }

    println!("Tests run: {}", results.total());
    if results.passed > 0 {
        println!("✓ Passed: {}", results.passed);
    }
    if results.failed > 0 {
        println!("⚠ Failed: {}", results.failed);
    }
    if results.errors > 0 {
        println!("❌ Errors: {}", results.errors);
    }

    println!();
    if results.errors == 0 && results.failed == 0 {
        println!("All tests passed! ✓");
    } else {
        println!("Some tests failed or had errors.");
    }
}
