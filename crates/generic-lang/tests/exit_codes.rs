//! Process-level checks of the exit-code contract of both run modes,
//! exercising the real binary the way a user (or CI gating on it) does.
//! The `--test` `.gen` fixtures are the same ones the dart unittest suites
//! pin output for; these tests pin the exit codes (documented in
//! `docs/language/testing.md`). The normal-run scripts are tiny inline
//! fixtures written to a scratch directory.

// Miri cannot spawn processes.
#![cfg(not(miri))]

use std::path::Path;
use std::process::Command;

/// Repo-relative path of a fixture, anchored at this crate's manifest.
fn fixture(path: &str) -> String {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(path)
        .to_string_lossy()
        .into_owned()
}

/// Run the real binary with the given arguments and return its exit code.
fn run_generic(args: &[&str]) -> i32 {
    Command::new(env!("CARGO_BIN_EXE_generic"))
        .args(args)
        .output()
        .expect("failed to spawn the generic binary")
        .status
        .code()
        .expect("the generic binary was killed by a signal")
}

#[test]
fn passing_test_file_exits_zero() {
    let code = run_generic(&["--test", &fixture("unittest_test/test_basic.gen")]);
    assert_eq!(code, 0);
}

#[test]
fn failing_test_file_exits_one() {
    // FAIL outcomes alone (no ERROR) must produce exit code 1.
    let code = run_generic(&[
        "--test",
        &fixture("unittest_test/failures/test_failures.gen"),
    ]);
    assert_eq!(code, 1);
}

#[test]
fn erroring_test_file_exits_one() {
    let code = run_generic(&["--test", &fixture("unittest_test/errors/test_errors.gen")]);
    assert_eq!(code, 1);
}

#[test]
fn directory_with_failures_exits_one() {
    let code = run_generic(&["--test", &fixture("unittest_test")]);
    assert_eq!(code, 1);
}

#[test]
fn test_flag_without_a_path_exits_sixty_four() {
    let code = run_generic(&["--test"]);
    assert_eq!(code, 64);
}

#[test]
fn missing_path_exits_seventy_four() {
    let code = run_generic(&["--test", &fixture("unittest_test/no_such_file.gen")]);
    assert_eq!(code, 74);
}

// --- normal runs (no --test) ------------------------------------------

/// Write a one-off script into a scratch directory and return its path.
fn write_script(name: &str, content: &str) -> String {
    let dir = std::env::temp_dir().join("generic-lang-exit-codes");
    std::fs::create_dir_all(&dir).expect("failed to create the scratch directory");
    let path = dir.join(name);
    std::fs::write(&path, content).expect("failed to write the script fixture");
    path.to_string_lossy().into_owned()
}

#[test]
fn clean_script_exits_zero() {
    let script = write_script("clean.gen", "print(\"ok\");\n");
    assert_eq!(run_generic(&[&script]), 0);
}

#[test]
fn compile_error_exits_sixty_five() {
    let script = write_script("compile_error.gen", "fun {\n");
    assert_eq!(run_generic(&[&script]), 65);
}

#[test]
fn uncaught_exception_exits_seventy() {
    let script = write_script("uncaught.gen", "throw ValueError(\"boom\");\n");
    assert_eq!(run_generic(&[&script]), 70);
}

#[test]
fn missing_script_exits_seventy_four() {
    let dir = std::env::temp_dir().join("generic-lang-exit-codes");
    let missing = dir.join("no_such_script.gen");
    assert_eq!(run_generic(&[&missing.to_string_lossy()]), 74);
}
