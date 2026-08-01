//! Regression test for the circular-import guard panicking on a stdlib
//! module's bare path.
//!
//! Stdlib generic modules are keyed by their bare name (e.g. `argparse`), so
//! their module path is not a real filesystem path. The circular-import check
//! runs only when an import resolves to a file on disk, and it used to
//! misclassify such a stdlib module as user code and then call
//! `canonicalize().unwrap()` on its bare path - aborting the interpreter.
//!
//! It is reachable when a file in the working directory shadows a name that a
//! stdlib module imports internally: `argparse` imports `os`, so a file named
//! `os` next to the run makes that internal import hit the disk branch while
//! `argparse` sits on the module stack. The bug requires the process working
//! directory to contain the shadow, so the fixture is driven through a scratch
//! directory rather than the shared `.gen` suite.

// Miri cannot spawn processes. On 32-bit Unix under `cross`, QEMU runs the
// test harness but cannot spawn the cross-compiled binary as a subprocess
// (no binfmt_misc); Windows i686 handles it via WoW64.
#![cfg(not(miri))]
#![cfg(any(target_arch = "x86_64", target_os = "windows"))]

use std::process::Command;

use tempfile::TempDir;

#[test]
fn stdlib_internal_import_shadowed_in_cwd_does_not_panic() {
    let dir = TempDir::new().expect("failed to create the scratch directory");
    // The import string is the bare `os`, so the interpreter reads a file named
    // `os` (no `.gen` extension) from disk - that is what shadows the embedded
    // stdlib module.
    std::fs::write(dir.path().join("os"), "var shadowed = 1;\n")
        .expect("failed to write the shadow file");
    std::fs::write(
        dir.path().join("shadow.gen"),
        "import \"argparse\";\nprint(\"loaded\");\n",
    )
    .expect("failed to write the script fixture");

    let output = Command::new(env!("CARGO_BIN_EXE_generic"))
        .arg("shadow.gen")
        .current_dir(dir.path())
        .output()
        .expect("failed to spawn the generic binary");

    let code = output
        .status
        .code()
        .expect("the generic binary was killed by a signal");
    assert_eq!(
        code,
        0,
        "importing argparse aborted (code {code}); stderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "loaded\n");
}
