//! Process-level checks for `generic fmt`, including safe in-place writes.

// Miri cannot spawn processes. Some cross targets also cannot execute the
// cross-compiled binary from the test harness.
#![cfg(not(miri))]

use std::{
    fs,
    path::Path,
    process::Command,
    sync::OnceLock,
    time::{Duration, UNIX_EPOCH},
};

#[cfg(unix)]
use std::os::unix::fs::{MetadataExt, PermissionsExt, symlink};

fn can_run_generic() -> bool {
    static RUNNABLE: OnceLock<bool> = OnceLock::new();
    *RUNNABLE.get_or_init(|| {
        Command::new(env!("CARGO_BIN_EXE_generic"))
            .arg("--version")
            .output()
            .is_ok_and(|output| output.status.success())
    })
}

fn run_fmt(args: &[&str]) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_generic"))
        .arg("fmt")
        .args(args)
        .output()
        .expect("failed to spawn the generic binary")
}

fn path_arg(path: &Path) -> &str {
    path.to_str().expect("temporary paths should be UTF-8")
}

#[test]
fn fmt_write_rewrites_in_place() {
    if !can_run_generic() {
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("script.gen");
    fs::write(&path, "var x=1;\n").unwrap();

    let output = run_fmt(&["-w", path_arg(&path)]);

    assert!(output.status.success());
    assert!(output.stdout.is_empty());
    assert!(output.stderr.is_empty());
    assert_eq!(fs::read_to_string(&path).unwrap(), "var x = 1;\n");
    assert_eq!(fs::read_dir(directory.path()).unwrap().count(), 1);
}

#[test]
fn fmt_write_preserves_mtime_for_unchanged_files() {
    if !can_run_generic() {
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("script.gen");
    fs::write(&path, "var x = 1;\n").unwrap();
    let old = UNIX_EPOCH + Duration::from_secs(1_000_000_000);
    fs::OpenOptions::new()
        .write(true)
        .open(&path)
        .unwrap()
        .set_times(fs::FileTimes::new().set_modified(old))
        .unwrap();
    let before = fs::metadata(&path).unwrap().modified().unwrap();

    let output = run_fmt(&["-w", path_arg(&path)]);

    assert!(output.status.success());
    assert_eq!(output.stdout, Vec::<u8>::new());
    assert_eq!(fs::read_to_string(&path).unwrap(), "var x = 1;\n");
    assert_eq!(fs::metadata(&path).unwrap().modified().unwrap(), before);
}

#[test]
fn fmt_write_handles_multiple_files() {
    if !can_run_generic() {
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let first = directory.path().join("first.gen");
    let second = directory.path().join("second.gen");
    fs::write(&first, "var a=1;\n").unwrap();
    fs::write(&second, "var b=2;\n").unwrap();

    let output = run_fmt(&["--write", path_arg(&first), path_arg(&second)]);

    assert!(output.status.success());
    assert_eq!(fs::read_to_string(first).unwrap(), "var a = 1;\n");
    assert_eq!(fs::read_to_string(second).unwrap(), "var b = 2;\n");
}

#[test]
fn fmt_prints_headers_for_multiple_files() {
    if !can_run_generic() {
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let first = directory.path().join("first.gen");
    let second = directory.path().join("second.gen");
    fs::write(&first, "var a=1;\n").unwrap();
    fs::write(&second, "var b=2;\n").unwrap();

    let output = run_fmt(&[path_arg(&first), path_arg(&second)]);

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        format!(
            "==> {} <==\nvar a = 1;\n==> {} <==\nvar b = 2;\n",
            first.display(),
            second.display()
        )
    );
}

#[cfg(unix)]
#[test]
fn fmt_write_preserves_file_permissions() {
    if !can_run_generic() {
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("script.gen");
    fs::write(&path, "var x=1;\n").unwrap();
    fs::set_permissions(&path, fs::Permissions::from_mode(0o754)).unwrap();

    let output = run_fmt(&["-w", path_arg(&path)]);

    assert!(output.status.success());
    assert_eq!(
        fs::metadata(&path).unwrap().permissions().mode() & 0o777,
        0o754
    );
}

#[test]
fn fmt_error_identifies_the_failing_file() {
    if !can_run_generic() {
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let good = directory.path().join("good.gen");
    let bad = directory.path().join("bad.gen");
    fs::write(&good, "var x=1;\n").unwrap();
    fs::write(&bad, "var = ;\n").unwrap();

    let output = run_fmt(&[path_arg(&good), path_arg(&bad)]);

    assert_eq!(output.status.code(), Some(65));
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "var x = 1;\n");
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(stderr.contains(path_arg(&bad)));
}

#[test]
fn fmt_write_continues_after_parse_error() {
    if !can_run_generic() {
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let first = directory.path().join("first.gen");
    let bad = directory.path().join("bad.gen");
    let second = directory.path().join("second.gen");
    fs::write(&first, "var a=1;\n").unwrap();
    fs::write(&bad, "var = ;\n").unwrap();
    fs::write(&second, "var b=2;\n").unwrap();

    let output = run_fmt(&["-w", path_arg(&first), path_arg(&bad), path_arg(&second)]);

    assert_eq!(output.status.code(), Some(65));
    assert_eq!(fs::read_to_string(first).unwrap(), "var a = 1;\n");
    assert_eq!(fs::read_to_string(&bad).unwrap(), "var = ;\n");
    assert_eq!(fs::read_to_string(second).unwrap(), "var b = 2;\n");
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(stderr.contains(path_arg(&bad)));
}

#[test]
fn fmt_reports_every_file_error() {
    if !can_run_generic() {
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let first_bad = directory.path().join("first_bad.gen");
    let second_bad = directory.path().join("second_bad.gen");
    fs::write(&first_bad, "var = ;\n").unwrap();
    fs::write(&second_bad, "fun f( {\n").unwrap();

    let output = run_fmt(&["-w", path_arg(&first_bad), path_arg(&second_bad)]);

    assert_eq!(output.status.code(), Some(65));
    assert_eq!(fs::read_to_string(&first_bad).unwrap(), "var = ;\n");
    assert_eq!(fs::read_to_string(&second_bad).unwrap(), "fun f( {\n");
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(stderr.contains(path_arg(&first_bad)));
    assert!(stderr.contains(path_arg(&second_bad)));
}

#[test]
fn fmt_io_error_takes_precedence_and_still_formats_other_files() {
    if !can_run_generic() {
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let missing = directory.path().join("missing.gen");
    let bad = directory.path().join("bad.gen");
    let good = directory.path().join("good.gen");
    fs::write(&bad, "var = ;\n").unwrap();
    fs::write(&good, "var x=1;\n").unwrap();

    let output = run_fmt(&["-w", path_arg(&missing), path_arg(&bad), path_arg(&good)]);

    assert_eq!(output.status.code(), Some(74));
    assert_eq!(fs::read_to_string(&bad).unwrap(), "var = ;\n");
    assert_eq!(fs::read_to_string(good).unwrap(), "var x = 1;\n");
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(stderr.contains(path_arg(&missing)));
    assert!(stderr.contains(path_arg(&bad)));
}

#[cfg(unix)]
#[test]
fn fmt_write_follows_symlinks() {
    if !can_run_generic() {
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let target = directory.path().join("target.gen");
    let link = directory.path().join("link.gen");
    fs::write(&target, "var x=1;\n").unwrap();
    symlink(&target, &link).unwrap();

    let output = run_fmt(&["-w", path_arg(&link)]);

    assert!(output.status.success());
    assert!(
        fs::symlink_metadata(&link)
            .unwrap()
            .file_type()
            .is_symlink()
    );
    assert_eq!(fs::read_to_string(&target).unwrap(), "var x = 1;\n");
}

#[cfg(unix)]
#[test]
fn fmt_write_preserves_hard_links() {
    if !can_run_generic() {
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let original = directory.path().join("original.gen");
    let link = directory.path().join("link.gen");
    fs::write(&original, "var x=1;\n").unwrap();
    fs::hard_link(&original, &link).unwrap();

    let output = run_fmt(&["-w", path_arg(&original)]);

    assert!(output.status.success());
    assert_eq!(fs::read_to_string(&original).unwrap(), "var x = 1;\n");
    assert_eq!(fs::read_to_string(&link).unwrap(), "var x = 1;\n");
    assert_eq!(
        fs::metadata(&original).unwrap().ino(),
        fs::metadata(&link).unwrap().ino()
    );
}

#[test]
fn fmt_write_parse_error_leaves_the_file_untouched() {
    if !can_run_generic() {
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("broken.gen");
    let original = "var = ;\n";
    fs::write(&path, original).unwrap();

    let output = run_fmt(&["-w", path_arg(&path)]);

    assert_eq!(output.status.code(), Some(65));
    assert_eq!(fs::read_to_string(&path).unwrap(), original);
    assert_eq!(fs::read_dir(directory.path()).unwrap().count(), 1);
}

#[test]
fn fmt_without_write_prints_and_leaves_the_file_untouched() {
    if !can_run_generic() {
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("script.gen");
    let original = "var x=1;\n";
    fs::write(&path, original).unwrap();

    let output = run_fmt(&[path_arg(&path)]);

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "var x = 1;\n");
    assert_eq!(fs::read_to_string(path).unwrap(), original);
}
