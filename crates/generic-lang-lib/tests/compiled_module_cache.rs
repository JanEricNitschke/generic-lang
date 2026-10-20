//! End-to-end coverage for the compiled imported-module disk cache.

use generic_lang_lib::{InterpretResult, RunOptions, run_file};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

const CACHE_ENABLED: RunOptions = RunOptions {
    cache_read: true,
    cache_write: true,
};

const OLD_TIMESTAMP_SECS: u64 = 1_000_000_000;

fn old_timestamp() -> SystemTime {
    UNIX_EPOCH + Duration::from_secs(OLD_TIMESTAMP_SECS)
}

fn run(script: &Path, options: RunOptions) -> InterpretResult {
    run_file(script.to_path_buf(), Vec::new(), options).unwrap()
}

fn write_module(directory: &Path) -> (PathBuf, PathBuf) {
    let module_path = directory.join("cached_module.gen");
    fs::write(&module_path, "var value = 40 + 2;\n").unwrap();
    let script_path = directory.join("main.gen");
    fs::write(&script_path, "import \"cached_module.gen\";\n").unwrap();
    (module_path, script_path)
}

fn cache_file(directory: &Path) -> PathBuf {
    let entries = fs::read_dir(directory.join("__generic_cache__"))
        .unwrap()
        .filter_map(Result::ok)
        .filter(|entry| entry.file_type().is_ok_and(|kind| kind.is_file()))
        .collect::<Vec<_>>();
    assert_eq!(entries.len(), 1, "expected exactly one cache file");
    entries[0].path()
}

fn set_modified(path: &Path, time: SystemTime) {
    fs::OpenOptions::new()
        .write(true)
        .open(path)
        .unwrap()
        .set_modified(time)
        .unwrap();
}

fn modified_secs(path: &Path) -> u64 {
    fs::metadata(path)
        .unwrap()
        .modified()
        .unwrap()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

#[test]
fn cache_hit_does_not_rewrite_the_cache() {
    let directory = tempfile::tempdir().unwrap();
    let (_, script_path) = write_module(directory.path());

    assert_eq!(run(&script_path, CACHE_ENABLED), InterpretResult::Ok);
    let cache_path = cache_file(directory.path());
    set_modified(&cache_path, old_timestamp());

    assert_eq!(run(&script_path, CACHE_ENABLED), InterpretResult::Ok);
    assert_eq!(modified_secs(&cache_path), OLD_TIMESTAMP_SECS);
}

#[test]
fn disabling_reads_recompiles_and_rewrites_the_cache() {
    let directory = tempfile::tempdir().unwrap();
    let (_, script_path) = write_module(directory.path());

    assert_eq!(run(&script_path, CACHE_ENABLED), InterpretResult::Ok);
    let cache_path = cache_file(directory.path());
    set_modified(&cache_path, old_timestamp());

    let no_read = RunOptions {
        cache_read: false,
        cache_write: true,
    };
    assert_eq!(run(&script_path, no_read), InterpretResult::Ok);
    assert_ne!(modified_secs(&cache_path), OLD_TIMESTAMP_SECS);
}

#[test]
fn disabling_writes_creates_no_cache() {
    let directory = tempfile::tempdir().unwrap();
    let (_, script_path) = write_module(directory.path());

    let no_write = RunOptions {
        cache_read: true,
        cache_write: false,
    };
    assert_eq!(run(&script_path, no_write), InterpretResult::Ok);
    assert!(!directory.path().join("__generic_cache__").exists());
}

#[test]
fn version_mismatch_recompiles_and_rewrites_the_cache() {
    let directory = tempfile::tempdir().unwrap();
    let (_, script_path) = write_module(directory.path());

    assert_eq!(run(&script_path, CACHE_ENABLED), InterpretResult::Ok);
    let cache_path = cache_file(directory.path());
    let version = env!("CARGO_PKG_VERSION")
        .split('.')
        .take(2)
        .collect::<Vec<_>>()
        .join(".");
    let mut replacement = version.clone().into_bytes();
    replacement[0] = if replacement[0] == b'9' { b'8' } else { b'9' };

    let mut bytes = fs::read(&cache_path).unwrap();
    let offset = bytes
        .windows(version.len())
        .position(|window| window == version.as_bytes())
        .unwrap();
    bytes[offset..offset + version.len()].copy_from_slice(&replacement);
    fs::write(&cache_path, bytes).unwrap();

    assert_eq!(run(&script_path, CACHE_ENABLED), InterpretResult::Ok);
    assert!(
        fs::read(&cache_path)
            .unwrap()
            .windows(version.len())
            .any(|window| window == version.as_bytes())
    );
}

#[cfg(debug_assertions)]
#[test]
fn debug_builds_do_not_cache_by_default() {
    let directory = tempfile::tempdir().unwrap();
    let (_, script_path) = write_module(directory.path());

    assert_eq!(
        run_file(script_path, Vec::new(), RunOptions::default()).unwrap(),
        InterpretResult::Ok
    );
    assert!(!directory.path().join("__generic_cache__").exists());
}

#[test]
fn files_with_the_same_stem_get_distinct_cache_entries() {
    let directory = tempfile::tempdir().unwrap();
    fs::write(directory.path().join("mod.gen"), "var value = 1;\n").unwrap();
    fs::write(
        directory.path().join("mod.generic"),
        "var value = 2;\nif value != 2 { throw Exception(\"wrong cache\"); }\n",
    )
    .unwrap();

    let first_script = directory.path().join("first.gen");
    fs::write(&first_script, "import \"mod.gen\";\n").unwrap();
    let second_script = directory.path().join("second.gen");
    fs::write(&second_script, "import \"mod.generic\";\n").unwrap();

    assert_eq!(run(&first_script, CACHE_ENABLED), InterpretResult::Ok);
    assert_eq!(run(&second_script, CACHE_ENABLED), InterpretResult::Ok);

    let entries = fs::read_dir(directory.path().join("__generic_cache__"))
        .unwrap()
        .filter_map(Result::ok)
        .filter(|entry| entry.file_type().is_ok_and(|kind| kind.is_file()))
        .count();
    assert_eq!(entries, 2, "same-stem files must not share a cache entry");
}

#[test]
fn tstring_modules_use_the_disk_cache() {
    let directory = tempfile::tempdir().unwrap();
    let module_path = directory.path().join("template_module.gen");
    fs::write(&module_path, "var template = t\"hello\";\n").unwrap();
    let script_path = directory.path().join("main.gen");
    fs::write(&script_path, "import \"template_module.gen\";\n").unwrap();

    assert_eq!(run(&script_path, CACHE_ENABLED), InterpretResult::Ok);
    assert!(cache_file(directory.path()).is_file());
    assert_eq!(run(&script_path, CACHE_ENABLED), InterpretResult::Ok);
}
