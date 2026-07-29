//! The `pathlib` stdlib module: a `Path` native class over `std::path`.
//!
//! Paths join with `/` (operator or method), expose their components,
//! query the filesystem, read and write file contents, and iterate
//! directories. Filesystem errors raise `IoError`.

// Native functions must return `VmResult<Value>` to match the calling
// convention, so infallible ones still wrap in `Ok`.
#![allow(clippy::unnecessary_wraps)]

use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Component, Path as StdPath, PathBuf};

use rustc_hash::FxHasher;

use crate::value::{Instance, List, ModuleContents, ModuleExport, NativeClass, Path, Value};
use crate::vm::ExceptionKind::{IoError, TypeError};
use crate::vm::VM;
use crate::vm::errors::VmResult;

/// Lexically normalize a path without touching the filesystem: drop `.`
/// components and redundant/trailing separators, but KEEP `..` (folding
/// it is unsound under symlinks) - matching the familiar `PurePath`
/// normalization. An all-`.`/empty path normalizes to `.`.
fn normalize(path: &StdPath) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            other => normalized.push(other.as_os_str()),
        }
    }
    if normalized.as_os_str().is_empty() {
        normalized.push(".");
    }
    normalized
}

/// Build a `Path` instance from a path, normalizing it first.
fn make_path(vm: &mut VM, path: &StdPath) -> Value {
    let instance = Instance::new(
        *vm.heap.native_classes.get("Path").unwrap(),
        Some(
            Path {
                path: normalize(path),
            }
            .into(),
        ),
    );
    vm.heap.add_instance(instance)
}

/// Decode a `Path` instance or a plain string into a `PathBuf`.
fn as_path_buf(vm: &mut VM, value: Value, what: &str) -> VmResult<PathBuf> {
    match value {
        Value::String(string_id) => Ok(PathBuf::from(vm.heap.strings[string_id].as_str())),
        Value::Instance(id)
            if matches!(&id.to_value(&vm.heap).backing, Some(NativeClass::Path(_))) =>
        {
            Ok(value.as_path(&vm.heap).path.clone())
        }
        other => Err(vm
            .throw(
                TypeError,
                &format!(
                    "{what} expects a path or string, got: {}",
                    other.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

fn io_error(vm: &mut VM, action: &str, path: &StdPath, error: &std::io::Error) -> VmResult<Value> {
    Err(vm
        .throw(
            IoError,
            &format!("Could not {action} `{}`: {error}", path.display()),
        )
        .unwrap_err())
}

/// `Path(value)` - a path from a string or another path (normalized).
fn path_init_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let path = normalize(&as_path_buf(vm, args[0], "'Path'")?);
    let instance_id = *receiver.as_instance();
    instance_id.to_value_mut(&mut vm.heap).backing = Some(Path { path }.into());
    Ok(*receiver)
}

/// `p.join(other)` and `p / other` - append a component.
fn path_join_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let base = receiver.as_path(&vm.heap).path.clone();
    let other = as_path_buf(vm, args[0], "'join'")?;
    Ok(make_path(vm, &base.join(other)))
}

/// `p.parent()` - the path without its final component.
fn path_parent_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let parent = receiver
        .as_path(&vm.heap)
        .path
        .parent()
        .map_or_else(PathBuf::new, StdPath::to_path_buf);
    Ok(make_path(vm, &parent))
}

/// `p.name()` - the final component as a string.
fn path_name_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let name = receiver
        .as_path(&vm.heap)
        .path
        .file_name()
        .map_or_else(String::new, |name| name.to_string_lossy().into_owned());
    Ok(vm.heap.string_id(&name).into())
}

/// `p.suffix()` - the final component's extension (with the dot), or "".
fn path_suffix_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let suffix = receiver
        .as_path(&vm.heap)
        .path
        .extension()
        .map_or_else(String::new, |ext| format!(".{}", ext.to_string_lossy()));
    Ok(vm.heap.string_id(&suffix).into())
}

/// `p.stem()` - the final component without its extension.
fn path_stem_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let stem = receiver
        .as_path(&vm.heap)
        .path
        .file_stem()
        .map_or_else(String::new, |stem| stem.to_string_lossy().into_owned());
    Ok(vm.heap.string_id(&stem).into())
}

/// `p.exists()` - whether the path exists on the filesystem.
fn path_exists_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_path(&vm.heap).path.exists().into())
}

/// `p.is_file()` - whether the path is an existing regular file.
fn path_is_file_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_path(&vm.heap).path.is_file().into())
}

/// `p.is_dir()` - whether the path is an existing directory.
fn path_is_dir_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_path(&vm.heap).path.is_dir().into())
}

/// `p.to_str()` - the raw path text (what `str(p)` wraps in `Path(...)`).
fn path_to_str_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let text = receiver
        .as_path(&vm.heap)
        .path
        .to_string_lossy()
        .into_owned();
    Ok(vm.heap.string_id(&text).into())
}

/// `p.read_text()` - the file contents as a string.
fn path_read_text_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    match fs::read_to_string(&receiver.as_path(&vm.heap).path) {
        Ok(contents) => Ok(vm.heap.string_id(&contents).into()),
        Err(error) => {
            let path = receiver.as_path(&vm.heap).path.clone();
            io_error(vm, "read", &path, &error)
        }
    }
}

/// `p.write_text(contents)` - write a string, replacing any content.
fn path_write_text_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let Value::String(contents_id) = args[0] else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'write_text' expects a string, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    match fs::write(
        &receiver.as_path(&vm.heap).path,
        vm.heap.strings[contents_id].as_bytes(),
    ) {
        Ok(()) => Ok(Value::Nil),
        Err(error) => {
            let path = receiver.as_path(&vm.heap).path.clone();
            io_error(vm, "write", &path, &error)
        }
    }
}

/// `p.iterdir()` - a list of the directory's entries as paths.
fn path_iterdir_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let entries = match fs::read_dir(&receiver.as_path(&vm.heap).path) {
        Ok(entries) => entries,
        Err(error) => {
            let path = receiver.as_path(&vm.heap).path.clone();
            return io_error(vm, "read directory", &path, &error);
        }
    };
    let mut children = Vec::new();
    for entry in entries {
        match entry {
            Ok(entry) => children.push(make_path(vm, &entry.path())),
            Err(error) => {
                let path = receiver.as_path(&vm.heap).path.clone();
                return io_error(vm, "read directory entry of", &path, &error);
            }
        }
    }
    let instance = Instance::new(
        *vm.heap.native_classes.get("List").unwrap(),
        Some(List::new(children).into()),
    );
    Ok(vm.heap.add_instance(instance))
}

/// `p.parts()` - the path components as a list of strings, the root (if
/// any) included: `Path("a/b").parts()` is `["a", "b"]`,
/// `Path("/a").parts()` is `["/", "a"]`.
fn path_parts_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    // Collect the component texts first (borrowing the path), then intern
    // them (borrowing the heap mutably) - no path clone needed.
    let texts: Vec<String> = receiver
        .as_path(&vm.heap)
        .path
        .components()
        .map(|component| component.as_os_str().to_string_lossy().into_owned())
        .collect();
    let parts: Vec<Value> = texts
        .iter()
        .map(|text| vm.heap.string_id(text).into())
        .collect();
    let instance = Instance::new(
        *vm.heap.native_classes.get("List").unwrap(),
        Some(List::new(parts).into()),
    );
    Ok(vm.heap.add_instance(instance))
}

/// `p.mkdir()` - create the directory, including any missing parents;
/// succeeds if it already exists.
fn path_mkdir_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    match fs::create_dir_all(&receiver.as_path(&vm.heap).path) {
        Ok(()) => Ok(Value::Nil),
        Err(error) => {
            let path = receiver.as_path(&vm.heap).path.clone();
            io_error(vm, "create directory", &path, &error)
        }
    }
}

/// `p.rmdir()` / `p.rmdir(false)` - remove an empty directory;
/// `p.rmdir(true)` - remove the directory and everything under it.
fn path_rmdir_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let recursive = match args.first() {
        None | Some(Value::Bool(false)) => false,
        Some(Value::Bool(true)) => true,
        Some(other) => {
            return Err(vm
                .throw(
                    TypeError,
                    &format!("'rmdir' expects a bool, got: {}", other.to_string(&vm.heap)),
                )
                .unwrap_err());
        }
    };
    let result = if recursive {
        fs::remove_dir_all(&receiver.as_path(&vm.heap).path)
    } else {
        fs::remove_dir(&receiver.as_path(&vm.heap).path)
    };
    match result {
        Ok(()) => Ok(Value::Nil),
        Err(error) => {
            let path = receiver.as_path(&vm.heap).path.clone();
            io_error(vm, "remove directory", &path, &error)
        }
    }
}

/// `p == other` - path equality against another path or a string, both
/// compared in their normalized forms.
fn path_eq_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let other = match args[0] {
        Value::String(string_id) => normalize(StdPath::new(&vm.heap.strings[string_id])),
        other @ Value::Instance(id)
            if matches!(&id.to_value(&vm.heap).backing, Some(NativeClass::Path(_))) =>
        {
            other.as_path(&vm.heap).path.clone()
        }
        _ => return Ok(false.into()),
    };
    Ok((receiver.as_path(&vm.heap).path == other).into())
}

/// `hash(p)` - equal paths (equal normalized forms) hash equal, so paths
/// work as set members and dict keys.
fn path_hash_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let mut hasher = FxHasher::default();
    receiver.as_path(&vm.heap).path.hash(&mut hasher);
    Ok(hasher.finish().cast_signed().into())
}

pub(super) fn register(vm: &mut VM) {
    vm.define_native_class(&"Path", false);
    vm.define_native_method(&"Path", &"__init__", &[1], path_init_native);
    vm.define_native_method(&"Path", &"join", &[1], path_join_native);
    vm.define_native_method(&"Path", &"__div__", &[1], path_join_native);
    vm.define_native_method(&"Path", &"__eq__", &[1], path_eq_native);
    vm.define_native_method(&"Path", &"__hash__", &[0], path_hash_native);
    vm.define_native_method(&"Path", &"parent", &[0], path_parent_native);
    vm.define_native_method(&"Path", &"name", &[0], path_name_native);
    vm.define_native_method(&"Path", &"suffix", &[0], path_suffix_native);
    vm.define_native_method(&"Path", &"stem", &[0], path_stem_native);
    vm.define_native_method(&"Path", &"exists", &[0], path_exists_native);
    vm.define_native_method(&"Path", &"is_file", &[0], path_is_file_native);
    vm.define_native_method(&"Path", &"is_dir", &[0], path_is_dir_native);
    vm.define_native_method(&"Path", &"to_str", &[0], path_to_str_native);
    vm.define_native_method(&"Path", &"read_text", &[0], path_read_text_native);
    vm.define_native_method(&"Path", &"write_text", &[1], path_write_text_native);
    vm.define_native_method(&"Path", &"iterdir", &[0], path_iterdir_native);
    vm.define_native_method(&"Path", &"parts", &[0], path_parts_native);
    vm.define_native_method(&"Path", &"mkdir", &[0], path_mkdir_native);
    vm.define_native_method(&"Path", &"rmdir", &[0, 1], path_rmdir_native);
    vm.register_stdlib_module(&"pathlib", module());
}

/// Export all the contents of the module with the
/// name they are to be accessed with from generic.
fn module() -> ModuleContents {
    vec![ModuleExport::Class { name: "Path" }]
}

// Real filesystem access: not runnable under `cargo miri test`.
#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn new_path(vm: &mut VM, path: &StdPath) -> Value {
        make_path(vm, path)
    }

    #[test]
    fn write_then_read_round_trips() {
        let mut vm = VM::new();
        let dir = TempDir::new().unwrap();
        let file = new_path(&mut vm, &dir.path().join("note.txt"));
        let contents = Value::String(vm.heap.string_id(&"hello file".to_string()));
        assert_eq!(
            path_write_text_native(&mut vm, &file, &[contents]).unwrap(),
            Value::Nil
        );
        let read = path_read_text_native(&mut vm, &file, &[]).unwrap();
        let Value::String(read_id) = read else {
            panic!("read_text returned a non-string");
        };
        assert_eq!(vm.heap.strings[read_id], "hello file");
    }

    #[test]
    fn iterdir_lists_entries() {
        let mut vm = VM::new();
        let dir = TempDir::new().unwrap();
        fs::write(dir.path().join("a.txt"), "a").unwrap();
        fs::write(dir.path().join("b.txt"), "b").unwrap();
        let dir_path = new_path(&mut vm, dir.path());
        let listing = path_iterdir_native(&mut vm, &dir_path, &[]).unwrap();
        let Value::Instance(id) = listing else {
            panic!("iterdir returned a non-instance");
        };
        let Some(NativeClass::List(list)) = &id.to_value(&vm.heap).backing else {
            panic!("iterdir returned a non-list");
        };
        assert_eq!(list.items.len(), 2);
    }

    #[test]
    fn mkdir_creates_parents_and_is_idempotent() {
        let mut vm = VM::new();
        let base = TempDir::new().unwrap();
        let nested = new_path(&mut vm, &base.path().join("a/b/c"));
        assert_eq!(
            path_mkdir_native(&mut vm, &nested, &[]).unwrap(),
            Value::Nil
        );
        assert!(base.path().join("a/b/c").is_dir());
        // A second call on an existing directory still succeeds.
        assert_eq!(
            path_mkdir_native(&mut vm, &nested, &[]).unwrap(),
            Value::Nil
        );
    }

    #[test]
    fn rmdir_flat_and_recursive() {
        let mut vm = VM::new();
        let base = TempDir::new().unwrap();
        // Flat rmdir removes an empty directory but refuses a non-empty one.
        let empty = base.path().join("empty");
        fs::create_dir(&empty).unwrap();
        let empty_path = new_path(&mut vm, &empty);
        assert_eq!(
            path_rmdir_native(&mut vm, &empty_path, &[]).unwrap(),
            Value::Nil
        );
        assert!(!empty.exists());

        let full = base.path().join("full");
        fs::create_dir(&full).unwrap();
        fs::write(full.join("f.txt"), "x").unwrap();
        let full_path = new_path(&mut vm, &full);
        assert!(path_rmdir_native(&mut vm, &full_path, &[]).is_err());
        // Recursive rmdir removes it and its contents.
        assert_eq!(
            path_rmdir_native(&mut vm, &full_path, &[Value::Bool(true)]).unwrap(),
            Value::Nil
        );
        assert!(!full.exists());
    }

    #[test]
    fn write_text_rejects_non_string() {
        let mut vm = VM::new();
        let file = new_path(&mut vm, StdPath::new("unused"));
        assert!(path_write_text_native(&mut vm, &file, &[Value::Nil]).is_err());
    }
}
