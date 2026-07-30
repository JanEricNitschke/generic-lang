//! The `os` stdlib module: environment variables, system information,
//! and the script's command line arguments.

use crate::value::{CreatorContext, Dict, ModuleContents, ModuleExport, Value};
use crate::vm::ExceptionKind::TypeError;
use crate::vm::VM;
use crate::vm::errors::VmResult;

/// Read an environment variable from the live environment.
/// `getenv(name)` returns its value or `nil`; `getenv(name, default)`
/// returns `default` when the variable is unset (or not valid UTF-8).
fn getenv_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let Value::String(name_id) = args[0] else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'getenv' expected a string name, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let name = name_id.to_value(&vm.heap).clone();
    match std::env::var(&name) {
        Ok(value) => Ok(vm.heap.string_id(&value).into()),
        Err(_) => Ok(args.get(1).copied().unwrap_or(Value::Nil)),
    }
}

fn create_name(vm: &mut VM, _context: &CreatorContext) -> Value {
    vm.heap.string_id(&std::env::consts::OS).into()
}

/// A `Dict` snapshot of the environment at import time. Entries whose
/// name or value is not valid UTF-8 are skipped.
fn create_environ(vm: &mut VM, _context: &CreatorContext) -> Value {
    let dict = vm.new_dict();
    for (key, value) in std::env::vars_os() {
        let (Ok(key), Ok(value)) = (key.into_string(), value.into_string()) else {
            continue;
        };
        let key = vm.heap.string_id(&key).into();
        let value = vm.heap.string_id(&value).into();
        // String keys hash without re-entering the interpreter, so the
        // insert cannot throw, and no bytecode runs while the dict is
        // being filled, so nothing here needs rooting.
        Dict::add(vm, &dict, key, value).expect("string keys hash infallibly");
    }
    dict
}

/// The script path followed by the arguments given after it on the
/// command line. Empty in the REPL.
fn create_argv(vm: &mut VM, _context: &CreatorContext) -> Value {
    let args = vm.script_args.clone();
    let items = args
        .iter()
        .map(|arg| vm.heap.string_id(arg).into())
        .collect();
    vm.new_list(items)
}

/// Register the `os` module.
pub(super) fn register(vm: &mut VM) {
    vm.register_stdlib_module(&"os", module());
}

/// Export all the contents of the module with the
/// name they are to be accessed with from generic; functions
/// additionally carry their supported arities.
fn module() -> ModuleContents {
    vec![
        ModuleExport::Function {
            name: "getenv",
            arity: &[1, 2],
            fun: getenv_native,
        },
        ModuleExport::Value {
            name: "name".into(),
            create: create_name,
        },
        ModuleExport::Value {
            name: "environ".into(),
            create: create_environ,
        },
        ModuleExport::Value {
            name: "argv".into(),
            create: create_argv,
        },
    ]
}

// `import` canonicalizes candidate paths (`realpath`): not runnable under
// `cargo miri test`.
#[cfg(all(test, not(miri)))]
mod tests {
    use crate::vm::{InterpretResult, VM};
    use std::path::PathBuf;

    /// `os.argv` reflects the script args installed on the VM, in order.
    #[test]
    fn argv_reflects_script_args() {
        let mut vm = VM::new();
        vm.set_script_args(vec![
            "script.gen".to_string(),
            "--flag".to_string(),
            "value".to_string(),
        ]);
        let source = r#"
import "os";
assert(len(os.argv) == 3);
assert(os.argv[0] == "script.gen");
assert(os.argv[1] == "--flag");
assert(os.argv[2] == "value");
"#;
        assert_eq!(
            vm.interpret(source, PathBuf::from("argv_test")),
            InterpretResult::Ok
        );
    }
}
