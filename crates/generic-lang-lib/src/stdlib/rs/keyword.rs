//! The `keyword` stdlib module: the language's reserved words, derived
//! from the scanner (see [`keywords`]) so no separate list exists.
//!
//! [`keywords`]: crate::scanner::keywords

use crate::scanner::keywords;
use crate::value::{CreatorContext, ModuleContents, ModuleExport, Value};
use crate::vm::ExceptionKind::TypeError;
use crate::vm::VM;
use crate::vm::errors::VmResult;

/// `kwlist` - the reserved words as a list of strings, built at import.
fn make_kwlist(vm: &mut VM, _context: &CreatorContext) -> Value {
    let items: Vec<Value> = keywords()
        .iter()
        .map(|keyword| vm.heap.string_id(keyword).into())
        .collect();
    vm.new_list(items)
}

/// `iskeyword(name)` - whether `name` is a reserved word.
fn iskeyword_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let Value::String(string_id) = args[0] else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'iskeyword' expects a string, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let name = string_id.to_value(&vm.heap);
    Ok(keywords().iter().any(|keyword| keyword == name).into())
}

pub(super) fn register(vm: &mut VM) {
    vm.register_stdlib_module(&"keyword", module());
}

/// Export all the contents of the module with the
/// name they are to be accessed with from generic; functions
/// additionally carry their supported arities.
fn module() -> ModuleContents {
    vec![
        ModuleExport::Value {
            name: "kwlist".into(),
            create: make_kwlist,
        },
        ModuleExport::Function {
            name: "iskeyword",
            arity: &[1],
            fun: iskeyword_native,
        },
    ]
}
