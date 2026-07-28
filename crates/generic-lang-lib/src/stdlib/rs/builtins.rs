//! The `builtins` stdlib module: a snapshot of the built-in functions
//! namespace. The export list is built at registration time by looping
//! the live namespace (which is complete before the stdlib registers
//! and never gains names afterwards), so it is complete by
//! construction; each export reads its builtin's value at import time.
//! Assigning through the module never touches the live builtins; a
//! mutable builtin *value* is still shared.

use crate::value::{ModuleContents, ModuleExport};
use crate::vm::VM;

pub(super) fn register(vm: &mut VM) {
    let contents: ModuleContents = vm
        .builtins
        .keys()
        .map(|name| ModuleExport::Value {
            name: name.to_value(&vm.heap).clone().into(),
            create: |vm, context| {
                let name_id = vm.heap.string_id(&context.name.to_string());
                vm.builtins
                    .get(&name_id)
                    .unwrap_or_else(|| {
                        panic!("`{}` vanished from the builtins namespace", context.name)
                    })
                    .value
            },
        })
        .collect();
    vm.register_stdlib_module(&"builtins", contents);
}
