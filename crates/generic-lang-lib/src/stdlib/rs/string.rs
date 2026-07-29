//! The `string` stdlib module: named constants for the control
//! characters the language has no escape syntax for. Build strings that
//! need them by interpolation, e.g. `f"line one${string.newline}line two"`.

use crate::value::{CreatorContext, ModuleContents, ModuleExport, Value};
use crate::vm::VM;

/// The named control characters, in export order.
const CHARACTERS: &[(&str, char)] = &[
    ("newline", '\n'),
    ("carriage_return", '\r'),
    ("tab", '\t'),
    ("quote", '"'),
    ("backslash", '\\'),
    ("null", '\0'),
];

/// Build the single-character value for the export named `context.name`.
fn character(vm: &mut VM, context: &CreatorContext) -> Value {
    let character = CHARACTERS
        .iter()
        .find_map(|(name, character)| (*name == context.name).then_some(*character))
        .expect("every string export is a known character");
    vm.heap.string_id(&character.to_string()).into()
}

pub(super) fn register(vm: &mut VM) {
    vm.register_stdlib_module(&"string", module());
}

/// Export all the contents of the module with the
/// name they are to be accessed with from generic.
fn module() -> ModuleContents {
    CHARACTERS
        .iter()
        .map(|(name, _)| ModuleExport::Value {
            name: (*name).into(),
            create: character,
        })
        .collect()
}
