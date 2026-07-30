//! A debug-only stdlib module that exercises re-entrant value creation at
//! import time. `probe`'s creator re-enters the VM, so under the stress-GC
//! suite a collection runs part-way through building the module's exports.
//! The meaningful roots are the earlier `answer` function (a fresh native
//! function) and the `sentinel` value (with a name string the value does
//! not itself reference): both must survive that collection or their heap
//! ids dangle. The `Path` class export covers the `Class` import arm too,
//! but its survival is not a real test - native classes are permanent GC
//! roots regardless of the import machinery. Gated to debug builds so it
//! never reaches the release stdlib namespace.

#![allow(clippy::unnecessary_wraps)]

use crate::types::InjectedKind;
use crate::value::{CreatorContext, ModuleContents, ModuleExport, Value};
use crate::vm::VM;
use crate::vm::errors::VmResult;

/// A trivial native function export, so the `Function` arm of the import
/// rooting loop is exercised alongside the re-entrant creator.
fn answer_native(_vm: &mut VM, _args: &[Value]) -> VmResult<Value> {
    Ok(42.into())
}

/// A plain string value whose content differs from the export name, so the
/// name string `"sentinel"` is kept reachable only by the import-time rooting,
/// never by the value itself.
fn sentinel(vm: &mut VM, _context: &CreatorContext) -> Value {
    vm.heap.string_id(&"sentinel-value").into()
}

/// A value built by re-entering the VM: evaluating an expression runs
/// bytecode, and under the stress-GC build that collects at every
/// instruction, so the exports built before this one are exercised against
/// the collector mid-import.
fn probe(vm: &mut VM, _context: &CreatorContext) -> Value {
    vm.run_injected_source("1 + 1", None, &[], InjectedKind::Eval)
        .expect("evaluating `1 + 1` cannot fail")
}

pub(super) fn register(vm: &mut VM) {
    // The re-entrant `probe` is last, so the collection it triggers happens
    // after the function, class, and plain value are built and must survive.
    let contents: ModuleContents = vec![
        ModuleExport::Function {
            name: "answer",
            arity: &[0],
            fun: answer_native,
        },
        ModuleExport::Class { name: "Path" },
        ModuleExport::Value {
            name: "sentinel".into(),
            create: sentinel,
        },
        ModuleExport::Value {
            name: "probe".into(),
            create: probe,
        },
    ];
    vm.register_stdlib_module(&"reentrant_import", contents);
}
