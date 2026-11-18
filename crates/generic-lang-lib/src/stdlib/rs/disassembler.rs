//! The `disassembler` stdlib module: render a Generic function's bytecode.

use crate::heap::{FunctionId, Heap};
use crate::value::{ModuleContents, ModuleExport, Value};
use crate::vm::ExceptionKind::TypeError;
use crate::vm::VM;
use crate::vm::errors::VmResult;

/// Resolve a user-visible callable to the function whose chunk is
/// disassembled. Bound methods are unwrapped to the closure they bind; native
/// callables have no bytecode and resolve to `None`.
fn bytecode_function(value: Value, heap: &Heap) -> Option<FunctionId> {
    match value {
        Value::Closure(closure) => Some(closure.to_value(heap).function),
        Value::Function(function) => Some(function),
        Value::BoundMethod(bound) => match bound.to_value(heap).method {
            Value::Closure(closure) => Some(closure.to_value(heap).function),
            Value::Function(function) => Some(function),
            _ => None,
        },
        _ => None,
    }
}

/// `disassemble(value)` - render a function's bytecode.
fn disassemble_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let Some(function) = bytecode_function(args[0], &vm.heap) else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'disassemble' expects a function, closure, or bound method, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let rendered = function.to_value(&vm.heap).chunk.to_string(&vm.heap);
    Ok(vm.heap.string_id(&rendered).into())
}

pub(super) fn register(vm: &mut VM) {
    vm.register_stdlib_module(&"disassembler", module());
}

/// Export all the contents of the module with the
/// name they are to be accessed with from generic; functions
/// additionally carry their supported arities.
fn module() -> ModuleContents {
    vec![ModuleExport::Function {
        name: "disassemble",
        arity: &[1],
        fun: disassemble_native,
    }]
}
