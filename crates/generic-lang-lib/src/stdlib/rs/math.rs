//! Example module exporting math functionality.

use crate::value::{ModuleContents, ModuleExport, Number, Value};
use crate::vm::ExceptionKind::TypeError;
use crate::vm::VM;
use crate::vm::errors::VmResult;

/// Calculate the square root of the number. Always return a float.
fn sqrt_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    match &args[0] {
        Value::Number(Number::Float(n)) => Ok(n.sqrt().into()),
        Value::Number(Number::Integer(n)) => Ok((n.to_f64(&vm.heap)).sqrt().into()),
        x => Err(vm
            .throw(
                TypeError,
                &format!(
                    "'sqrt' expected numeric argument, got: {}",
                    x.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

/// Export all the contents of the module with the
/// name they are to be accessed with from generic; functions
/// additionally carry their supported arities.
pub(super) fn module() -> ModuleContents {
    vec![
        ModuleExport::Function {
            name: "sqrt",
            arity: &[1],
            fun: sqrt_native,
        },
        ModuleExport::Value {
            name: "pi",
            create: |_vm| std::f64::consts::PI.into(),
        },
    ]
}
