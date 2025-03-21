//! Example module exporting math functionality.

use crate::value::{ModuleContents, Number, Value};
use crate::vm::VM;

/// Calculate the square root of the number. Always return a float.
fn sqrt_native(vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::Number(Number::Float(n)) => Ok(n.sqrt().into()),
        Value::Number(Number::Integer(n)) => Ok((n.to_f64(&vm.heap)).sqrt().into()),
        x => Err(format!(
            "'sqrt' expected numeric argument, got: {}",
            x.to_string(&vm.heap)
        )),
    }
}

/// Export all the function of the module with the
/// name they are to be called with from generic as well as
/// their supported arities.
pub(super) fn module() -> ModuleContents {
    vec![("sqrt", &[1], sqrt_native)]
}
