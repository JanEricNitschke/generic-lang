use crate::value::{ias_f64, ModuleContents, Number, Value};
use crate::vm::VM;

pub(super) fn sqrt_native(_vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::Number(Number::Float(n)) => Ok(n.sqrt().into()),
        Value::Number(Number::Integer(n)) => Ok((ias_f64(*n)).sqrt().into()),
        x => Err(format!("'sqrt' expected numeric argument, got: {}", *x)),
    }
}

pub(super) fn module() -> ModuleContents {
    vec![("sqrt", &[1], sqrt_native)]
}
