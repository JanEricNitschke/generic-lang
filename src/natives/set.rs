use crate::{value::Value, vm::VM};

pub(super) fn set_insert_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let set = receiver.as_set_mut();
    if !args[0].is_hasheable() {
        return Err(format!("Value `{}` is not hashable.", args[0]));
    }
    set.items.insert(*args[0]);
    Ok(Value::Nil)
}

pub(super) fn set_remove_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_set = receiver.as_set_mut();
    if !args[0].is_hasheable() {
        return Err(format!("Value `{}` is not hashable.", args[0]));
    };
    Ok(my_set.items.remove(args[0]).into())
}

pub(super) fn set_contains_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_set = receiver.as_set();
    if !args[0].is_hasheable() {
        return Err(format!("Value `{}` is not hashable.", args[0]));
    };
    Ok(my_set.items.contains(args[0]).into())
}
