use crate::{
    value::Value,
    vm::VM,
};


pub(super) fn dict_get_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let dict = receiver.as_dict();
    if !args[0].is_hasheable() {
        return Err(format!("Key `{}` is not hashable.", args[0]));
    }
    match dict.items.get(args[0]) {
        Some(value) => Ok(*value),
        None => Err(format!("Key `{}` not found.", args[0])),
    }

}

pub(super) fn dict_set_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let dict = receiver.as_dict_mut();
    if !args[0].is_hasheable() {
        return Err(format!("Key `{}` is not hashable.", args[0]));
    }
    dict.items.insert(*args[0], *args[1]);
    Ok(Value::Nil)
}