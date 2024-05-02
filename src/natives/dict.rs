//! Methods of the native `Dict` class.

use crate::{value::Value, vm::VM};

/// Get an item via `dict[a]`, where `a` is a hashable value type.
/// Currently only `nil`, `StopIteration`, bools, integers and strings are hashable.
pub(super) fn dict_get_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let dict = receiver.as_dict();
    if !args[0].is_hasheable() {
        return Err(format!("Key `{}` is not hashable.", args[0]));
    }
    dict.items.get(args[0]).map_or_else(
        || Err(format!("Key `{}` not found.", args[0])),
        |value| Ok(*value),
    )
}

/// Set an item via `dict[a] = b`, where `a` is a hashable value type.
/// Currently only `nil`, `StopIteration`, bools, integers and strings are hashable.
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
