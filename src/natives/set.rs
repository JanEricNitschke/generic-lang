//! Methods of the native `Set` class.

use crate::{value::Value, vm::VM};

/// Insert an item into the set `set.insert(item)`.
/// Only works on hasheable values.
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

/// Remove a value from the set `set.remove(val)`.
/// The value has to be able to actually be in the set, meaning that it
/// has to be hasheable.
/// Returns whether the value was originally in the set.
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

/// Check if a hasheable value is in the set `set.contains(val)`.
/// Also powers `val in set`.
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
