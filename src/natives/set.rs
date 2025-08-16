//! Methods of the native `Set` class.

use crate::{
    value::{Number, Value},
    vm::VM,
};

/// Insert an item into the set `set.insert(item)`.
/// Now supports all value types with __hash__ method support.
pub(super) fn set_insert_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let mut set = std::mem::take(receiver.as_set_mut(&mut vm.heap));
    set.add(*args[0], vm)?;
    *receiver.as_set_mut(&mut vm.heap) = set;
    Ok(Value::Nil)
}

/// Remove a value from the set `set.remove(val)`.
/// Returns whether the value was originally in the set.
pub(super) fn set_remove_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let mut my_set = std::mem::take(receiver.as_set_mut(&mut vm.heap));
    let result = my_set.remove(args[0], vm)?.into();
    *receiver.as_set_mut(&mut vm.heap) = my_set;
    Ok(result)
}

/// Check if a hasheable value is in the set `set.contains(val)`.
/// Also powers `val in set`.
pub(super) fn set_contains_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    // Create a temporary set to avoid borrowing conflicts
    let set = std::mem::take(receiver.as_set_mut(&mut vm.heap));
    let result = set.contains(args[0], vm)?.into();
    // Restore the set
    *receiver.as_set_mut(&mut vm.heap) = set;
    Ok(result)
}

pub(super) fn set_len_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_set = receiver.as_set(&vm.heap);
    let total_len: usize = my_set.items.values().map(Vec::len).sum();
    Ok(Number::from_usize(total_len, &mut vm.heap).into())
}
