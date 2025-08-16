//! Methods of the native `Dict` class.

use crate::{
    value::{Number, Value},
    vm::VM,
};

/// Get an item via `dict[a]`, where `a` is a hashable value type.
/// Now supports all value types with __hash__ method support.
pub(super) fn dict_get_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    // Create a temporary dict to avoid borrowing conflicts
    let dict = std::mem::take(receiver.as_dict_mut(&mut vm.heap));
    let result = match dict.get(args[0], vm)? {
        Some(value) => Ok(*value),
        None => Err(format!("Key `{}` not found.", args[0].to_string(&vm.heap))),
    };
    // Restore the dict
    *receiver.as_dict_mut(&mut vm.heap) = dict;
    result
}

/// Set an item via `dict[a] = b`, where `a` is a hashable value type.
/// Now supports all value types with __hash__ method support.
pub(super) fn dict_set_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let mut dict = std::mem::take(receiver.as_dict_mut(&mut vm.heap));
    dict.add(*args[0], *args[1], vm)?;
    *receiver.as_dict_mut(&mut vm.heap) = dict;
    Ok(Value::Nil)
}

pub(super) fn dict_len_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let dict = receiver.as_dict(&vm.heap);
    let total_len: usize = dict.items.values().map(Vec::len).sum();
    Ok(Number::from_usize(total_len, &mut vm.heap).into())
}
