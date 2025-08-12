//! Methods of the native `Dict` class.

use crate::{
    value::{Number, Value},
    vm::VM,
};

/// Get an item via `dict[a]`, where `a` is any hashable value type.
/// Instances with __hash__ methods will use their custom hash implementation.
pub(super) fn dict_get_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let hash = if let Value::Instance(_) = args[0] {
        args[0].compute_instance_hash(vm)?
    } else {
        args[0].to_hash(&vm.heap)
    };

    let dict = receiver.as_dict(&vm.heap);
    match dict.get_with_hash(args[0], hash, &vm.heap) {
        Some(value) => Ok(*value),
        None => Err(format!("Key `{}` not found.", args[0].to_string(&vm.heap))),
    }
}

/// Set an item via `dict[a] = b`, where `a` is any hashable value type.
/// Instances with __hash__ methods will use their custom hash implementation.
pub(super) fn dict_set_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let hash = if let Value::Instance(_) = args[0] {
        args[0].compute_instance_hash(vm)?
    } else {
        args[0].to_hash(&vm.heap)
    };

    let mut dict = std::mem::take(receiver.as_dict_mut(&mut vm.heap));
    dict.add_with_hash(*args[0], *args[1], hash, &vm.heap);
    *receiver.as_dict_mut(&mut vm.heap) = dict;
    Ok(Value::Nil)
}

pub(super) fn dict_len_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let dict = receiver.as_dict(&vm.heap);
    Ok(Number::from_usize(dict.items.len(), &mut vm.heap).into())
}
