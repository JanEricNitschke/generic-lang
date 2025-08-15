//! Methods of the native `Dict` class.

use crate::{
    value::{NativeClass, Number, Value},
    vm::VM,
};

/// Get an item via `dict[a]`, where `a` is a hashable value type.
/// Supports all value types that implement hashable functionality.
pub(super) fn dict_get_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    // Create a temporary dict to avoid borrowing conflicts
    let dict = std::mem::take(receiver.as_dict_mut(&mut vm.heap));
    let result = match dict.get(*args[0], vm)? {
        Some(value) => Ok(*value),
        None => Err(format!("Key `{}` not found.", args[0].to_string(&vm.heap))),
    };
    // Restore the dict
    *receiver.as_dict_mut(&mut vm.heap) = dict;
    result
}

/// Set an item via `dict[a] = b`, where `a` is a hashable value type.
/// Supports all value types that implement hashable functionality.
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

pub(super) fn dict_contains_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    // Create a temporary set to avoid borrowing conflicts
    let dict = std::mem::take(receiver.as_dict_mut(&mut vm.heap));
    let result = dict.contains(*args[0], vm)?.into();
    // Restore the set
    *receiver.as_dict_mut(&mut vm.heap) = dict;
    Ok(result)
}

pub(super) fn dict_len_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let dict = receiver.as_dict(&vm.heap);
    Ok(Number::from_usize(dict.items.len(), &mut vm.heap).into())
}

pub(super) fn dict_bool_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let is_empty = receiver.as_dict(&vm.heap).items.is_empty();
    Ok((!is_empty).into())
}

/// Constructor for Dict that accepts variable number of arguments.
///
/// `Dict()` creates empty dict,
/// `Dict((1, "a"), (2, "b"), (3, "c"))` creates {1: "a", 2: "b", 3: "c"}.
/// Only hashable values are allowed as keys.
pub(super) fn dict_init_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let mut dict = std::mem::take(receiver.as_dict_mut(&mut vm.heap));
    dict.items.clear(); // reset

    for arg in args {
        if let Value::Instance(inst) = arg
            && let Some(NativeClass::Tuple(tuple)) = &inst.to_value(&vm.heap).backing
            && let [key, value] = tuple.items()
        {
            dict.add(*key, *value, vm)?;
        } else {
            return Err(format!(
                "Dict initializer expects 2-element tuples, got `{}`.",
                arg.to_string(&vm.heap)
            ));
        }
    }
    *receiver.as_dict_mut(&mut vm.heap) = dict;
    Ok(*receiver)
}
