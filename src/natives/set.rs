//! Methods of the native `Set` class.

use crate::{
    value::{Number, Value},
    vm::VM,
};

/// Insert an item into the set `set.insert(item)`.
/// Only works on hasheable values.
pub(super) fn set_insert_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let mut set = std::mem::take(receiver.as_set_mut(&mut vm.heap));
    if !args[0].is_hasheable() {
        return Err(format!(
            "Value `{}` is not hashable.",
            args[0].to_string(&vm.heap)
        ));
    }
    set.add(*args[0], &vm.heap);
    *receiver.as_set_mut(&mut vm.heap) = set;
    Ok(Value::Nil)
}

/// Remove a value from the set `set.remove(val)`.
/// The value has to be able to actually be in the set, meaning that it
/// has to be hasheable.
/// Returns whether the value was originally in the set.
pub(super) fn set_remove_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let mut my_set = std::mem::take(receiver.as_set_mut(&mut vm.heap));
    if !args[0].is_hasheable() {
        return Err(format!(
            "Value `{}` is not hashable.",
            args[0].to_string(&vm.heap)
        ));
    }
    let result = my_set.remove(args[0], &vm.heap).into();
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
    let my_set = receiver.as_set(&vm.heap);
    if !args[0].is_hasheable() {
        return Err(format!(
            "Value `{}` is not hashable.",
            args[0].to_string(&vm.heap)
        ));
    }
    Ok(my_set.contains(args[0], &vm.heap).into())
}

pub(super) fn set_len_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_set = receiver.as_set(&vm.heap);
    Ok(Number::from_usize(my_set.items.len(), &mut vm.heap).into())
}

pub(super) fn set_bool_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let is_empty = receiver.as_set(&vm.heap).items.is_empty();
    Ok((!is_empty).into())
}

/// Constructor for Set that accepts variable number of arguments.
///
/// `Set()` creates empty set, `Set(1, 2, 3)` creates {1, 2, 3}.
/// Only hashable values are allowed.
pub(super) fn set_init_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let mut set = std::mem::take(receiver.as_set_mut(&mut vm.heap));
    set.items.clear(); // Explicitly clear to ensure it's empty
    for arg in args {
        if !arg.is_hasheable() {
            return Err(format!(
                "Value `{}` is not hashable.",
                arg.to_string(&vm.heap)
            ));
        }
        set.add(**arg, &vm.heap);
    }
    *receiver.as_set_mut(&mut vm.heap) = set;
    Ok(*receiver)
}
