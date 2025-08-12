//! Methods of the native `Set` class.

use crate::{
    value::{Number, Value},
    vm::VM,
};

/// Insert an item into the set `set.insert(item)`.
/// Works on all value types - instances with __hash__ methods use custom hash implementation.
pub(super) fn set_insert_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let hash = if let Value::Instance(_) = args[0] {
        args[0].compute_instance_hash(vm)?
    } else {
        args[0].to_hash(&vm.heap)
    };

    let mut set = std::mem::take(receiver.as_set_mut(&mut vm.heap));
    set.add_with_hash(*args[0], hash, &vm.heap);
    *receiver.as_set_mut(&mut vm.heap) = set;
    Ok(Value::Nil)
}

/// Remove a value from the set `set.remove(val)`.
/// Works on all value types - instances with __hash__ methods use custom hash implementation.
/// Returns whether the value was originally in the set.
pub(super) fn set_remove_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let hash = if let Value::Instance(_) = args[0] {
        args[0].compute_instance_hash(vm)?
    } else {
        args[0].to_hash(&vm.heap)
    };

    let mut my_set = std::mem::take(receiver.as_set_mut(&mut vm.heap));
    let result = my_set.remove_with_hash(args[0], hash, &vm.heap).into();
    *receiver.as_set_mut(&mut vm.heap) = my_set;
    Ok(result)
}

/// Check if a value is in the set `set.contains(val)`.
/// Works on all value types - instances with __hash__ methods use custom hash implementation.
/// Also powers `val in set`.
pub(super) fn set_contains_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let hash = if let Value::Instance(_) = args[0] {
        args[0].compute_instance_hash(vm)?
    } else {
        args[0].to_hash(&vm.heap)
    };

    let my_set = receiver.as_set(&vm.heap);
    let result = my_set.contains_with_hash(args[0], hash, &vm.heap);
    Ok(result.into())
}

pub(super) fn set_len_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_set = receiver.as_set(&vm.heap);
    Ok(Number::from_usize(my_set.items.len(), &mut vm.heap).into())
}
