//! Methods of the native `Set` class.

use crate::{
    value::{Number, Set, Value},
    vm::{VM, errors::VmResult},
};

/// Insert an item into the set `set.insert(item)`.
/// Supports all value types that implement hashable functionality.
pub(super) fn set_insert_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    Set::add(vm, receiver, args[0])?;
    Ok(Value::Nil)
}

/// Remove a value from the set `set.remove(val)`.
/// Returns whether the value was originally in the set.
pub(super) fn set_remove_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    Ok(Set::remove(vm, receiver, args[0])?.into())
}

/// Check if a hasheable value is in the set `set.contains(val)`.
/// Also powers `val in set`.
pub(super) fn set_contains_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    Ok(Set::contains(vm, receiver, args[0])?.into())
}

pub(super) fn set_len_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let my_set = receiver.as_set(&vm.heap);
    Ok(Number::from_usize(my_set.items.len(), &mut vm.heap).into())
}

pub(super) fn set_bool_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let is_empty = receiver.as_set(&vm.heap).items.is_empty();
    Ok((!is_empty).into())
}

/// Constructor for Set that accepts variable number of arguments.
///
/// `Set()` creates empty set, `Set(1, 2, 3)` creates {1, 2, 3}.
/// Only hashable values are allowed.
pub(super) fn set_init_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    receiver.as_set_mut(&mut vm.heap).items.clear(); // reset
    let items = if args.len() == 1
        && let Some(iter_items) = vm.collect_items_from_iterable(args[0])?
    {
        iter_items
    } else {
        args.to_vec()
    };

    // Items collected from an iterable may not be referenced anywhere else,
    // so keep them rooted while the adds run `__hash__`/`__eq__`.
    vm.for_each_rooted(items, |vm, item| Set::add(vm, receiver, item))?;
    Ok(*receiver)
}
