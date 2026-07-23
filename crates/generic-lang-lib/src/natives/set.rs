//! Methods of the native `Set` class.
use crate::vm::ExceptionKind::RuntimeError;
use crate::{
    value::{Instance, Number, Set, SetIterator, Value},
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

pub(super) fn set_str_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let start = vm.stack.len();
    vm.stack.extend(
        receiver
            .as_set(&vm.heap)
            .items
            .iter()
            .map(|(item, _hash)| *item),
    );
    let end = vm.stack.len();

    let mut string = String::from("{");

    for stack_index in start..end {
        let item = vm.stack[stack_index];
        if stack_index > start {
            string.push_str(", ");
        }

        string.push_str(vm.value_to_string(&item)?.to_value(&vm.heap));
    }

    string.push('}');

    vm.stack.truncate(start);
    Ok(vm.heap.string_id(&string).into())
}

/// Produce an iterator over the set. `set.__iter__()`.
pub(super) fn set_iter_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let set_instance = receiver.as_instance();
    let size = receiver.as_set(&vm.heap).items.len();
    let target_class = vm.heap.native_classes.get("SetIterator").unwrap();
    let iterator = SetIterator::new(*set_instance, size);
    let instance = Instance::new(*target_class, Some(iterator.into()));
    Ok(vm.heap.add_instance(instance))
}

/// Iterators are their own iterators.
pub(super) fn set_iter_iter_native(
    _vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    Ok(*receiver)
}

/// Get the next element from a set iterator (`__next__()`).
pub(super) fn set_iter_next_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    // GC-safety: mem::take pattern, same as list/tuple/range iterators.
    let mut iter = std::mem::take(receiver.as_set_iterator_mut(&mut vm.heap));
    let set = iter.get_set(&vm.heap);

    // Check if the set's size changed since the iterator was created.
    if set.items.len() != iter.size {
        *receiver.as_set_iterator_mut(&mut vm.heap) = iter;
        return Err(vm
            .throw(RuntimeError, "set changed size during iteration")
            .unwrap_err());
    }

    let result = if let Some((value, _hash)) = set.items.iter().nth(iter.index) {
        iter.index += 1;

        Ok(*value)
    } else {
        Ok(Value::StopIteration)
    };

    *receiver.as_set_iterator_mut(&mut vm.heap) = iter;
    result
}

pub(super) fn set_iter_str_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    let set = receiver.as_set_iterator(&vm.heap).set;
    let set_string = vm.value_to_string(&set.into())?.to_value(&vm.heap);

    let string = format!("<set iterator of {set_string}>");
    Ok(vm.heap.string_id(&string).into())
}
