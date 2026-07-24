//! Methods of the native `Set` class.
use crate::heap::Heap;
use crate::vm::ExceptionKind::{RuntimeError, TypeError};
use crate::{
    value::{Instance, NativeClass, Number, Set, SetIterator, Value},
    vm::{VM, errors::VmResult},
};

/// Whether `value` is (backed by) a `Set`.
fn is_set(value: Value, heap: &Heap) -> bool {
    matches!(value, Value::Instance(instance)
        if matches!(&instance.to_value(heap).backing, Some(NativeClass::Set(_))))
}

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

    // `bucket` is a physical slot cursor: resume scanning from it and advance
    // past the slot we return, so a full iteration visits each bucket once.
    let num_buckets = set.items.num_buckets();
    let mut result = Value::StopIteration;
    while iter.bucket < num_buckets {
        let bucket = iter.bucket;
        iter.bucket += 1;
        if let Some((value, _hash)) = set.items.get_bucket(bucket) {
            result = *value;
            break;
        }
    }

    *receiver.as_set_iterator_mut(&mut vm.heap) = iter;
    Ok(result)
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

/// Set.__bitor__(other) - union.
pub(super) fn set_bitor_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let new_set = create_empty_set_instance(vm);
    // Root new set on the stack
    vm.stack.push(new_set);

    let other_items = extract_set_items(vm, &args[0])?;
    let self_items = receiver.as_set(&vm.heap).items.iter().map(|(v, _h)| *v);

    // Create new set with all elements from both.
    // Root items on the stack during Set::add calls.
    let all_items: Vec<Value> = self_items.into_iter().chain(other_items).collect();
    vm.for_each_rooted(all_items, |vm, item| Set::add(vm, &new_set, item))?;

    // Remove the stored new set
    vm.stack.pop();
    Ok(new_set)
}

/// Set.__bitand__(other) - intersection.
pub(super) fn set_bitand_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let new_set = create_empty_set_instance(vm);
    // Root new set on the stack
    vm.stack.push(new_set);

    let self_items: Vec<Value> = receiver
        .as_set(&vm.heap)
        .items
        .iter()
        .map(|(v, _)| *v)
        .collect();

    vm.for_each_rooted(self_items, |vm, item| {
        if Set::contains(vm, &args[0], item)? {
            Set::add(vm, &new_set, item)?;
        }
        Ok(None)
    })?;

    // Remove the stored new set
    vm.stack.pop();
    Ok(new_set)
}

/// Set.__sub__(other) - difference.
pub(super) fn set_sub_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let new_set = create_empty_set_instance(vm);
    // Root new set on the stack
    vm.stack.push(new_set);

    let self_items: Vec<Value> = receiver
        .as_set(&vm.heap)
        .items
        .iter()
        .map(|(v, _)| *v)
        .collect();

    vm.for_each_rooted(self_items, |vm, item| {
        if !Set::contains(vm, &args[0], item)? {
            Set::add(vm, &new_set, item)?;
        }
        Ok(None)
    })?;

    // Remove the stored new set
    vm.stack.pop();
    Ok(new_set)
}

/// Set.__bitxor__(other) - symmetric difference.
pub(super) fn set_bitxor_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let new_set = create_empty_set_instance(vm);
    // Root new set on the stack
    vm.stack.push(new_set);

    let self_items: Vec<Value> = receiver
        .as_set(&vm.heap)
        .items
        .iter()
        .map(|(v, _)| *v)
        .collect();

    let other_items = extract_set_items(vm, &args[0])?;

    let before_each_self = vm.stack.len();
    vm.stack.extend(other_items.clone());
    // Items in self but not other.
    vm.for_each_rooted(self_items.clone(), |vm, item| {
        if !Set::contains(vm, &args[0], item)? {
            Set::add(vm, &new_set, item)?;
        }
        Ok(None)
    })?;
    vm.stack.truncate(before_each_self);

    vm.stack.extend(self_items);
    // Items in other but not self.
    vm.for_each_rooted(other_items, |vm, item| {
        if !Set::contains(vm, receiver, item)? {
            Set::add(vm, &new_set, item)?;
        }
        Ok(None)
    })?;
    // Everything is stack neutral, so we can reuse the same value
    vm.stack.truncate(before_each_self);

    // Remove the stored new set
    vm.stack.pop();
    Ok(new_set)
}

/// Helper: extract items from a Set value into a Vec.
fn extract_set_items(vm: &mut VM, value: &Value) -> VmResult<Vec<Value>> {
    if let Value::Instance(inst) = value
        && let Some(crate::value::NativeClass::Set(set)) = &inst.to_value(&vm.heap).backing
    {
        Ok(set.items.iter().map(|(v, _h)| *v).collect())
    } else {
        Err(vm
            .throw(
                TypeError,
                &format!("Expected a set, got `{}`.", value.to_string(&vm.heap)),
            )
            .unwrap_err())
    }
}

/// Helper: create a new empty Set instance.
fn create_empty_set_instance(vm: &mut VM) -> Value {
    let instance = Instance::new(
        *vm.heap.native_classes.get("Set").unwrap(),
        Some(Set::default().into()),
    );
    vm.heap.add_instance(instance)
}

pub(super) fn set_clear_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let set = receiver.as_set_mut(&mut vm.heap);
    set.items.clear();
    Ok(Value::Nil)
}

/// `set == other`: equal iff both are sets of the same size and every element
/// of one is in the other (membership respects each element's `__eq__`). Any
/// other type is unequal (never an error).
pub(super) fn set_eq_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let other = args[0];
    if receiver.is(&other) {
        return Ok(true.into());
    }
    if !is_set(other, &vm.heap) {
        return Ok(false.into());
    }
    if receiver.as_set(&vm.heap).items.len() != other.as_set(&vm.heap).items.len() {
        return Ok(false.into());
    }

    // Same size + every element present in `other` implies set equality.
    // Membership re-enters the interpreter through `__hash__`/`__eq__`, so keep
    // the elements rooted on the VM stack while probing.
    let start = vm.stack.len();
    vm.stack.extend(
        receiver
            .as_set(&vm.heap)
            .items
            .iter()
            .map(|(item, _hash)| *item),
    );
    let end = vm.stack.len();
    for index in start..end {
        let item = vm.stack[index];
        if !Set::contains(vm, &other, item)? {
            vm.stack.truncate(start);
            return Ok(false.into());
        }
    }
    vm.stack.truncate(start);
    // A reentrant `__hash__`/`__eq__` may have resized either set mid-probe;
    // recheck the sizes agree so a shrunk set is not reported equal.
    if receiver.as_set(&vm.heap).items.len() != other.as_set(&vm.heap).items.len() {
        return Ok(false.into());
    }
    Ok(true.into())
}
