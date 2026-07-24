//! Methods of the native `List` class.

use crate::heap::Heap;
use crate::types::Comparison;
use crate::vm::ExceptionKind::{IndexError, TypeError, ValueError};
use crate::{
    value::{Instance, List, ListIterator, NativeClass, Number, Value},
    vm::{VM, errors::VmResult},
};
use std::cmp::Ordering;

/// Whether `value` is (backed by) a `List`.
fn is_list(value: Value, heap: &Heap) -> bool {
    matches!(value, Value::Instance(instance)
        if matches!(&instance.to_value(heap).backing, Some(NativeClass::List(_))))
}

/// Append an item to the end of the list.
/// `list.append(a)`.
pub(super) fn list_append_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let list = receiver.as_list_mut(&mut vm.heap);
    list.items.push(args[0]);
    Ok(Value::Nil)
}

/// Pop an item off the end of the list via `list.pop()`
/// or from a specified index `list.pop(index)`.
/// Returns the removed element.
pub(super) fn list_pop_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let index = if args.is_empty() {
        None
    } else {
        let index = match &args[0] {
            Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
                Ok(index) => index,
                Err(_) => {
                    return Err(vm.throw(ValueError, &format!(
                        "Can not index into list with negative or too large numbers, got `{}`.",
                        n.to_string(&vm.heap)
                    )).unwrap_err());
                }
            },
            x => {
                return Err(vm
                    .throw(
                        TypeError,
                        &format!(
                            "Can only index into list with integer, got `{}`.",
                            x.to_string(&vm.heap)
                        ),
                    )
                    .unwrap_err());
            }
        };
        Some(index)
    };

    let my_list = receiver.as_list_mut(&mut vm.heap);

    match index {
        Some(index) => {
            let length = my_list.items.len();
            if index >= length {
                Err(vm
                    .throw(
                        IndexError,
                        &format!("Index `{index}` is out of bounds of list with len `{length}`."),
                    )
                    .unwrap_err())
            } else {
                Ok(my_list.items.remove(index))
            }
        }
        None => match my_list.items.pop() {
            Some(value) => Ok(value),
            None => Err(vm
                .throw(IndexError, "Can't 'pop' from an empty list.")
                .unwrap_err()),
        },
    }
}

/// Get an item at a specified index `list[a]`.
pub(super) fn list_get_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let index =
        match &args[0] {
            Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
                Ok(index) => index,
                Err(_) => {
                    return Err(vm
                    .throw(ValueError, &format!(
                        "Can not index into list with negative or too large numbers, got `{}`.",
                        n.to_string(&vm.heap)
                    ))
                    .unwrap_err());
                }
            },
            x => {
                return Err(vm
                    .throw(
                        TypeError,
                        &format!(
                            "Can only index into list with integer, got `{}`.",
                            x.to_string(&vm.heap)
                        ),
                    )
                    .unwrap_err());
            }
        };

    let my_list = receiver.as_list(&vm.heap);

    match my_list.items.get(index) {
        Some(value) => Ok(*value),
        None => Err(vm
            .throw(
                IndexError,
                &format!(
                    "Index `{index}` is out of bounds of list with len `{}`.",
                    my_list.items.len()
                ),
            )
            .unwrap_err()),
    }
}

/// Set the item at the specified index `list[a] = b`.
pub(super) fn list_set_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let index =
        match &args[0] {
            Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
                Ok(index) => index,
                Err(_) => {
                    return Err(vm
                    .throw(ValueError, &format!(
                        "Can not index into list with negative or too large numbers, got `{}`.",
                        n.to_string(&vm.heap)
                    ))
                    .unwrap_err());
                }
            },
            x => {
                return Err(vm
                    .throw(
                        TypeError,
                        &format!(
                            "Can only index into list with integer, got `{}`.",
                            x.to_string(&vm.heap)
                        ),
                    )
                    .unwrap_err());
            }
        };

    let my_list = receiver.as_list_mut(&mut vm.heap);
    let list_length = my_list.items.len();
    match my_list.items.get_mut(index) {
        Some(value) => {
            *value = args[1];
            Ok(Value::Nil)
        }
        None => Err(vm
            .throw(
                IndexError,
                &format!("Index `{index}` is out of bounds of list with len `{list_length}`."),
            )
            .unwrap_err()),
    }
}

/// Insert an item into the list at the specified index.
/// `list.insert(index, value)`, such that afterwards `list[index] = value`.
pub(super) fn list_insert_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let index =
        match &args[0] {
            Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
                Ok(index) => index,
                Err(_) => {
                    return Err(vm
                    .throw(ValueError, &format!(
                        "Can not index into list with negative or too large numbers, got `{}`.",
                        n.to_string(&vm.heap)
                    ))
                    .unwrap_err());
                }
            },
            x => {
                return Err(vm
                    .throw(
                        TypeError,
                        &format!(
                            "Can only index into list with integer, got `{}`.",
                            x.to_string(&vm.heap)
                        ),
                    )
                    .unwrap_err());
            }
        };

    let my_list = receiver.as_list_mut(&mut vm.heap);

    let length = my_list.items.len();
    if index > length {
        Err(vm
            .throw(
                IndexError,
                &format!("Index `{index}` is out of bounds of list with len `{length}`."),
            )
            .unwrap_err())
    } else {
        my_list.items.insert(index, args[1]);
        Ok(Value::Nil)
    }
}

/// Check if the list contains a value `list.contains(a)`.
/// This also powers `a in list`.
pub(super) fn list_contains_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let needle = args[0];
    let mut index = 0;
    // Re-fetch by index each step: `compare_values` re-enters and may mutate
    // or reallocate the list.
    while index < receiver.as_list(&vm.heap).items.len() {
        let element = receiver.as_list(&vm.heap).items[index];
        if vm.compare_values_eq(element, needle)? {
            return Ok(true.into());
        }
        index += 1;
    }
    Ok(false.into())
}

/// Produce an iterator over the list `var iter = list.__iter__()`.
/// Used by `foreach (var a in list)`.
pub(super) fn list_iter_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let my_list = receiver.as_instance();
    let my_iterator = ListIterator::new(*my_list);
    let target_class = vm.heap.native_classes.get("ListIterator").unwrap();
    let my_instance = Instance::new(*target_class, Some(my_iterator.into()));
    Ok(vm.heap.add_instance(my_instance))
}

/// Iterators are their own iterators.
pub(super) fn list_iter_iter_native(
    _vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    Ok(*receiver)
}

/// Get the next element from a listiterator `var next = listiter.__next__()`.
/// Powers `foreach (var a in list)`
#[allow(clippy::option_if_let_else)]
pub(super) fn list_iter_next_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    // GC-safety: taking the iterator out of the heap hides it from the GC.
    // This is only sound because nothing below re-enters the interpreter
    // (GC runs exclusively from the instruction dispatch loop) before the
    // iterator is restored. Do not add calls that execute bytecode here.
    let mut my_iter = std::mem::take(receiver.as_list_iterator_mut(&mut vm.heap));
    let my_list = my_iter.get_list(&vm.heap);
    let result = if my_iter.index < my_list.items.len() {
        let value = my_list.items[my_iter.index];
        my_iter.index += 1;
        Ok(value)
    } else {
        Ok(Value::StopIteration)
    };

    *receiver.as_list_iterator_mut(&mut vm.heap) = my_iter;
    result
}

pub(super) fn list_iter_str_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    let list = receiver.as_list_iterator(&vm.heap).list;
    let list_string = vm.value_to_string(&list.into())?.to_value(&vm.heap);

    let string = format!("<list iterator of {list_string}>");
    Ok(vm.heap.string_id(&string).into())
}

pub(super) fn list_add_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let my_list = receiver.as_list(&vm.heap);
    if let Value::Instance(instance) = &args[0]
        && let Some(NativeClass::List(other_list)) = &instance.to_value(&vm.heap).backing
    {
        // Create a new list with combined contents
        let mut items = Vec::new();
        items.extend_from_slice(&my_list.items);
        items.extend_from_slice(&other_list.items);

        let new_list = List::new(items);

        // Create a new List instance
        let instance = Instance::new(
            *vm.heap.native_classes.get("List").unwrap(),
            Some(new_list.into()),
        );
        Ok(vm.heap.add_instance(instance))
    } else {
        Err(vm
            .throw(
                TypeError,
                &format!(
                    "Can only add a list to another list, got `{}`.",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err())
    }
}

pub(super) fn list_len_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let my_list = receiver.as_list(&vm.heap);
    Ok(Number::from_usize(my_list.items.len(), &mut vm.heap).into())
}

pub(super) fn list_bool_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let is_empty = receiver.as_list(&vm.heap).items.is_empty();
    Ok((!is_empty).into())
}

/// Constructor for List that accepts variable number of arguments.
/// `List()` creates empty list, `List(1, 2, 3)` creates [1, 2, 3].
pub(super) fn list_init_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let items = if args.len() == 1
        && let Some(iter_items) = vm.collect_items_from_iterable(args[0])?
    {
        iter_items
    } else {
        args.to_vec()
    };
    let list = receiver.as_list_mut(&mut vm.heap);
    list.items = items;
    Ok(*receiver)
}

pub(super) fn list_str_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let mut string = String::from("[");

    // Re-fetch by index each step: `value_to_string` may re-enter and mutate
    // or reallocate the list.
    let mut index = 0;
    while index < receiver.as_list(&vm.heap).items.len() {
        if index > 0 {
            string.push_str(", ");
        }

        let element = receiver.as_list(&vm.heap).items[index];
        string.push_str(vm.value_to_string(&element)?.to_value(&vm.heap));

        index += 1;
    }

    string.push(']');

    Ok(vm.heap.string_id(&string).into())
}

/// Reverse the list in place. `list.reverse()`.
pub(super) fn list_reverse_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    receiver.as_list_mut(&mut vm.heap).items.reverse();
    Ok(Value::Nil)
}

/// Extend the list with elements from an iterable. `list.extend(iterable)`.
pub(super) fn list_extend_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let Some(items) = vm.collect_items_from_iterable(args[0])? else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "Expected an iterable, got `{}`.",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    receiver.as_list_mut(&mut vm.heap).items.extend(items);
    Ok(Value::Nil)
}

/// Remove all elements from the list. `list.clear()`.
pub(super) fn list_clear_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    receiver.as_list_mut(&mut vm.heap).items.clear();
    Ok(Value::Nil)
}

/// Return a shallow copy of the list. `list.copy()`.
pub(super) fn list_copy_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let items = receiver.as_list(&vm.heap).items.clone();
    let new_list = List::new(items);
    let instance = Instance::new(
        *vm.heap.native_classes.get("List").unwrap(),
        Some(new_list.into()),
    );
    Ok(vm.heap.add_instance(instance))
}

/// `list == other`: element-wise equality, respecting each element's `__eq__`.
/// Only equal to another list; any other type is unequal (never an error).
pub(super) fn list_eq_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let other = args[0];
    // Same object is trivially equal, and short-circuits self-referential lists.
    if receiver.is(&other) {
        return Ok(true.into());
    }
    if !is_list(other, &vm.heap) {
        return Ok(false.into());
    }
    if receiver.as_list(&vm.heap).items.len() != other.as_list(&vm.heap).items.len() {
        return Ok(false.into());
    }
    // Re-fetch by index each step: `compare_values_eq` re-enters and may mutate
    // or reallocate either list.
    let mut index = 0;
    while index < receiver.as_list(&vm.heap).items.len()
        && index < other.as_list(&vm.heap).items.len()
    {
        let element = receiver.as_list(&vm.heap).items[index];
        let other_element = other.as_list(&vm.heap).items[index];
        if !vm.compare_values_eq(element, other_element)? {
            return Ok(false.into());
        }
        index += 1;
    }
    // A reentrant element `__eq__` may have resized either list mid-comparison;
    // recheck the lengths agree so a shrunk list is not reported equal.
    if receiver.as_list(&vm.heap).items.len() != other.as_list(&vm.heap).items.len() {
        return Ok(false.into());
    }
    Ok(true.into())
}

/// Lexicographic ordering of `receiver` against `other`, or `None` when `other`
/// is not a list (the caller raises `TypeError`).
fn list_ordering(vm: &mut VM, receiver: &Value, other: Value) -> VmResult<Option<Ordering>> {
    if !is_list(other, &vm.heap) {
        return Ok(None);
    }
    if receiver.is(&other) {
        return Ok(Some(Ordering::Equal));
    }
    // Re-fetch by index each step: comparing elements re-enters the interpreter
    // and may mutate or reallocate either list.
    let mut index = 0;
    loop {
        let self_len = receiver.as_list(&vm.heap).items.len();
        let other_len = other.as_list(&vm.heap).items.len();
        if index >= self_len || index >= other_len {
            return Ok(Some(self_len.cmp(&other_len)));
        }
        let element = receiver.as_list(&vm.heap).items[index];
        let other_element = other.as_list(&vm.heap).items[index];
        if !vm.compare_values_eq(element, other_element)? {
            let ordering = if vm.compare_values_lt(element, other_element)? {
                Ordering::Less
            } else {
                Ordering::Greater
            };
            return Ok(Some(ordering));
        }
        index += 1;
    }
}

/// Shared body of the four list ordering dunders.
fn list_compare(vm: &mut VM, receiver: &Value, other: Value, kind: Comparison) -> VmResult<Value> {
    match list_ordering(vm, receiver, other)? {
        Some(ordering) => Ok(kind.holds_for(ordering).into()),
        None => Err(vm
            .throw(
                TypeError,
                &format!(
                    "Can only compare a list to another list, got `{}`.",
                    other.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

pub(super) fn list_lt_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    list_compare(vm, receiver, args[0], Comparison::Less)
}

pub(super) fn list_le_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    list_compare(vm, receiver, args[0], Comparison::LessEqual)
}

pub(super) fn list_gt_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    list_compare(vm, receiver, args[0], Comparison::Greater)
}

pub(super) fn list_ge_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    list_compare(vm, receiver, args[0], Comparison::GreaterEqual)
}
