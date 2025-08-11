//! Methods of the native `List` class.

use crate::{
    value::{Instance, List, ListIterator, NativeClass, Number, Value},
    vm::VM,
};

/// Append an item to the end of the list.
/// `list.append(a)`.
pub(super) fn list_append_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let list = receiver.as_list_mut(&mut vm.heap);
    list.items.push(*args[0]);
    Ok(Value::Nil)
}

/// Pop an item off the end of the list via `list.pop()`
/// or from a specified index `list.pop(index)`.
/// Returns the removed element.
pub(super) fn list_pop_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let index = if args.is_empty() {
        None
    } else {
        let index = match &args[0] {
            Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
                Ok(index) => index,
                Err(_) => {
                    return Err(format!(
                        "Can not index into list with negative or too large numbers, got `{}`.",
                        n.to_string(&vm.heap)
                    ));
                }
            },
            x => {
                return Err(format!(
                    "Can only index into list with integer, got `{}`.",
                    x.to_string(&vm.heap)
                ));
            }
        };
        Some(index)
    };

    let my_list = receiver.as_list_mut(&mut vm.heap);

    match index {
        Some(index) => {
            let length = my_list.items.len();
            if index >= length {
                Err(format!(
                    "Index `{index}` is out of bounds of list with len `{length}`."
                ))
            } else {
                Ok(my_list.items.remove(index))
            }
        }
        None => my_list
            .items
            .pop()
            .map_or_else(|| Err("Can't 'pop' from an empty list.".to_string()), Ok),
    }
}

/// Get an item at a specified index `list[a]`.
pub(super) fn list_get_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let index = match &args[0] {
        Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
            Ok(index) => index,
            Err(_) => {
                return Err(format!(
                    "Can not index into list with negative or too large numbers, got `{}`.",
                    n.to_string(&vm.heap)
                ));
            }
        },
        x => {
            return Err(format!(
                "Can only index into list with integer, got `{}`.",
                x.to_string(&vm.heap)
            ));
        }
    };

    let my_list = receiver.as_list(&vm.heap);

    my_list.items.get(index).map_or_else(
        || {
            Err(format!(
                "Index `{index}` is out of bounds of list with len `{}`.",
                my_list.items.len()
            ))
        },
        |value| Ok(*value),
    )
}

/// Set the item at the specified index `list[a] = b`.
pub(super) fn list_set_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let index = match &args[0] {
        Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
            Ok(index) => index,
            Err(_) => {
                return Err(format!(
                    "Can not index into list with negative or too large numbers, got `{}`.",
                    n.to_string(&vm.heap)
                ));
            }
        },
        x => {
            return Err(format!(
                "Can only index into list with integer, got `{}`.",
                x.to_string(&vm.heap)
            ));
        }
    };

    let my_list = receiver.as_list_mut(&mut vm.heap);

    match my_list.items.get_mut(index) {
        Some(value) => {
            *value = *args[1];
            Ok(Value::Nil)
        }
        None => Err(format!(
            "Index `{index}` is out of bounds of list with len `{}`.",
            my_list.items.len()
        )),
    }
}

/// Insert an item into the list at the specified index.
/// `list.insert(index, value)`, such that afterwards `list[index] = value`.
pub(super) fn list_insert_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let index = match &args[0] {
        Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
            Ok(index) => index,
            Err(_) => {
                return Err(format!(
                    "Can not index into list with negative or too large numbers, got `{}`.",
                    n.to_string(&vm.heap)
                ));
            }
        },
        x => {
            return Err(format!(
                "Can only index into list with integer, got `{}`.",
                x.to_string(&vm.heap)
            ));
        }
    };

    let my_list = receiver.as_list_mut(&mut vm.heap);

    let length = my_list.items.len();
    if index > length {
        Err(format!(
            "Index `{index}` is out of bounds of list with len `{length}`."
        ))
    } else {
        my_list.items.insert(index, *args[1]);
        Ok(Value::Nil)
    }
}

/// Check if the list contains a value `list.contains(a)`.
/// This also powers `a in list`.
pub(super) fn list_contains_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_list = receiver.as_list(&vm.heap);
    Ok(my_list
        .items
        .iter()
        .any(|el| el.eq(args[0], &vm.heap))
        .into())
}

/// Produce an iterator over the list `var iter = list.__iter__()`.
/// Used by `foreach (var a in list)`.
pub(super) fn list_iter_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_list = receiver.as_instance();
    let my_iterator = ListIterator::new(*my_list);
    let target_class = vm.heap.native_classes.get("ListIterator").unwrap();
    let my_instance = Instance::new(*target_class, Some(my_iterator.into()));
    Ok(vm.heap.add_instance(my_instance))
}

/// Get the next element from a listiterator `var next = listiter.__next__()`.
/// Powers `foreach (var a in list)`
#[allow(clippy::option_if_let_else)]
pub(super) fn list_iter_next_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let mut my_iter = std::mem::take(receiver.as_list_iter_mut(&mut vm.heap));
    let my_list = my_iter.get_list(&vm.heap);
    let result = if my_iter.index < my_list.items.len() {
        let value = my_list.items[my_iter.index];
        my_iter.index += 1;
        Ok(value)
    } else {
        Ok(Value::StopIteration)
    };

    *receiver.as_list_iter_mut(&mut vm.heap) = my_iter;
    result
}

pub(super) fn list_add_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_list = receiver.as_list(&vm.heap);
    match &args[0] {
        Value::Instance(instance) => match &instance.to_value(&vm.heap).backing {
            Some(NativeClass::List(other_list)) => {
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
            }
            _ => Err(format!(
                "Can only add a list to another list, got `{}`.",
                instance.to_value(&vm.heap).to_string(&vm.heap)
            )),
        },
        x => Err(format!(
            "Can only add a list to another list, got `{}`.",
            x.to_string(&vm.heap)
        )),
    }
}

pub(super) fn list_len_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_list = receiver.as_list(&vm.heap);
    Ok(Number::from_usize(my_list.items.len(), &mut vm.heap).into())
}

pub(super) fn list_bool_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let is_empty = receiver.as_list(&vm.heap).items.is_empty();
    Ok((!is_empty).into())
}
