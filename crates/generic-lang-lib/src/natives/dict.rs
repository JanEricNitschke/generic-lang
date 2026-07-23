//! Methods of the native `Dict` class.

use crate::vm::ExceptionKind::{KeyError, RuntimeError, TypeError};
use crate::{
    value::{Dict, DictIterMode, DictIterator, Instance, NativeClass, Number, Tuple, Value},
    vm::{VM, errors::VmResult},
};

/// Get an item via `dict[a]`, where `a` is a hashable value type.
/// Supports all value types that implement hashable functionality.
pub(super) fn dict_get_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    match Dict::get(vm, receiver, args[0])? {
        Some(value) => Ok(value),
        None => Err(vm
            .throw(
                KeyError,
                &format!("Key `{}` not found.", args[0].to_string(&vm.heap)),
            )
            .unwrap_err()),
    }
}

/// Set an item via `dict[a] = b`, where `a` is a hashable value type.
/// Supports all value types that implement hashable functionality.
pub(super) fn dict_set_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    Dict::add(vm, receiver, args[0], args[1])?;
    Ok(Value::Nil)
}

/// Remove a key via `dict.pop(a)` and return its value, like Python's
/// `dict.pop`. Throws `KeyError` if the key is not present.
pub(super) fn dict_pop_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    match Dict::remove(vm, receiver, args[0])? {
        Some(value) => Ok(value),
        None => Err(vm
            .throw(
                KeyError,
                &format!("Key `{}` not found.", args[0].to_string(&vm.heap)),
            )
            .unwrap_err()),
    }
}

pub(super) fn dict_contains_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    Ok(Dict::contains(vm, receiver, args[0])?.into())
}

pub(super) fn dict_len_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let dict = receiver.as_dict(&vm.heap);
    Ok(Number::from_usize(dict.items.len(), &mut vm.heap).into())
}

pub(super) fn dict_bool_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let is_empty = receiver.as_dict(&vm.heap).items.is_empty();
    Ok((!is_empty).into())
}

/// Constructor for Dict that accepts variable number of arguments.
///
/// `Dict()` creates empty dict,
/// `Dict((1, "a"), (2, "b"), (3, "c"))` creates {1: "a", 2: "b", 3: "c"}.
/// Only hashable values are allowed as keys.
pub(super) fn dict_init_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    receiver.as_dict_mut(&mut vm.heap).items.clear(); // reset

    for arg in args {
        // Copy the pair out before re-entering the interpreter. The pair
        // containers stay rooted through the dispatch site's copies on the
        // VM stack (`execute_native_method_call` truncates them only after
        // this native returns), but a List pair can be mutated by user
        // `__hash__`/`__eq__` during the add, dropping its references - so
        // the key and value copies must be rooted separately below. (The `Option`
        // round-trip also ends the heap borrow before `Dict::add` needs the
        // VM mutably.)
        let pair = if let Value::Instance(inst) = arg
            && let Some(backing) = &inst.to_value(&vm.heap).backing
            && let Some(items) = match backing {
                NativeClass::Tuple(tuple) => Some(tuple.items()),
                NativeClass::List(list) => Some(list.items.as_ref()),
                _ => None,
            }
            && let [key, value] = items
        {
            Some((*key, *value))
        } else {
            None
        };
        let Some((key, value)) = pair else {
            return Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "Dict initializer expects 2-element tuples or lists, got `{}`.",
                        arg.to_string(&vm.heap)
                    ),
                )
                .unwrap_err());
        };
        // Same discipline as `VM::for_each_rooted`: keep the copies on the
        // VM stack while the add re-enters the interpreter. On error they
        // stay below the pending exception for the handler to truncate.
        let roots_start = vm.stack.len();
        vm.stack.push(key);
        vm.stack.push(value);
        Dict::add(vm, receiver, key, value)?;
        vm.stack.truncate(roots_start);
    }
    Ok(*receiver)
}

#[allow(clippy::literal_string_with_formatting_args)]
pub(super) fn dict_str_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let start = vm.stack.len();
    for (key, value, _hash) in &receiver.as_dict(&vm.heap).items {
        vm.stack.push(*key);
        vm.stack.push(*value);
    }
    let end = vm.stack.len();

    if start == end {
        return Ok(vm.heap.string_id(&"{:}").into());
    }

    let mut string = String::from("{");

    for stack_index in (start..end).step_by(2) {
        if stack_index > start {
            string.push_str(", ");
        }

        let key = vm.stack[stack_index];
        let value = vm.stack[stack_index + 1];

        string.push_str(vm.value_to_string(&key)?.to_value(&vm.heap));
        string.push_str(": ");
        string.push_str(vm.value_to_string(&value)?.to_value(&vm.heap));
    }

    string.push('}');

    vm.stack.truncate(start);
    Ok(vm.heap.string_id(&string).into())
}

/// Shared helper to create a `DictIterator` instance with a given mode.
fn dict_make_iterator(vm: &mut VM, receiver: &Value, mode: DictIterMode) -> Value {
    let dict_instance = receiver.as_instance();
    let size = receiver.as_dict(&vm.heap).items.len();
    let target_class = vm.heap.native_classes.get("DictIterator").unwrap();
    let iterator = DictIterator::new(*dict_instance, size, mode);
    let instance = Instance::new(*target_class, Some(iterator.into()));
    vm.heap.add_instance(instance)
}

/// Iterators are their own iterators.
pub(super) fn dict_iter_iter_native(
    _vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    Ok(*receiver)
}

/// Produce an iterator over the dict's keys. `dict.__iter__()`.
pub(super) fn dict_iter_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(dict_make_iterator(vm, receiver, DictIterMode::Keys))
}

/// `Dict.keys()` - return a lazy iterator over keys.
pub(super) fn dict_keys_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(dict_make_iterator(vm, receiver, DictIterMode::Keys))
}

/// `Dict.values()` - return a lazy iterator over values.
pub(super) fn dict_values_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    Ok(dict_make_iterator(vm, receiver, DictIterMode::Values))
}

/// `Dict.items()` - return a lazy iterator over (key, value) tuples.
pub(super) fn dict_items_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(dict_make_iterator(vm, receiver, DictIterMode::Items))
}

/// Get the next element from a dict iterator (`__next__()`).
pub(super) fn dict_iter_next_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    // GC-safety: mem::take pattern, same as list/tuple/range iterators.
    let mut iter = std::mem::take(receiver.as_dict_iterator_mut(&mut vm.heap));
    let dict = iter.get_dict(&vm.heap);

    // Check if the dict's size changed since the iterator was created.
    if dict.items.len() != iter.size {
        *receiver.as_dict_iterator_mut(&mut vm.heap) = iter;
        return Err(vm
            .throw(RuntimeError, "dict changed size during iteration")
            .unwrap_err());
    }

    let result = if let Some((key, value, _hash)) = dict.items.iter().nth(iter.index) {
        iter.index += 1;
        match iter.mode {
            DictIterMode::Keys => Ok(*key),
            DictIterMode::Values => Ok(*value),
            DictIterMode::Items => {
                let tuple = Tuple::new(vec![*key, *value]);
                let instance = Instance::new(
                    *vm.heap.native_classes.get("Tuple").unwrap(),
                    Some(tuple.into()),
                );
                Ok(vm.heap.add_instance(instance))
            }
        }
    } else {
        Ok(Value::StopIteration)
    };

    *receiver.as_dict_iterator_mut(&mut vm.heap) = iter;
    result
}

/// `Dict.get(key, default)` - return value for key, or default if absent.
pub(super) fn dict_get_method_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    match Dict::get(vm, receiver, args[0])? {
        Some(value) => Ok(value),
        None => Ok(args[1]),
    }
}

/// Remove all entries from the dict. `dict.clear()`.
pub(super) fn dict_clear_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let dict = receiver.as_dict_mut(&mut vm.heap);
    dict.items.clear();
    Ok(Value::Nil)
}

pub(super) fn dict_iter_str_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    let iter = receiver.as_dict_iterator(&vm.heap);
    let mode = iter.mode.to_string().to_string();

    let dict = iter.dict;
    let dict_string = vm.value_to_string(&dict.into())?.to_value(&vm.heap);

    let string = format!("<dict_{mode} iterator of {dict_string}>");
    Ok(vm.heap.string_id(&string).into())
}
