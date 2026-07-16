//! Methods of the native `Dict` class.

use crate::vm::ExceptionKind::{KeyError, TypeError};
use crate::{
    value::{Dict, NativeClass, Number, Value},
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
        // Copy the pair out before re-entering the interpreter; the values
        // stay rooted through `args` on the VM stack.
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
        if let Some((key, value)) = pair {
            Dict::add(vm, receiver, key, value)?;
        } else {
            return Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "Dict initializer expects 2-element tuples, got `{}`.",
                        arg.to_string(&vm.heap)
                    ),
                )
                .unwrap_err());
        }
    }
    Ok(*receiver)
}
