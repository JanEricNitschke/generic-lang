use crate::{
    value::{Instance, ListIterator, Number, Value},
    vm::VM,
};

pub(super) fn list_append_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let list = receiver.as_list_mut();
    list.items.push(*args[0]);
    Ok(Value::Nil)
}

pub(super) fn list_pop_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let index = if args.is_empty() {
        None
    } else {
        let index = match &args[0] {
            Value::Number(Number::Integer(n)) => match usize::try_from(*n) {
                Ok(index) => index,
                Err(_) => {
                    return Err(format!(
                        "Can not index into list with negative or too large numbers, got `{n}`."
                    ));
                }
            },
            x => {
                return Err(format!("Can only index into list with integer, got `{x}`."));
            }
        };
        Some(index)
    };

    let my_list = receiver.as_list_mut();

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

pub(super) fn list_get_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let index = match &args[0] {
        Value::Number(Number::Integer(n)) => match usize::try_from(*n) {
            Ok(index) => index,
            Err(_) => {
                return Err(format!(
                    "Can not index into list with negative or too large numbers, got `{n}`."
                ));
            }
        },
        x => {
            return Err(format!("Can only index into list with integer, got `{x}`."));
        }
    };

    let my_list = receiver.as_list();

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

pub(super) fn list_set_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let index = match &args[0] {
        Value::Number(Number::Integer(n)) => match usize::try_from(*n) {
            Ok(index) => index,
            Err(_) => {
                return Err(format!(
                    "Can not index into list with negative or too large numbers, got `{n}`."
                ));
            }
        },
        x => {
            return Err(format!("Can only index into list with integer, got `{x}`."));
        }
    };

    let my_list = receiver.as_list_mut();

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

pub(super) fn list_insert_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let index = match &args[0] {
        Value::Number(Number::Integer(n)) => match usize::try_from(*n) {
            Ok(index) => index,
            Err(_) => {
                return Err(format!(
                    "Can not index into list with negative or too large numbers, got `{n}`."
                ));
            }
        },
        x => {
            return Err(format!("Can only index into list with integer, got `{x}`."));
        }
    };

    let my_list = receiver.as_list_mut();

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

pub(super) fn list_contains_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_list = receiver.as_list();
    Ok(my_list.items.contains(args[0]).into())
}

pub(super) fn list_iter_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_list = receiver.as_instance();
    let my_iterator = ListIterator::new(Some(*my_list));
    let target_class = vm.heap.native_classes.get("ListIterator").unwrap();
    let my_instance = Instance::new(*target_class, Some(my_iterator.into()));
    Ok(vm.heap.add_instance(my_instance))
}

pub(super) fn list_iter_next_native(
    _vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_iter = receiver.as_list_iter_mut();
    let my_list = my_iter.get_list();
    match my_list {
        Some(my_list) => {
            if my_iter.index < my_list.items.len() {
                let value = my_list.items[my_iter.index];
                my_iter.index += 1;
                Ok(value)
            } else {
                Ok(Value::StopIteration)
            }
        }
        None => Ok(Value::StopIteration),
    }
}
