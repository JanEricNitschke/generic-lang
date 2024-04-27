use crate::{
    value::{List, ListIterator, Number, Value},
    vm::VM,
};

pub(super) fn init_list_native(
    vm: &mut VM,
    _receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let list = List::new(*vm.heap.native_classes.get("List").unwrap());
    Ok(vm.heap.add_list(list))
}

pub(super) fn append_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    match receiver {
        Value::List(list) => {
            list.items.push(*args[0]);
            Ok(Value::Nil)
        }
        x => Err(format!(
            "'append' expects its first argument to be a list, got `{x}` instead."
        )),
    }
}

pub(super) fn pop_native(
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

    let my_list = match receiver {
        Value::List(list) => list,
        x => {
            return Err(format!(
                "'pop' expects its first argument to be a list, got `{x}` instead."
            ))
        }
    };

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

pub(super) fn insert_native(
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

    let my_list = match receiver {
        Value::List(list) => list,
        x => {
            return Err(format!(
                "'insert' expects its first argument to be a list, got `{x}` instead."
            ))
        }
    };

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

pub(super) fn contains_native(
    _vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_list = match receiver {
        Value::List(list) => list,
        x => {
            return Err(format!(
                "'contains' expects to be called on a list, got `{x}` instead."
            ))
        }
    };
    Ok(my_list.items.contains(args[0]).into())
}

pub(super) fn iter_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_list = match receiver {
        Value::List(list) => list,
        x => {
            return Err(format!(
                "'iter' expects to be called on a list, got `{x}` instead."
            ))
        }
    };
    let my_iterator = ListIterator::new(
        *my_list,
        *vm.heap.native_classes.get("ListIterator").unwrap(),
    );
    Ok(vm.heap.add_list_iterator(my_iterator))
}

pub(super) fn list_iter_next_native(
    _vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let mut my_iter = match receiver {
        Value::ListIterator(iter) => *iter,
        x => {
            return Err(format!(
                "'next' expects to be called on a list iterator, got `{x}` instead."
            ))
        }
    };
    if my_iter.index < my_iter.list.items.len() {
        let value = my_iter.list.items[my_iter.index];
        my_iter.index += 1;
        Ok(value)
    } else {
        Ok(Value::StopIteration)
    }
}
