use crate::{
    value::{List, Number, Value},
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

    for value in &my_list.items {
        if value == args[0] {
            return Ok(Value::Bool(true));
        }
    }
    Ok(Value::Bool(false))
}

#[allow(clippy::cast_possible_wrap)]
pub(super) fn len_native(_vm: &mut VM, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::List(list) => Ok((list.items.len() as i64).into()),
        x => Err(format!("'len' expected list argument, got: `{x}` instead.")),
    }
}
