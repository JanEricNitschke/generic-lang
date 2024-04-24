use crate::{
    heap::Heap,
    value::{List, Number, Value},
};

pub(super) fn init_list_native(
    heap: &mut Heap,
    _receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let list = List::new(*heap.native_classes.get("List").unwrap());
    Ok(heap.add_list(list))
}

pub(super) fn append_native(
    _heap: &mut Heap,
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
    _heap: &mut Heap,
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
    _heap: &mut Heap,
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
    _heap: &mut Heap,
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
pub(super) fn len_native(_heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    match &args[0] {
        Value::List(list) => Ok((list.items.len() as i64).into()),
        x => Err(format!("'len' expected list argument, got: `{x}` instead.")),
    }
}

pub(super) fn getattr_native(heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    match (&args[0], &args[1]) {
        (Value::Instance(instance), Value::String(string_id)) => {
            let field = &heap.strings[string_id];
            instance.fields.get(field).map_or_else(
                || Err(format!("Undefined property '{}'.", *field)),
                |value_id| Ok(*value_id),
            )
        }
        (instance @ Value::Instance(_), x) => Err(format!(
            "`getattr` can only index with string indexes, got: `{x}` (instance: `{instance}`)"
        )),
        (not_instance, _) => Err(format!(
            "`getattr` only works on instances, got `{not_instance}`"
        )),
    }
}

pub(super) fn setattr_native(heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    let field = if let Value::String(ref string_id) = args[1] {
        heap.strings[string_id].clone()
    } else {
        return Err(format!(
            "`setattr` can only index with string indexes, got: `{}` (instance: `{}`)",
            args[1], args[0]
        ));
    };
    let value = *args[2];
    if let Value::Instance(instance) = args[0] {
        instance.fields.insert(field, value);
        Ok(Value::Nil)
    } else {
        Err(format!(
            "`setattr` only works on instances, got `{}`",
            args[0]
        ))
    }
}

pub(super) fn hasattr_native(heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    match (&args[0], &args[1]) {
        (Value::Instance(instance), Value::String(string_id)) => Ok(Value::Bool(
            instance.fields.contains_key(&heap.strings[string_id]),
        )),
        (instance @ Value::Instance(_), x) => Err(format!(
            "`hasattr` can only index with string indexes, got: `{x}` (instance: `{instance}`)"
        )),
        (not_instance, _) => Err(format!(
            "`hasattr` only works on instances, got `{not_instance}`"
        )),
    }
}

pub(super) fn delattr_native(heap: &mut Heap, args: &mut [&mut Value]) -> Result<Value, String> {
    if let Value::String(ref string_id) = args[1] {
        let field = &heap.strings[string_id];
        if let Value::Instance(instance) = args[0] {
            match instance.fields.remove(field) {
                Some(_) => Ok(Value::Nil),
                None => Err(format!("Undefined property '{field}'.")),
            }
        } else {
            Err(format!(
                "`delattr` only works on instances, got `{}`",
                args[0]
            ))
        }
    } else {
        Err(format!(
            "`delattr` can only index with string indexes, got: `{}` (instance: `{}`)",
            args[1], args[0]
        ))
    }
}
