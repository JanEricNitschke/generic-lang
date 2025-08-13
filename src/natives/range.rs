//! Methods of the native `Range` class.

use crate::{
    value::{GenericInt, Instance, Number, Range, RangeIterator, Value},
    vm::VM,
};

/// Check if the range contains a value `range.contains(a)`.
/// This also powers `a in range`.
pub(super) fn range_contains_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_range = receiver.as_range(&vm.heap);
    match args[0] {
        Value::Number(Number::Integer(arg)) => Ok(Value::Bool(my_range.contains(arg, &vm.heap))),
        _ => Ok(Value::Bool(false)),
    }
}

/// Produce an iterator over the range `var iter = range.__iter__()`.
/// Used by `foreach (var a in range)`.
pub(super) fn range_iter_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_range = receiver.as_instance();
    let my_iterator = RangeIterator::new(*my_range);
    let target_class = vm.heap.native_classes.get("RangeIterator").unwrap();
    let my_instance = Instance::new(*target_class, Some(my_iterator.into()));
    Ok(vm.heap.add_instance(my_instance))
}

/// Get the next element from a range iterator `var next = rangeiter.__next__()`.
/// Powers `foreach (var a in range)`
#[allow(clippy::option_if_let_else)]
pub(super) fn range_iter_next_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let mut my_iter = std::mem::take(receiver.as_range_iter_mut(&mut vm.heap));
    let my_range = my_iter.get_range(&vm.heap);
    let my_range_start = my_range.start();
    let my_range_end = my_range.end();
    let is_forward = my_range.is_forward(&vm.heap);

    let current = my_range_start.add(my_iter.offset, &mut vm.heap);

    let result = if current.eq(&my_range_end, &vm.heap) {
        Ok(Value::StopIteration)
    } else {
        let offset_change = if is_forward {
            GenericInt::Small(1)
        } else {
            GenericInt::Small(-1)
        };
        my_iter.offset = my_iter.offset.add(offset_change, &mut vm.heap);
        Ok(Value::Number(Number::Integer(current)))
    };

    *receiver.as_range_iter_mut(&mut vm.heap) = my_iter;
    result
}

pub(super) fn range_len_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_range = *receiver.as_range(&vm.heap);
    let length = my_range.len(&mut vm.heap);
    Ok(Number::Integer(length).into())
}

pub(super) fn range_bool_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> Result<Value, String> {
    let my_range = receiver.as_range(&vm.heap);
    let is_non_empty = !my_range.is_empty(&vm.heap);
    Ok(is_non_empty.into())
}

/// Constructor for Range that accepts exactly 2 arguments.
/// `Range(start, end)` creates a range from start to end (exclusive).
pub(super) fn range_init_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> Result<Value, String> {
    let (start, end) = match (&args[0], &args[1]) {
        (Value::Number(Number::Integer(s)), Value::Number(Number::Integer(e))) => (*s, *e),
        (s, e) => {
            return Err(format!(
                "Range arguments must be integers, got `{}, {}`.",
                s.to_string(&vm.heap),
                e.to_string(&vm.heap),
            ));
        }
    };

    let range = receiver.as_range_mut(&mut vm.heap);
    *range = Range::new(start, end);
    Ok(*receiver)
}
