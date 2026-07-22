//! Methods of the native `Tuple` class.

use crate::vm::ExceptionKind::{IndexError, TypeError, ValueError};
use crate::{
    value::{Instance, NativeClass, Number, Tuple, TupleIterator, Value},
    vm::{VM, errors::VmResult},
};

/// Get an item at a specified index `tuple[a]`.
pub(super) fn tuple_get_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let index = match &args[0] {
        Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
            Ok(index) => index,
            Err(err) => return Err(vm.throw(ValueError, &err).unwrap_err()),
        },
        x => {
            return Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "Can only index into tuple with integer, got `{}`.",
                        x.to_string(&vm.heap)
                    ),
                )
                .unwrap_err());
        }
    };

    let my_tuple = receiver.as_tuple(&vm.heap);

    match my_tuple.items().get(index) {
        Some(value) => Ok(*value),
        None => Err(vm
            .throw(
                IndexError,
                &format!(
                    "Index `{index}` is out of bounds of tuple with len `{}`.",
                    my_tuple.items().len()
                ),
            )
            .unwrap_err()),
    }
}

/// Check if the tuple contains a value `tuple.contains(a)`.
/// This also powers `a in tuple`.
pub(super) fn tuple_contains_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let needle = args[0];
    let mut index = 0;
    while index < receiver.as_tuple(&vm.heap).items().len() {
        let element = receiver.as_tuple(&vm.heap).items()[index];
        if vm.compare_values(element, needle)? {
            return Ok(true.into());
        }
        index += 1;
    }
    Ok(false.into())
}

/// Produce an iterator over the tuple `var iter = tuple.__iter__()`.
/// Used by `foreach (var a in tuple)`.
pub(super) fn tuple_iter_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let my_tuple = receiver.as_instance();
    let my_iterator = TupleIterator::new(*my_tuple);
    let target_class = vm.heap.native_classes.get("TupleIterator").unwrap();
    let my_instance = Instance::new(*target_class, Some(my_iterator.into()));
    Ok(vm.heap.add_instance(my_instance))
}

/// Get the next element from a tupleiterator `var next = tupleiter.__next__()`.
/// Powers `foreach (var a in tuple)`
#[allow(clippy::option_if_let_else)]
pub(super) fn tuple_iter_next_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    // GC-safety: taking the iterator out of the heap hides it from the GC.
    // This is only sound because nothing below re-enters the interpreter
    // (GC runs exclusively from the instruction dispatch loop) before the
    // iterator is restored. Do not add calls that execute bytecode here.
    let mut my_iter = std::mem::take(receiver.as_tuple_iterator_mut(&mut vm.heap));
    let my_tuple = my_iter.get_tuple(&vm.heap);
    let result = if my_iter.index < my_tuple.items().len() {
        let value = my_tuple.items()[my_iter.index];
        my_iter.index += 1;
        Ok(value)
    } else {
        Ok(Value::StopIteration)
    };

    *receiver.as_tuple_iterator_mut(&mut vm.heap) = my_iter;
    result
}

pub(super) fn tuple_add_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let my_tuple = receiver.as_tuple(&vm.heap);
    if let Value::Instance(instance) = &args[0]
        && let Some(NativeClass::Tuple(other_tuple)) = &instance.to_value(&vm.heap).backing
    {
        // Create a new tuple with combined contents
        let mut items = Vec::new();
        items.extend_from_slice(my_tuple.items());
        items.extend_from_slice(other_tuple.items());

        let new_tuple = Tuple::new(items);

        // Create a new Tuple instance
        let instance = Instance::new(
            *vm.heap.native_classes.get("Tuple").unwrap(),
            Some(new_tuple.into()),
        );
        Ok(vm.heap.add_instance(instance))
    } else {
        Err(vm
            .throw(
                TypeError,
                &format!(
                    "Can only add a tuple to another tuple, got `{}`.",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err())
    }
}

pub(super) fn tuple_len_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let my_tuple = receiver.as_tuple(&vm.heap);
    Ok(Number::from_usize(my_tuple.items().len(), &mut vm.heap).into())
}

pub(super) fn tuple_bool_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let is_empty = receiver.as_tuple(&vm.heap).items().is_empty();
    Ok((!is_empty).into())
}

/// Constructor for Tuple that accepts variable number of arguments.
/// `Tuple()` creates empty tuple, `Tuple(1, 2, 3)` creates (1, 2, 3).
pub(super) fn tuple_init_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let items: Vec<Value> = args.to_vec();
    let tuple = receiver.as_tuple_mut(&mut vm.heap);
    *tuple = Tuple::new(items);
    Ok(*receiver)
}

pub(super) fn tuple_str_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let items_len = receiver.as_tuple(&vm.heap).items().len();

    let mut string = String::from("(");

    let mut index = 0;
    while index < items_len {
        if index > 0 {
            string.push_str(", ");
        }

        let element = receiver.as_tuple(&vm.heap).items()[index];
        string.push_str(vm.value_to_string(&element)?.to_value(&vm.heap));

        index += 1;
    }

    // Single element tuple needs the trailing comma
    if items_len == 1 {
        string.push(',');
    }

    string.push(')');

    Ok(vm.heap.string_id(&string).into())
}
