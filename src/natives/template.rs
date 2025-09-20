//! Methods of the native `Template` and `Interpolation` classes.

use crate::{
    value::{Instance, TemplateIterator, Tuple, Value},
    vm::{VM, errors::VmResult},
};

/// Produce an iterator over the template `var iter = template.__iter__()`.
/// Used by `foreach (var a in template)`.
pub(super) fn template_iter_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> VmResult<Value> {
    let my_template = receiver.as_instance();
    let my_iterator = TemplateIterator::new(*my_template);
    let target_class = vm.heap.native_classes.get("TemplateIterator").unwrap();
    let my_instance = Instance::new(*target_class, Some(my_iterator.into()));
    Ok(vm.heap.add_instance(my_instance))
}

pub(super) fn template_strings_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> VmResult<Value> {
    let my_template = receiver.as_template(&vm.heap);
    let string_values: Vec<Value> = my_template
        .strings()
        .iter()
        .map(|&string_id| string_id.into())
        .collect();

    let tuple = Tuple::new(string_values);
    let instance = Instance::new(
        *vm.heap.native_classes.get("Tuple").unwrap(),
        Some(tuple.into()),
    );
    let instance_value = vm.heap.add_instance(instance);

    Ok(instance_value)
}

pub(super) fn template_interpolations_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> VmResult<Value> {
    let my_template = receiver.as_template(&vm.heap);
    let interpolation_values: Vec<Value> = my_template
        .interpolations()
        .iter()
        .map(|&interp| interp.into())
        .collect();

    let tuple = Tuple::new(interpolation_values);
    let instance = Instance::new(
        *vm.heap.native_classes.get("Tuple").unwrap(),
        Some(tuple.into()),
    );
    let instance_value = vm.heap.add_instance(instance);

    Ok(instance_value)
}

/// Get the next element from a template iterator `var next = templateiter.__next__()`.
/// Powers `foreach (var a in template)`
#[allow(clippy::option_if_let_else)]
pub(super) fn template_iter_next_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> VmResult<Value> {
    let mut my_iter = std::mem::take(receiver.as_template_iter_mut(&mut vm.heap));
    let my_template = my_iter.get_template(&vm.heap);

    let max_index = my_template.strings().len() + my_template.interpolations().len();
    let result = loop {
        if my_iter.index >= max_index {
            break Value::StopIteration;
        }

        let entry_index = my_iter.index / 2;
        let is_string = my_iter.index.is_multiple_of(2);
        my_iter.index += 1;

        if is_string {
            let string_id = my_template.strings()[entry_index];
            if !string_id.to_value(&vm.heap).is_empty() {
                break string_id.into();
            }
            // otherwise continue: skip empty string
        } else {
            break my_template.interpolations()[entry_index].into();
        }
    };

    *receiver.as_template_iter_mut(&mut vm.heap) = my_iter;
    Ok(result)
}

pub(super) fn interpolation_value_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> VmResult<Value> {
    let my_interpolation = receiver.as_interpolation(&vm.heap);
    let value = my_interpolation.value();

    Ok(value)
}

pub(super) fn interpolation_expression_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> VmResult<Value> {
    let my_interpolation = receiver.as_interpolation(&vm.heap);
    let expression = my_interpolation.expression().into();

    Ok(expression)
}
