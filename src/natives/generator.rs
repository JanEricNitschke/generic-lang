//! Native methods for generators
use crate::{
    value::{Generator, GeneratorState, Value},
    vm::{VM, errors::VmResult},
};

/// Generators are their own iterators.
pub(super) fn generator_iter_native(
    _vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> VmResult<Value> {
    Ok(*receiver)
}

pub(super) fn generator_send_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmResult<Value> {
    let closure_id = receiver.as_generator(&vm.heap).closure();
    let mut generator = std::mem::replace(
        receiver.as_generator_mut(&mut vm.heap),
        Generator::from_closure_id(closure_id),
    );
    let result = generator.send(*args[0], vm);
    *receiver.as_generator_mut(&mut vm.heap) = generator;
    result
}

pub(super) fn generator_next_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> VmResult<Value> {
    let closure_id = receiver.as_generator(&vm.heap).closure();
    let mut generator = std::mem::replace(
        receiver.as_generator_mut(&mut vm.heap),
        Generator::from_closure_id(closure_id),
    );
    let result = generator.next(vm);
    *receiver.as_generator_mut(&mut vm.heap) = generator;
    result
}

pub(super) fn generator_raise_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmResult<Value> {
    let closure_id = receiver.as_generator(&vm.heap).closure();
    let mut generator = std::mem::replace(
        receiver.as_generator_mut(&mut vm.heap),
        Generator::from_closure_id(closure_id),
    );
    let result = generator.raise(*args[0], vm);
    *receiver.as_generator_mut(&mut vm.heap) = generator;
    result
}

pub(super) fn generator_close_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
) -> VmResult<Value> {
    receiver.as_generator_mut(&mut vm.heap).state = GeneratorState::Completed;
    Ok(Value::Nil)
}
