//! Native methods for generators
use crate::{
    value::{Generator, GeneratorState, Value},
    vm::{VM, errors::VmResult},
};

/// Generators are their own iterators.
pub(super) fn generator_iter_native(
    _vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    Ok(*receiver)
}

// GC-safety of the `mem::replace` pattern in send/next/raise below: the
// generator is taken out of the heap and thus hidden from the GC while it
// runs. This is sound because `Generator::resume_with` moves everything
// GC-relevant it holds (saved value stack, callframe) onto the VM stack and
// callstack — which are GC roots — before any bytecode executes, and no
// other code between take and restore re-enters the interpreter (GC runs
// exclusively from the instruction dispatch loop).
pub(super) fn generator_send_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let closure_id = receiver.as_generator(&vm.heap).closure();
    let mut generator = std::mem::replace(
        receiver.as_generator_mut(&mut vm.heap),
        Generator::from_closure_id(closure_id),
    );
    let result = generator.send(args[0], vm);
    *receiver.as_generator_mut(&mut vm.heap) = generator;
    result
}

pub(super) fn generator_next_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
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
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let closure_id = receiver.as_generator(&vm.heap).closure();
    let mut generator = std::mem::replace(
        receiver.as_generator_mut(&mut vm.heap),
        Generator::from_closure_id(closure_id),
    );
    let result = generator.raise(args[0], vm);
    *receiver.as_generator_mut(&mut vm.heap) = generator;
    result
}

pub(super) fn generator_close_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    receiver.as_generator_mut(&mut vm.heap).state = GeneratorState::Completed;
    Ok(Value::Nil)
}
