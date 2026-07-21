//! Native methods for generators
use crate::vm::ExceptionKind::ValueError;
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

// GC-safety of the take/restore pattern in send/next/raise below: the
// generator is taken out of the heap (`take_generator`) and thus hidden
// from the GC while it runs. This is sound because `Generator::resume_with`
// moves everything GC-relevant it holds (saved value stack, callframe) onto
// the VM stack and callstack — which are GC roots — before any bytecode
// executes, and no other code between take and restore re-enters the
// interpreter (GC runs exclusively from the instruction dispatch loop).

/// Take the generator out of the heap for a resume, leaving a
/// [`GeneratorState::Executing`] placeholder in its slot. Finding that
/// placeholder here means the generator is resuming itself (directly, or
/// through two generators resuming each other) — throw instead of running
/// the placeholder's empty frame.
fn take_generator(vm: &mut VM, receiver: &Value) -> VmResult<Generator> {
    let generator = receiver.as_generator(&vm.heap);
    if generator.state == GeneratorState::Executing {
        return Err(vm
            .throw(ValueError, "Generator already executing.")
            .unwrap_err());
    }
    let closure_id = generator.closure();
    Ok(std::mem::replace(
        receiver.as_generator_mut(&mut vm.heap),
        Generator::executing_placeholder(closure_id),
    ))
}

pub(super) fn generator_send_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let mut generator = take_generator(vm, receiver)?;
    let result = generator.send(args[0], vm);
    *receiver.as_generator_mut(&mut vm.heap) = generator;
    result
}

pub(super) fn generator_next_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    let mut generator = take_generator(vm, receiver)?;
    let result = generator.next(vm);
    *receiver.as_generator_mut(&mut vm.heap) = generator;
    result
}

pub(super) fn generator_raise_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let mut generator = take_generator(vm, receiver)?;
    let result = generator.raise(args[0], vm);
    *receiver.as_generator_mut(&mut vm.heap) = generator;
    result
}

pub(super) fn generator_close_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    // Closing while executing would only mark the placeholder: the native
    // wrapper that took the generator out writes the real one back over it
    // afterwards, silently undoing the close. Throw like the resumes do.
    if receiver.as_generator(&vm.heap).state == GeneratorState::Executing {
        return Err(vm
            .throw(ValueError, "Generator already executing.")
            .unwrap_err());
    }
    receiver.as_generator_mut(&mut vm.heap).state = GeneratorState::Completed;
    Ok(Value::Nil)
}
