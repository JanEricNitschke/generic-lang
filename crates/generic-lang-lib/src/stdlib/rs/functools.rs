//! Native half of the `functools` stdlib module, registered as
//! `_functools`: the `reduce` function and the `partial` class. The
//! generic half (`functools.gen`) re-exports them alongside the pure
//! generic parts of the module.

use crate::natives::VARIADIC_0_PLUS;
use crate::value::{ModuleContents, ModuleExport, Value};
use crate::vm::ExceptionKind::TypeError;
use crate::vm::VM;
use crate::vm::errors::{VmErrorKind, VmResult};

/// Fold an iterable into a single value with a callable taking
/// `(accumulator, item)`. `reduce(function, iterable)` starts from the
/// first item and throws on an empty iterable;
/// `reduce(function, iterable, initial)` starts from `initial`.
fn reduce_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let function = args[0];
    let Some(items) = vm.collect_items_from_iterable(args[1])? else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'reduce' expected an iterable, got: {}",
                    args[1].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };

    let (initial, rest) = if let Some(initial) = args.get(2) {
        (*initial, items.as_slice())
    } else if let Some((first, rest)) = items.split_first() {
        (*first, rest)
    } else {
        return Err(vm
            .throw(
                TypeError,
                "'reduce' of empty iterable with no initial value",
            )
            .unwrap_err());
    };

    // The accumulator and the remaining items are rooted on the VM stack
    // across the re-entrant calls; the accumulator lives in the slot at
    // `base` so each iteration's result stays reachable.
    let base = vm.stack.len();
    vm.stack.push(initial);
    vm.stack.extend_from_slice(rest);
    let end = vm.stack.len();

    let outcome = run_reduce(vm, function, base, end);
    match outcome {
        Ok(()) => {
            let result = vm.stack[base];
            vm.stack.truncate(base);
            Ok(result)
        }
        Err(error @ VmErrorKind::Exception(_)) => {
            let exception = vm.stack.pop().expect("pending exception on stack top");
            vm.stack.truncate(base);
            vm.stack.push(exception);
            Err(error)
        }
        Err(error) => Err(error),
    }
}

/// The fold loop of [`reduce_native`]: combine the accumulator at
/// `stack[base]` with each item at `stack[base + 1..end]` in order,
/// writing each result back into the accumulator slot.
fn run_reduce(vm: &mut VM, function: Value, base: usize, end: usize) -> VmResult<()> {
    for index in (base + 1)..end {
        vm.stack.push(function);
        let accumulator = vm.stack[base];
        vm.stack.push(accumulator);
        let item = vm.stack[index];
        vm.stack.push(item);
        vm.call_value_and_run(2)?;
        let result = vm.stack.pop().expect("call left no result on the stack");
        vm.stack[base] = result;
    }
    Ok(())
}

/// `partial(callable, bound...)`: store the callable and the leading
/// arguments on the instance's backing.
fn partial_init_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let Some((function, bound)) = args.split_first() else {
        return Err(vm
            .throw(TypeError, "'partial' expected a callable to bind.")
            .unwrap_err());
    };
    let backing = receiver.as_partial_mut(&mut vm.heap);
    backing.func = *function;
    backing.args = bound.to_vec();
    Ok(*receiver)
}

/// Call the wrapped callable with the bound arguments followed by this
/// call's own arguments.
fn partial_call_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let partial = receiver.as_partial(&vm.heap);
    let function = partial.func;
    let bound = partial.args.clone();

    let Ok(total) = u8::try_from(bound.len() + args.len()) else {
        return Err(vm
            .throw(TypeError, "'partial' call exceeds 255 arguments.")
            .unwrap_err());
    };

    vm.stack.push(function);
    vm.stack.extend(bound);
    vm.stack.extend_from_slice(args);
    vm.call_value_and_run(total)?;
    Ok(vm.stack.pop().expect("call left no result on the stack"))
}

/// `str(partial_instance)`: the wrapped callable and the bound arguments.
#[allow(clippy::unnecessary_wraps)]
fn partial_str_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let rendered = receiver.as_partial(&vm.heap).to_string(&vm.heap, 0);
    Ok(Value::String(vm.heap.string_id(&rendered)))
}

/// Define the `partial` native class and register the `_functools`
/// module. The class is not added to the builtins; it is only reachable
/// through the module (and its `functools.gen` re-export).
pub(super) fn register(vm: &mut VM) {
    vm.define_native_class(&"partial", false);
    vm.define_native_method(
        &"partial",
        &"__init__",
        &VARIADIC_0_PLUS,
        partial_init_native,
    );
    vm.define_native_method(
        &"partial",
        &"__call__",
        &VARIADIC_0_PLUS,
        partial_call_native,
    );
    vm.define_native_method(&"partial", &"__str__", &[0], partial_str_native);
    vm.register_stdlib_module(&"_functools", module());
}

/// Export all the contents of the module with the
/// name they are to be accessed with from generic; functions
/// additionally carry their supported arities.
fn module() -> ModuleContents {
    vec![
        ModuleExport::Function {
            name: "reduce",
            arity: &[2, 3],
            fun: reduce_native,
        },
        ModuleExport::Class { name: "partial" },
    ]
}
