//! Native half of the `functools` stdlib module, registered as
//! `_functools`: the `partial` class. The generic half
//! (`functools.gen`) re-exports it alongside the pure generic parts of
//! the module (`reduce`, `cmp_to_key`).

use crate::natives::VARIADIC_0_PLUS;
use crate::value::{ModuleContents, ModuleExport, Value};
use crate::vm::ExceptionKind::TypeError;
use crate::vm::VM;
use crate::vm::errors::VmResult;

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

/// `str(partial_instance)`: the wrapped callable and the bound arguments,
/// each rendered the way `str` renders them (honoring `__str__`).
fn partial_str_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let mut string = String::from("partial(");
    let function = receiver.as_partial(&vm.heap).func;
    string.push_str(vm.value_to_string(&function)?.to_value(&vm.heap));

    // Re-fetch by index each step: `value_to_string` may re-enter.
    let mut index = 0;
    while index < receiver.as_partial(&vm.heap).args.len() {
        string.push_str(", ");
        let argument = receiver.as_partial(&vm.heap).args[index];
        string.push_str(vm.value_to_string(&argument)?.to_value(&vm.heap));
        index += 1;
    }

    string.push(')');
    Ok(vm.heap.string_id(&string).into())
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
    vec![ModuleExport::Class { name: "partial" }]
}
