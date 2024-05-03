//! Module for registering rust native stdlib modules.

mod math;

use crate::vm::VM;

pub fn register(vm: &mut VM) {
    vm.register_stdlib_module(&"math", math::module());
}
