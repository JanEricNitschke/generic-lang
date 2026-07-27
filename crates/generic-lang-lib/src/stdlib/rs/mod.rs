//! Module for registering rust native stdlib modules.

mod functools;
mod math;

use crate::vm::VM;

pub fn register(vm: &mut VM) {
    math::register(vm);
    functools::register(vm);
}
