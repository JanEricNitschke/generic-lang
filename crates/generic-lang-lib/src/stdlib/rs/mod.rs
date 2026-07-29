//! Module for registering rust native stdlib modules.

mod builtins;
mod dataclasses;
mod functools;
mod keyword;
mod math;
mod string;

use crate::vm::VM;

pub fn register(vm: &mut VM) {
    math::register(vm);
    functools::register(vm);
    string::register(vm);
    dataclasses::register(vm);
    keyword::register(vm);
    builtins::register(vm);
}
