//! Module for registering rust native stdlib modules.

mod dataclasses;
mod functools;
mod keyword;
mod math;

use crate::vm::VM;

pub fn register(vm: &mut VM) {
    math::register(vm);
    functools::register(vm);
    dataclasses::register(vm);
    keyword::register(vm);
}
