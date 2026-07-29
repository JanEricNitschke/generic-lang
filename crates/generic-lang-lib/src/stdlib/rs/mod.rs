//! Module for registering rust native stdlib modules.

mod builtins;
mod dataclasses;
mod functools;
mod json;
mod keyword;
mod math;
mod os;
mod pathlib;
mod requests;
mod string;
mod time;

use crate::vm::VM;

pub fn register(vm: &mut VM) {
    math::register(vm);
    os::register(vm);
    functools::register(vm);
    json::register(vm);
    string::register(vm);
    requests::register(vm);
    pathlib::register(vm);
    dataclasses::register(vm);
    keyword::register(vm);
    builtins::register(vm);
    time::register(vm);
}
