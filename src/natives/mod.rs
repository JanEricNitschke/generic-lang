//! Module for defining and registering rust native functions, classes and methods.

#![allow(clippy::unnecessary_wraps)]

mod dict;
mod list;
mod native_functions;
mod set;

use crate::vm::VM;

use crate::natives::list::{
    list_append_native, list_contains_native, list_get_native, list_insert_native,
    list_iter_native, list_iter_next_native, list_pop_native, list_set_native,
};

use crate::natives::set::{set_contains_native, set_insert_native, set_remove_native};

use crate::natives::dict::{dict_get_native, dict_set_native};

use crate::natives::native_functions::{
    assert_native, clock_native, delattr_native, getattr_native, hasattr_native, input_native,
    is_int_native, len_native, print_native, rng_native, setattr_native, sleep_native,
    to_float_native, to_int_native, to_string_native, type_native,
};

pub fn define(vm: &mut VM) {
    vm.define_native_function(&"clock", &[0], clock_native);
    vm.define_native_function(&"assert", &[1], assert_native);
    vm.define_native_function(&"sleep", &[1], sleep_native);
    vm.define_native_function(&"input", &[1], input_native);
    vm.define_native_function(&"float", &[1], to_float_native);
    vm.define_native_function(&"int", &[1], to_int_native);
    vm.define_native_function(&"is_int", &[1], is_int_native);
    vm.define_native_function(&"str", &[1], to_string_native);
    vm.define_native_function(&"type", &[1], type_native);
    vm.define_native_function(&"print", &[1, 2], print_native);
    vm.define_native_function(&"getattr", &[2], getattr_native);
    vm.define_native_function(&"setattr", &[3], setattr_native);
    vm.define_native_function(&"hasattr", &[2], hasattr_native);
    vm.define_native_function(&"delattr", &[2], delattr_native);
    vm.define_native_function(&"rng", &[2], rng_native);
    vm.define_native_function(&"len", &[1], len_native);

    // The add to builtins is a bit of a workaround for how native instances
    // are instantiated. Currently we either need a way to instantiate them
    // without giving any data or we have to make it so they are not accessible in
    // user land.
    vm.define_native_class(&"List", true);
    vm.define_native_method(&"List", &"append", &[1], list_append_native);
    vm.define_native_method(&"List", &"pop", &[0, 1], list_pop_native);
    vm.define_native_method(&"List", &"insert", &[2], list_insert_native);
    vm.define_native_method(&"List", &"contains", &[1], list_contains_native);
    vm.define_native_method(&"List", &"__iter__", &[0], list_iter_native);
    vm.define_native_method(&"List", &"__getitem__", &[1], list_get_native);
    vm.define_native_method(&"List", &"__setitem__", &[2], list_set_native);

    vm.define_native_class(&"ListIterator", false);
    vm.define_native_method(&"ListIterator", &"__next__", &[0, 1], list_iter_next_native);

    vm.define_native_class(&"Set", true);
    vm.define_native_method(&"Set", &"contains", &[1], set_contains_native);
    vm.define_native_method(&"Set", &"insert", &[1], set_insert_native);
    vm.define_native_method(&"Set", &"remove", &[1], set_remove_native);

    vm.define_native_class(&"Dict", true);
    vm.define_native_method(&"Dict", &"__getitem__", &[1], dict_get_native);
    vm.define_native_method(&"Dict", &"__setitem__", &[2], dict_set_native);
}
