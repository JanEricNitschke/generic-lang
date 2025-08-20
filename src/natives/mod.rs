//! Module for defining and registering rust native functions, classes and methods.

#![allow(clippy::unnecessary_wraps)]

mod dict;
mod exception;
mod list;
mod native_functions;
mod range;
mod set;
mod tuple;
mod value_constructors;

use crate::vm::VM;

use crate::natives::list::{
    list_add_native, list_append_native, list_bool_native, list_contains_native, list_get_native,
    list_init_native, list_insert_native, list_iter_native, list_iter_next_native, list_len_native,
    list_pop_native, list_set_native,
};

use crate::natives::range::{
    range_bool_native, range_contains_native, range_init_native, range_iter_native,
    range_iter_next_native, range_len_native,
};

use crate::natives::set::{
    set_bool_native, set_contains_native, set_init_native, set_insert_native, set_len_native,
    set_remove_native,
};

use crate::natives::tuple::{
    tuple_add_native, tuple_bool_native, tuple_contains_native, tuple_get_native,
    tuple_init_native, tuple_iter_native, tuple_iter_next_native, tuple_len_native,
};

use crate::natives::dict::{
    dict_bool_native, dict_contains_native, dict_get_native, dict_init_native, dict_len_native,
    dict_set_native,
};

use crate::natives::exception::{
    exception_init_native, exception_message_native, exception_stack_trace_native,
    exception_str_native,
};

use crate::natives::native_functions::{
    assert_native, clock_native, delattr_native, getattr_native, hasattr_native, input_native,
    is_int_native, isinstance_native, issubclass_native, len_native, print_native, rng_native,
    setattr_native, sleep_native, to_float_native, to_int_native, to_string_native, type_native,
};

use crate::natives::value_constructors::{
    bool_init_native, float_init_native, integer_init_native, rational_init_native,
    string_init_native,
};

/// Static arity arrays for common variadic argument patterns.
/// Arity for "0 or more arguments" (up to 255 for maximum u8 range)
static VARIADIC_0_PLUS: [u8; 256] = [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
    50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73,
    74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97,
    98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
    117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135,
    136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154,
    155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173,
    174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192,
    193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211,
    212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230,
    231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249,
    250, 251, 252, 253, 254, 255,
];

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
    vm.define_native_function(&"isinstance", &[2], isinstance_native);
    vm.define_native_function(&"issubclass", &[2], issubclass_native);

    // The add to builtins is a bit of a workaround for how native instances
    // are instantiated. Currently we either need a way to instantiate them
    // without giving any data or we have to make it so they are not accessible in
    // user land.
    vm.define_native_class(&"List", true);
    vm.define_native_method(&"List", &"__init__", &VARIADIC_0_PLUS, list_init_native);
    vm.define_native_method(&"List", &"append", &[1], list_append_native);
    vm.define_native_method(&"List", &"pop", &[0, 1], list_pop_native);
    vm.define_native_method(&"List", &"insert", &[2], list_insert_native);
    vm.define_native_method(&"List", &"contains", &[1], list_contains_native);
    vm.define_native_method(&"List", &"__iter__", &[0], list_iter_native);
    vm.define_native_method(&"List", &"__getitem__", &[1], list_get_native);
    vm.define_native_method(&"List", &"__setitem__", &[2], list_set_native);
    vm.define_native_method(&"List", &"__add__", &[1], list_add_native);
    vm.define_native_method(&"List", &"__len__", &[0], list_len_native);
    vm.define_native_method(&"List", &"__bool__", &[0], list_bool_native);

    vm.define_native_class(&"ListIterator", false);
    vm.define_native_method(&"ListIterator", &"__next__", &[0], list_iter_next_native);

    vm.define_native_class(&"Tuple", true);
    vm.define_native_method(&"Tuple", &"__init__", &VARIADIC_0_PLUS, tuple_init_native);
    vm.define_native_method(&"Tuple", &"__getitem__", &[1], tuple_get_native);
    vm.define_native_method(&"Tuple", &"__len__", &[0], tuple_len_native);
    vm.define_native_method(&"Tuple", &"__iter__", &[0], tuple_iter_native);
    vm.define_native_method(&"Tuple", &"__add__", &[1], tuple_add_native);
    vm.define_native_method(&"Tuple", &"__bool__", &[0], tuple_bool_native);
    vm.define_native_method(&"Tuple", &"contains", &[1], tuple_contains_native);

    vm.define_native_class(&"TupleIterator", false);
    vm.define_native_method(&"TupleIterator", &"__next__", &[0], tuple_iter_next_native);

    vm.define_native_class(&"Set", true);
    vm.define_native_method(&"Set", &"__init__", &VARIADIC_0_PLUS, set_init_native);
    vm.define_native_method(&"Set", &"contains", &[1], set_contains_native);
    vm.define_native_method(&"Set", &"insert", &[1], set_insert_native);
    vm.define_native_method(&"Set", &"remove", &[1], set_remove_native);
    vm.define_native_method(&"Set", &"__len__", &[0], set_len_native);
    vm.define_native_method(&"Set", &"__bool__", &[0], set_bool_native);

    vm.define_native_class(&"Dict", true);
    vm.define_native_method(&"Dict", &"__init__", &VARIADIC_0_PLUS, dict_init_native);
    vm.define_native_method(&"Dict", &"contains", &[1], dict_contains_native);
    vm.define_native_method(&"Dict", &"__getitem__", &[1], dict_get_native);
    vm.define_native_method(&"Dict", &"__setitem__", &[2], dict_set_native);
    vm.define_native_method(&"Dict", &"__len__", &[0], dict_len_native);
    vm.define_native_method(&"Dict", &"__bool__", &[0], dict_bool_native);

    vm.define_native_class(&"Range", true);
    vm.define_native_method(&"Range", &"__init__", &[2], range_init_native);
    vm.define_native_method(&"Range", &"contains", &[1], range_contains_native);
    vm.define_native_method(&"Range", &"__iter__", &[0], range_iter_native);
    vm.define_native_method(&"Range", &"__len__", &[0], range_len_native);
    vm.define_native_method(&"Range", &"__bool__", &[0], range_bool_native);

    vm.define_native_class(&"RangeIterator", false);
    vm.define_native_method(&"RangeIterator", &"__next__", &[0], range_iter_next_native);

    vm.define_native_class(&"Exception", true);
    vm.define_native_method(&"Exception", &"__init__", &[0, 1], exception_init_native);
    vm.define_native_method(&"Exception", &"message", &[0], exception_message_native);
    vm.define_native_method(
        &"Exception",
        &"stack_trace",
        &[0],
        exception_stack_trace_native,
    );
    vm.define_native_method(&"Exception", &"__str__", &[0], exception_str_native);

    // Value type proxy classes (native classes with special __init__ methods)
    vm.define_native_class(&"Bool", true);
    vm.define_native_method(&"Bool", &"__init__", &[1], bool_init_native);

    vm.define_native_class(&"String", true);
    vm.define_native_method(&"String", &"__init__", &[1], string_init_native);

    vm.define_native_class(&"Integer", true);
    vm.define_native_method(&"Integer", &"__init__", &[1], integer_init_native);

    vm.define_native_class(&"Float", true);
    vm.define_native_method(&"Float", &"__init__", &[1], float_init_native);

    vm.define_native_class(&"Rational", true);
    vm.define_native_method(&"Rational", &"__init__", &[2], rational_init_native);
}
