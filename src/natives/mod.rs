#![allow(clippy::unnecessary_wraps)]

mod list;
mod native_functions;

use crate::vm::VM;

use crate::natives::list::*;
use crate::natives::native_functions::*;

pub fn define_natives(vm: &mut VM) {
    vm.define_native_function(&"clock", &[0], clock_native);
    vm.define_native_function(&"assert", &[1], assert_native);
    vm.define_native_function(&"sleep", &[1], sleep_native);
    vm.define_native_function(&"sqrt", &[1], sqrt_native);
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

    vm.define_native_class(&"List");
    vm.define_native_method(&"List", &"append", &[1], append_native);
    vm.define_native_method(&"List", &*vm.init_string(), &[0], init_list_native);
    vm.define_native_method(&"List", &"pop", &[0, 1], pop_native);
    vm.define_native_method(&"List", &"insert", &[2], insert_native);
    vm.define_native_method(&"List", &"contains", &[1], contains_native);
}
