//! Shared error constructors for native stdlib modules.

use crate::value::Value;
use crate::vm::ExceptionKind::{TypeError, ValueError};
use crate::vm::VM;
use crate::vm::errors::VmErrorKind;

pub(super) fn type_error(vm: &mut VM, what: &str, expected: &str, value: Value) -> VmErrorKind {
    vm.throw(
        TypeError,
        &format!(
            "'{what}' expects {expected}, got: {}",
            value.to_string(&vm.heap)
        ),
    )
    .unwrap_err()
}

pub(super) fn value_error(vm: &mut VM, message: &str) -> VmErrorKind {
    vm.throw(ValueError, message).unwrap_err()
}
