use super::{Global, VM, errors::VmError};
use crate::{chunk::OpCode, value::Value};

impl VM {
    pub(super) fn set_local(&mut self, op: OpCode) {
        let slot = if op == OpCode::GetLocalLong {
            self.read_24bit_number()
        } else {
            usize::from(self.read_byte())
        };
        *self.stack_get_mut(slot) = *self.peek(0).expect("stack underflow in OP_SET_LOCAL");
    }

    pub(super) fn get_local(&mut self, op: OpCode) {
        let slot = if op == OpCode::GetLocalLong {
            self.read_24bit_number()
        } else {
            usize::from(self.read_byte())
        };
        self.stack_push(*self.stack_get(slot));
    }

    pub(super) fn get_global(&mut self, op: OpCode) -> VmError {
        let constant_index = self.read_constant_index(op == OpCode::GetGlobalLong);
        let constant_value = self.read_constant_value(constant_index);
        match &constant_value {
            Value::String(name) => {
                let maybe_value = self
                    .defining_module()
                    .to_value(&self.heap)
                    .globals
                    .get(name)
                    .map(|global| global.value);
                if let Some(value) = maybe_value {
                    self.stack_push(value);
                } else {
                    let maybe_builtin = self.builtins.get(name).map(|global| global.value);
                    if let Some(value) = maybe_builtin {
                        self.stack_push(value);
                    } else {
                        let message = format!("Undefined variable '{}'.", self.heap.strings[*name]);
                        return self.throw_name_error(&message);
                    }
                }
            }

            x => panic!("Internal error: non-string operand to {op:?}: {x:?}"),
        }
        Ok(())
    }

    pub(super) fn set_global(&mut self, op: OpCode) -> VmError {
        let constant_index = self.read_constant_index(op == OpCode::SetGlobalLong);
        let constant_value = self.read_constant_value(constant_index);
        let name = match &constant_value {
            Value::String(name) => *name,
            x => panic!("Internal error: non-string operand to OP_SET_GLOBAL: {x:?}"),
        };
        let stack_top_value = *self
            .stack
            .last()
            .unwrap_or_else(|| panic!("Stack underflow in {op:?}"));
        if let Some(global) = self
            .defining_module()
            .to_value_mut(&mut self.heap)
            .globals
            .get_mut(&name)
        {
            if !global.mutable {
                return self.throw_const_reassignment_error("Cannot reassign const variable.");
            }
            global.value = stack_top_value;
        } else {
            let maybe_builtin = self.builtins.get_mut(&name);
            if let Some(global) = maybe_builtin {
                if !global.mutable {
                    return self.throw_const_reassignment_error("Cannot reassign const variable.");
                }
                global.value = stack_top_value;
            } else {
                let message = format!("Undefined variable '{}'.", name.to_value(&self.heap));
                return self.throw_name_error(&message);
            }
        }

        Ok(())
    }

    pub(super) fn define_global(&mut self, op: OpCode) {
        let constant = self.read_constant(op == OpCode::DefineGlobalLong);
        match &constant {
            Value::String(name) => {
                let name = *name;
                let stack_top_value = *self
                    .stack
                    .last()
                    .unwrap_or_else(|| panic!("stack underflow in {op:?}"));
                self.globals().insert(
                    name,
                    Global {
                        value: stack_top_value,
                        mutable: op != OpCode::DefineGlobalConst
                            && op != OpCode::DefineGlobalConstLong,
                    },
                );
                self.stack.pop();
            }
            x => panic!("Internal error: non-string operand to {op:?}: {x:?}"),
        }
    }
}
