use super::VM;
use crate::value::Value;

impl VM {
    /// Check the top value of the stack.
    pub(crate) fn peek(&self, n: usize) -> Option<&Value> {
        let len = self.stack.len();
        if n >= len {
            None
        } else {
            Some(&self.stack[len - n - 1])
        }
    }

    /// Mutably check the top value of the stack.
    pub(super) fn peek_mut(&mut self, n: usize) -> Option<&mut Value> {
        let len = self.stack.len();
        if n >= len {
            None
        } else {
            Some(&mut self.stack[len - n - 1])
        }
    }

    #[inline]
    pub(super) fn stack_push(&mut self, value_id: Value) {
        self.stack.push(value_id);
        // This check has a pretty big performance overhead; disabled for now
        // TODO find a better way: keep the check and minimize overhead
        /*
        if self.stack.len() > STACK_MAX {
            runtime_error!(self, "Stack overflow");
        }
        */
    }

    #[inline]
    pub(super) fn stack_push_value(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub(super) fn stack_get(&self, slot: usize) -> &Value {
        &self.stack[self.stack_base() + slot]
    }

    pub(super) fn stack_get_mut(&mut self, slot: usize) -> &mut Value {
        let offset = self.stack_base();
        &mut self.stack[offset + slot]
    }

    fn stack_base(&self) -> usize {
        self.callstack.current().stack_base
    }
}
