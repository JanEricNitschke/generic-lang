use super::{InterpretResult, VM};
use crate::chunk::CodeOffset;
use crate::value::{GenericInt, Instance, Number, Range, Value};

impl VM {
    pub fn build_range(&mut self, was_exclusive: bool) -> Option<InterpretResult> {
        let end = self.stack.pop().expect("Stack underflow in OP_BUILD_RANGE");
        let start = self.stack.pop().expect("Stack underflow in OP_BUILD_RANGE");

        let range =
            if let (Value::Number(Number::Integer(start)), Value::Number(Number::Integer(end))) =
                (start, end)
            {
                if was_exclusive {
                    Range::new(start, end)
                } else {
                    // Internally ranges are always exclusive.
                    // So 1..=10 -> 1..<11
                    Range::new(start, end.add(GenericInt::Small(1), &mut self.heap))
                }
            } else {
                runtime_error!(
                    self,
                    "Invalid operands ({}, {}) for range construction.",
                    start.to_string(&self.heap),
                    end.to_string(&self.heap)
                );
                return Some(InterpretResult::RuntimeError);
            };

        let instance = Instance::new(
            *self.heap.native_classes.get("Range").unwrap(),
            Some(range.into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push_value(instance_value);
        None
    }
}
