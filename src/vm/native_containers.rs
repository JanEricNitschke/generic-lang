use super::{RuntimeError, VM};
use crate::chunk::CodeOffset;
use crate::value::{Dict, List, Set, Tuple};
use crate::value::{GenericInt, Instance, Number, Range, Value};
impl VM {
    pub fn build_range(&mut self, was_exclusive: bool) -> Result<(), RuntimeError> {
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
                return Err(RuntimeError::new());
            };

        let instance = Instance::new(
            *self.heap.native_classes.get("Range").unwrap(),
            Some(range.into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push_value(instance_value);
        Ok(())
    }

    // Build a list. The number of items is the operand.
    // Items are on the stack in order from left to right
    // (... --- item1 --- item2 --- ... --- itemN)
    pub(crate) fn build_list(&mut self) {
        let arg_count = self.read_byte();
        let items: Vec<Value> = (0..arg_count)
            .rev()
            .map(|index| *self.peek(usize::from(index)).unwrap())
            .collect();
        let list = List::new(items);

        // Pop all items from stack at once
        self.stack
            .truncate(self.stack.len() - usize::from(arg_count));
        let instance = Instance::new(
            *self.heap.native_classes.get("List").unwrap(),
            Some(list.into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push_value(instance_value);
    }

    // Build a tuple. The number of items is the operand.
    // Items are on the stack in order from left to right
    // (... --- item1 --- item2 --- ... --- itemN)
    pub(crate) fn build_tuple(&mut self) {
        let arg_count = self.read_byte();
        let items: Vec<Value> = (0..arg_count)
            .rev()
            .map(|index| *self.peek(usize::from(index)).unwrap())
            .collect();
        let list = Tuple::new(items);

        // Pop all items from stack at once
        self.stack
            .truncate(self.stack.len() - usize::from(arg_count));
        let instance = Instance::new(
            *self.heap.native_classes.get("Tuple").unwrap(),
            Some(list.into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push_value(instance_value);
    }

    // Build a set. The number of items is the operand.
    // Items are on the stack in order from left to right
    // (... --- item1 --- item2 --- ... --- itemN)
    pub(crate) fn build_set(&mut self) -> Result<(), RuntimeError> {
        let mut set = Set::default();

        let arg_count = self.read_byte();
        for index in (0..arg_count).rev() {
            let value = *self.peek(usize::from(index)).unwrap();

            if let Err(err) = set.add(value, self) {
                runtime_error!(self, "{}", err);
                return Err(RuntimeError::new());
            }
        }
        // Pop all items from stack at once
        self.stack
            .truncate(self.stack.len() - usize::from(arg_count));

        let instance = Instance::new(
            *self.heap.native_classes.get("Set").unwrap(),
            Some(set.into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push_value(instance_value);
        Ok(())
    }

    // Build a dict. The number of key-value-pairs is the operand.
    // Items are on the stack in order from left to right
    // (... --- key1 --- value1 --- key2 --- value2 --- ... --- keyN --- valueN)
    pub(crate) fn build_dict(&mut self) -> Result<(), RuntimeError> {
        let mut dict = Dict::default();
        // Number of key, value pairs.
        let arg_count = self.read_byte();
        for index in (0..arg_count).rev() {
            let key = *self.peek(usize::from(2 * index + 1)).unwrap();
            let value = *self.peek(usize::from(2 * index)).unwrap();

            if let Err(err) = dict.add(key, value, self) {
                runtime_error!(self, "{}", err);
                return Err(RuntimeError::new());
            }
        }
        // Pop all key-value pairs from stack at once
        self.stack
            .truncate(self.stack.len() - usize::from(arg_count) * 2);
        let instance = Instance::new(
            *self.heap.native_classes.get("Dict").unwrap(),
            Some(dict.into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push_value(instance_value);
        Ok(())
    }
}
