use super::{InterpretResult, VM};
use crate::chunk::CodeOffset;
use crate::value::{Dict, List, Set, Tuple};
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
    pub(crate) fn build_set(&mut self) -> Option<InterpretResult> {
        let mut set = Set::default();

        let arg_count = self.read_byte();
        for index in (0..arg_count).rev() {
            let value = self.peek(usize::from(index)).unwrap();
            if !value.is_hasheable() {
                runtime_error!(
                    self,
                    "Value `{}` is not hashable when this is required for items in a set.",
                    value.to_string(&self.heap)
                );
                return Some(InterpretResult::RuntimeError);
            }
            set.add(*value, &self.heap);
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
        None
    }

    // Build a dict. The number of key-value-pairs is the operand.
    // Items are on the stack in order from left to right
    // (... --- key1 --- value1 --- key2 --- value2 --- ... --- keyN --- valueN)
    pub(crate) fn build_dict(&mut self) -> Option<InterpretResult> {
        let mut dict = Dict::default();
        // Number of key, value pairs.
        let arg_count = self.read_byte();
        for index in (0..arg_count).rev() {
            let key = self.peek(usize::from(2 * index + 1)).unwrap();
            if !key.is_hasheable() {
                runtime_error!(
                    self,
                    "Key `{}` is not hashable when this is required for items in a dict.",
                    key.to_string(&self.heap)
                );
                return Some(InterpretResult::RuntimeError);
            }
            let value = self.peek(usize::from(2 * index)).unwrap();
            dict.add(*key, *value, &self.heap);
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
        None
    }

    /// Build an f-string by concatenating parts on the stack.
    /// The number of parts is the operand.
    /// Parts are on the stack as strings in order from left to right.
    pub(crate) fn build_fstring(&mut self) {
        let part_count = self.read_byte();

        if part_count == 0 {
            // Empty f-string
            let empty_string_id = self.heap.string_id(&String::new());
            self.stack_push_value(Value::String(empty_string_id));
            return;
        }

        // Collect all parts from the stack (they are already strings)
        let mut parts = Vec::new();
        for index in (0..part_count).rev() {
            let part_value = *self
                .peek(usize::from(index))
                .expect("Stack underflow in BuildFString");
            match part_value {
                Value::String(string_id) => {
                    let part_str = string_id.to_value(&self.heap);
                    parts.push(part_str.clone());
                }
                _ => {
                    // This should not happen as we ensure all parts are strings
                    runtime_error!(
                        self,
                        "Invalid f-string part type: {}",
                        part_value.to_string(&self.heap)
                    );
                    return;
                }
            }
        }

        // Pop all parts from stack
        self.stack
            .truncate(self.stack.len() - usize::from(part_count));

        // Concatenate parts in correct order
        let result = parts.join("");

        // Push the result string
        let result_id = self.heap.string_id(&result);
        self.stack_push_value(Value::String(result_id));
    }
}
