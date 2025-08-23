use super::{VM, errors::VmResult};
use crate::types::RangeType;
use crate::value::{Dict, GenericInt, Instance, List, Number, Range, Set, Tuple, Value};
impl VM {
    pub fn build_range(&mut self, range_type: RangeType) -> VmResult {
        let end = self.stack.pop().expect("Stack underflow in OP_BUILD_RANGE");
        let start = self.stack.pop().expect("Stack underflow in OP_BUILD_RANGE");

        let range =
            if let (Value::Number(Number::Integer(start)), Value::Number(Number::Integer(end))) =
                (start, end)
            {
                match range_type {
                    RangeType::Exclusive => Range::new(start, end),
                    RangeType::Inclusive => {
                        // Internally ranges are always exclusive.
                        // For ascending ranges (start <= end): 1..=10 -> 1..<11 (add 1 to end)
                        // For descending ranges (start > end): 10..=5 -> 10..<4 (subtract 1 from end)
                        let adjusted_end = if start.le(&end, &self.heap) {
                            end.add(GenericInt::Small(1), &mut self.heap)
                        } else {
                            end.sub(GenericInt::Small(1), &mut self.heap)
                        };
                        Range::new(start, adjusted_end)
                    }
                }
            } else {
                let message = format!(
                    "Invalid operands ({}, {}) for range construction.",
                    start.to_string(&self.heap),
                    end.to_string(&self.heap)
                );
                return self.throw_type_error(&message);
            };

        let instance = Instance::new(
            *self.heap.native_classes.get("Range").unwrap(),
            Some(range.into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push_value(instance_value);
        Ok(())
    }

    /// Build a list. The number of items is the operand.
    ///
    /// Items are on the stack in order from left to right
    /// (... --- item1 --- item2 --- ... --- itemN)
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

    /// Build a tuple. The number of items is the operand.
    ///
    /// Items are on the stack in order from left to right
    /// (... --- item1 --- item2 --- ... --- itemN)
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

    /// Build a set. The number of items is the operand.
    ///
    ///  Items are on the stack in order from left to right
    /// (... --- item1 --- item2 --- ... --- itemN)
    pub(crate) fn build_set(&mut self) -> VmResult {
        let mut set = Set::default();

        let arg_count = self.read_byte();
        for index in (0..arg_count).rev() {
            let value = *self.peek(usize::from(index)).unwrap();

            set.add(value, self)?;
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

    /// Build a dict. The number of key-value-pairs is the operand.
    ///
    ///  Items are on the stack in order from left to right
    /// (... --- key1 --- value1 --- key2 --- value2 --- ... --- keyN --- valueN)
    pub(crate) fn build_dict(&mut self) -> VmResult {
        let mut dict = Dict::default();
        // Number of key, value pairs.
        let arg_count = self.read_byte();
        for index in (0..arg_count).rev() {
            let key = *self.peek(usize::from(2 * index + 1)).unwrap();
            let value = *self.peek(usize::from(2 * index)).unwrap();

            dict.add(key, value, self)?;
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

    /// Build a formatted string. The number of parts is the operand.
    ///
    /// Items are on the stack in order from left to right
    /// (... --- item1 --- item2 --- ... --- itemN)
    /// So for f"Hi ${1+1}, i'm ${name}"
    /// We would have (... --- "Hi " --- 2 --- ", i'm " --- <`VALUE_OF_NAME`> --- ""
    pub(crate) fn build_fstring(&mut self) -> VmResult {
        let mut string = String::new();

        let arg_count = self.read_byte();
        for index in (0..arg_count).rev() {
            let value = *self.peek(usize::from(index)).unwrap();
            let string_id = match value {
                Value::String(string_id) => string_id,
                _ => self.value_to_string(&value)?,
            };
            string.push_str(string_id.to_value(&self.heap));
        }
        // Pop all items from stack at once
        self.stack
            .truncate(self.stack.len() - usize::from(arg_count));

        let value = self.heap.string_id(&string).into();
        self.stack_push_value(value);
        Ok(())
    }
}
