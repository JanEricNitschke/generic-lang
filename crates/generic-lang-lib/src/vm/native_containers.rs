use super::{VM, errors::VmResult};
use crate::types::RangeType;
use crate::value::{
    Dict, Generator, GenericInt, Instance, Interpolation, List, Number, Range, Set, Template,
    Tuple, Value,
};
use crate::vm::ExceptionKind::TypeError;
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
                return self.throw(TypeError, &message);
            };

        let instance = Instance::new(
            *self.heap.native_classes.get("Range").unwrap(),
            Some(range.into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push(instance_value);
        Ok(None)
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
        self.stack_push(instance_value);
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
        let tuple = Tuple::new(items);

        // Pop all items from stack at once
        self.stack
            .truncate(self.stack.len() - usize::from(arg_count));
        let instance = Instance::new(
            *self.heap.native_classes.get("Tuple").unwrap(),
            Some(tuple.into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push(instance_value);
    }

    /// Build a set. The number of items is the operand.
    ///
    ///  Items are on the stack in order from left to right
    /// (... --- item1 --- item2 --- ... --- itemN)
    pub(crate) fn build_set(&mut self) -> VmResult {
        let arg_count = self.read_byte();

        // Allocate the empty set first and root it on the stack (above the
        // items) so it and its contents stay visible to the GC while the
        // adds run `__hash__`/`__eq__`.
        let instance = Instance::new(
            *self.heap.native_classes.get("Set").unwrap(),
            Some(Set::default().into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push(instance_value);

        for index in (0..arg_count).rev() {
            // +1: the set itself sits on top of the items.
            let value = *self.peek(usize::from(index) + 1).unwrap();
            Set::add(self, &instance_value, value)?;
        }
        // Pop the set and all items at once, then push the set back
        self.stack
            .truncate(self.stack.len() - usize::from(arg_count) - 1);
        self.stack_push(instance_value);
        Ok(None)
    }

    /// Build a dict. The number of key-value-pairs is the operand.
    ///
    ///  Items are on the stack in order from left to right
    /// (... --- key1 --- value1 --- key2 --- value2 --- ... --- keyN --- valueN)
    pub(crate) fn build_dict(&mut self) -> VmResult {
        // Number of key, value pairs.
        let arg_count = self.read_byte();

        // Allocate the empty dict first and root it on the stack (above the
        // items) so it and its contents stay visible to the GC while the
        // adds run `__hash__`/`__eq__`.
        let instance = Instance::new(
            *self.heap.native_classes.get("Dict").unwrap(),
            Some(Dict::default().into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push(instance_value);

        for index in (0..arg_count).rev() {
            // +1: the dict itself sits on top of the key-value pairs.
            let key = *self.peek(usize::from(2 * index + 1) + 1).unwrap();
            let value = *self.peek(usize::from(2 * index) + 1).unwrap();
            Dict::add(self, &instance_value, key, value)?;
        }
        // Pop the dict and all key-value pairs at once, then push the dict back
        self.stack
            .truncate(self.stack.len() - usize::from(arg_count) * 2 - 1);
        self.stack_push(instance_value);
        Ok(None)
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
        self.stack_push(value);
        Ok(None)
    }

    /// Build a t-string interpolation.
    ///
    /// Items are on the stack in order from left to right
    /// (... --- value --- expression)
    pub(crate) fn build_interpolation(&mut self) {
        let expression = *self
            .stack
            .pop()
            .expect("Stack underflow in OP_BUILD_INTERPOLATION")
            .as_string();
        let value = self
            .stack
            .pop()
            .expect("Stack underflow in OP_BUILD_INTERPOLATION");
        let interpolation = Interpolation::new(value, expression);

        let instance = Instance::new(
            *self.heap.native_classes.get("Interpolation").unwrap(),
            Some(interpolation.into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push(instance_value);
    }

    /// Build a t-string template.
    ///
    /// Items are on the stack in order from left to right
    /// (... --- tuple of interpolations --- tuple of strings)
    /// So for f"Hi ${1+1}, i'm ${name}"
    /// We would have (... --- (Interpolation(value=2, expression=1+1), `Interpolation(value=VALUE_OF_NAME)`, expression=name) --- ("Hi ", ", i'm ", ""))""
    pub(crate) fn build_template(&mut self) {
        let strings = self
            .stack
            .pop()
            .expect("Stack underflow in OP_BUILD_TEMPLATE")
            .as_tuple(&self.heap)
            .items()
            .iter()
            .map(|value| *value.as_string())
            .collect();
        let interpolations = self
            .stack
            .pop()
            .expect("Stack underflow in OP_BUILD_TEMPLATE")
            .as_tuple(&self.heap)
            .items()
            .iter()
            .map(|value| *value.as_instance())
            .collect();

        let template = Template::new(interpolations, strings);
        let instance = Instance::new(
            *self.heap.native_classes.get("Template").unwrap(),
            Some(template.into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push(instance_value);
    }

    /// Build a generator from the current call stack frame.
    ///
    /// Also store the corresponding stack portions and exception handlers.
    pub(crate) fn create_generator(&mut self) {
        let generator_frame = self
            .callstack
            .pop(&self.heap)
            .expect("Call stack underflow in OP_RETURN");

        // grab stack slice belonging to this frame
        let generator_stack: Vec<Value> = self.stack.drain(generator_frame.stack_base..).collect();

        // grab exception handlers belonging to this frame
        let callstack_len = self.callstack.len();
        let generator_handlers = self
            .exception_handlers
            .drain(
                self.exception_handlers
                    .iter()
                    .position(|h| h.frames_to_keep > callstack_len)
                    .unwrap_or(self.exception_handlers.len())..,
            )
            .collect::<Vec<_>>();

        let generator = Generator::new(generator_frame, generator_handlers, generator_stack);

        let instance = Instance::new(
            *self.heap.native_classes.get("Generator").unwrap(),
            Some(generator.into()),
        );
        let instance_value = self.heap.add_instance(instance);
        self.stack_push(instance_value);
    }
}
