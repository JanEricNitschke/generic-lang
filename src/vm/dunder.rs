use super::VM;
use crate::value::Value;

impl VM {
    /// Convert a value to string, handling instances with __str__ methods.
    /// This function is shared between value constructors, `to_string_native`, and `print_native`.
    pub fn value_to_string(&mut self, value: &Value) -> Value {
        let str_id = self.heap.string_id(&"__str__");

        if let Value::Instance(instance) = value
            && let Some(str_method) = instance
                .to_value(&self.heap)
                .get_field_or_method(str_id, &self.heap)
        {
            // Push the value onto the stack temporarily so invoke_and_run_function can access it
            self.stack.push(*value);
            self.invoke_and_run_function(str_id, 0, matches!(str_method, Value::NativeMethod(_)));
            self.stack
                .pop()
                .expect("Stack underflow in value_to_string")
        } else {
            Value::String(self.heap.string_id(&value.to_string(&self.heap)))
        }
    }

    pub(crate) fn is_falsey(&mut self, value: Value) -> bool {
        let bool_id = self.heap.string_id(&"__bool__");
        if let Value::Instance(instance) = value
            && let Some(bool_method) = instance
                .to_value(&self.heap)
                .get_field_or_method(bool_id, &self.heap)
        {
            // Value needs to be on top of the stack to invoke this check.
            self.stack_push_value(value);
            self.invoke_and_run_function(bool_id, 0, matches!(bool_method, Value::NativeMethod(_)));
            let result = self.stack.pop().expect("Stack underflow in IS_FALSEY");
            result == Value::Bool(false)
        } else {
            match value {
                Value::Nil | Value::Bool(false) => true,
                Value::Number(n) => n == 0.into(),
                Value::String(id) => (id.to_value(&self.heap)).to_string().is_empty(),
                _ => false,
            }
        }
    }
}
