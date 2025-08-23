use super::VM;
use crate::{
    heap::StringId,
    value::{GenericInt, Number, Value},
    vm::errors::VmResult,
};
use num_bigint::BigInt;
use rustc_hash::FxHasher;
use std::hash::{Hash, Hasher};

impl VM {
    /// Convert a value to string, handling instances with __str__ methods.
    /// This function is shared between value constructors, `to_string_native`, and `print_native`.
    pub fn value_to_string(&mut self, value: &Value) -> VmResult<StringId> {
        let str_id = self.heap.string_id(&"__str__");

        if let Value::Instance(instance) = value
            && let Some(str_method) = instance
                .to_value(&self.heap)
                .get_field_or_method(str_id, &self.heap)
        {
            // Push the value onto the stack temporarily so invoke_and_run_function can access it
            self.stack.push(*value);
            self.invoke_and_run_function(str_id, 0, matches!(str_method, Value::NativeMethod(_)))?;

            let result = self
                .stack
                .pop()
                .expect("Stack underflow in value_to_string");

            if let Value::String(string_id) = result {
                Ok(string_id)
            } else {
                Err(self
                    .throw_type_error(&format!(
                        "`__str__` must return a string, got {}",
                        result.to_string(&self.heap)
                    ))
                    .unwrap_err())
            }
        } else {
            Ok(self.heap.string_id(&value.to_string(&self.heap)))
        }
    }

    pub(crate) fn is_falsey(&mut self, value: Value) -> VmResult<bool> {
        let bool_id = self.heap.string_id(&"__bool__");
        if let Value::Instance(instance) = value
            && let Some(bool_method) = instance
                .to_value(&self.heap)
                .get_field_or_method(bool_id, &self.heap)
        {
            // Value needs to be on top of the stack to invoke this check.
            self.stack_push_value(value);
            self.invoke_and_run_function(
                bool_id,
                0,
                matches!(bool_method, Value::NativeMethod(_)),
            )?;
            let result = self.stack.pop().expect("Stack underflow in IS_FALSEY");
            if let Value::Bool(result) = result {
                Ok(!result)
            } else {
                Err(self
                    .throw_type_error(&format!(
                        "`__bool__` must return a boolean, got: {}",
                        result.to_string(&self.heap)
                    ))
                    .unwrap_err())
            }
        } else {
            Ok(match value {
                Value::Nil | Value::Bool(false) => true,
                Value::Number(n) => n == 0.into(),
                Value::String(id) => (id.to_value(&self.heap)).to_string().is_empty(),
                _ => false,
            })
        }
    }

    pub(crate) fn compute_hash(&mut self, value: Value) -> VmResult<u64> {
        let mut state = FxHasher::default();

        match value {
            // For instances, try __hash__ method first, then fall back to object ID
            Value::Instance(instance_id) => {
                let hash_method_id = self.heap.string_id(&"__hash__");

                if let Some(hash_method) = instance_id
                    .to_value(&self.heap)
                    .get_field_or_method(hash_method_id, &self.heap)
                {
                    // Push the instance onto the stack for method call
                    self.stack.push(value);

                    self.invoke_and_run_function(
                        hash_method_id,
                        0,
                        matches!(hash_method, Value::NativeMethod(_)),
                    )?;

                    let result = self.stack.pop().expect("Stack underflow in compute_hash");
                    return match result {
                        Value::Number(Number::Integer(GenericInt::Small(n))) => {
                            Ok(n.unsigned_abs())
                        }
                        _ => Err(self
                            .throw_type_error(&format!(
                                "__hash__ method must return an integer, got: {}",
                                result.to_string(&self.heap)
                            ))
                            .unwrap_err()),
                    };
                }
                // Fall back to object ID hashing
                instance_id.hash(&mut state);
            }
            // Basic types use their original hashing logic
            Value::Bool(b) => {
                b.hash(&mut state);
            }
            Value::Nil => {
                state.write_u8(0);
            }
            Value::StopIteration => {
                state.write_u8(1);
            }
            Value::Number(n) => {
                match n {
                    Number::Float(f) => {
                        let f = if f == 0.0 { 0.0 } else { f };
                        // If f has no fractional part, we treat it like an integer.
                        if f.fract() == 0.0 {
                            // Convert to an integer if the float has no fractional part
                            #[allow(clippy::cast_possible_truncation)]
                            BigInt::from(f as i64).hash(&mut state);
                        } else {
                            f.to_bits().hash(&mut state); // Otherwise, hash the float as is
                        }
                    }
                    Number::Integer(i) => match i {
                        GenericInt::Small(n) => BigInt::from(n).hash(&mut state),
                        GenericInt::Big(n) => (n.to_value(&self.heap)).hash(&mut state),
                    },
                    Number::Rational(rational) => {
                        rational.hash(&mut state, &self.heap);
                    }
                }
            }
            Value::String(s) => {
                s.hash(&mut state);
            }
            // For other object types, use their object ID as hash
            Value::Function(id) => {
                id.hash(&mut state);
            }
            Value::Closure(id) => {
                id.hash(&mut state);
            }
            Value::Module(id) => {
                id.hash(&mut state);
            }
            Value::Upvalue(id) => {
                id.hash(&mut state);
            }
            Value::NativeFunction(id) => {
                id.hash(&mut state);
            }
            Value::NativeMethod(id) => {
                id.hash(&mut state);
            }
            Value::Class(id) => {
                id.hash(&mut state);
            }
            Value::BoundMethod(id) => {
                id.hash(&mut state);
            }
        }

        Ok(state.finish())
    }

    /// Compare two values for equality, with support for custom __eq__ methods.
    /// If an exception occurs in __eq__, the `handle_exception` and/or `encountered_hard_error`
    /// flags will be set and should be checked by the caller of this..
    /// Callers should check `handling_exception` after calling this method.
    pub(crate) fn compare_values_for_collections(&mut self, left: Value, right: Value) -> bool {
        // If we already have an active exception, don't run the comparison
        if self.handling_exception || self.encountered_hard_exception {
            return false;
        }

        self.compare_values(left, right).unwrap_or_default()
    }

    /// Compare two values for equality, with support for custom __eq__ methods.
    /// Optimized for use by hash collections - only pushes to stack when needed.
    pub(crate) fn compare_values(&mut self, left: Value, right: Value) -> VmResult<bool> {
        let eq_id = self.heap.string_id(&"__eq__");

        // Check if left value is an instance with __eq__ method
        if let Value::Instance(instance) = left
            && let Some(eq_method) = instance
                .to_value(&self.heap)
                .get_field_or_method(eq_id, &self.heap)
        {
            // Only push to stack if we have an instance with __eq__ method
            self.stack_push(left);
            self.stack_push(right);

            self.invoke_and_run_function(eq_id, 1, matches!(eq_method, Value::NativeMethod(_)))?;
            let result = self.stack.pop().expect("Stack underflow in OP_EQUAL");
            return if let Value::Bool(bool) = result {
                Ok(bool)
            } else {
                Err(self
                    .throw_type_error(&format!(
                        "Return value of `__eq__` has to be bool, got {}",
                        result.to_string(&self.heap)
                    ))
                    .unwrap_err())
            };
        }

        // Fall back to heap-level equality
        Ok(left.eq(&right, &self.heap))
    }
}
