use super::VM;
use crate::{
    heap::StringId,
    value::{GenericInt, Number, Value},
    vm::errors::VmResult,
};
use num_bigint::BigInt;
use rustc_hash::FxHasher;
use std::hash::{Hash, Hasher};
use unicode_normalization::UnicodeNormalization;

impl VM {
    pub fn invoke_method_by_name(
        &mut self,
        values: &[Value],
        method_name: &str,
    ) -> VmResult<Option<Value>> {
        let method_id = self.heap.string_id(&method_name);

        if let Value::Instance(instance) = values[0]
            && let Some(method) = instance
                .to_value(&self.heap)
                .get_field_or_method(method_id, &self.heap)
        {
            self.stack.extend_from_slice(values);
            self.invoke_and_run_function(
                method_id,
                (values.len() - 1).try_into().unwrap(),
                matches!(method, Value::NativeMethod(_)),
            )?;
            Ok(Some(
                self.stack.pop().expect("Stack underflow in len_native"),
            ))
        } else {
            Ok(None)
        }
    }

    pub fn invoke_method_by_name_with_attribute_error(
        &mut self,
        value: Value,
        method_name: &str,
    ) -> VmResult<Value> {
        if let Some(length) = self.invoke_method_by_name(&[value], method_name)? {
            Ok(length)
        } else {
            Err(self
                .throw_attribute_error(&format!("Undefined property '{method_name}'."))
                .unwrap_err())
        }
    }

    /// Convert a value to string, handling instances with __str__ methods.
    /// This function is shared between value constructors, `to_string_native`, and `print_native`.
    pub fn value_to_string(&mut self, value: &Value) -> VmResult<StringId> {
        if let Some(string_value) = self.invoke_method_by_name(&[*value], "__str__")? {
            if let Value::String(string_id) = string_value {
                Ok(string_id)
            } else {
                Err(self
                    .throw_type_error(&format!(
                        "`__str__` must return a string, got {}",
                        string_value.to_string(&self.heap)
                    ))
                    .unwrap_err())
            }
        } else {
            Ok(self.heap.string_id(&value.to_string(&self.heap)))
        }
    }

    pub(crate) fn is_falsey(&mut self, value: Value) -> VmResult<bool> {
        if let Some(bool_result) = self.invoke_method_by_name(&[value], "__bool__")? {
            if let Value::Bool(result) = bool_result {
                Ok(!result)
            } else {
                Err(self
                    .throw_type_error(&format!(
                        "`__bool__` must return a boolean, got: {}",
                        bool_result.to_string(&self.heap)
                    ))
                    .unwrap_err())
            }
        } else {
            Ok(match value {
                Value::Nil | Value::Bool(false) => true,
                Value::Number(n) => n == 0.into(),
                Value::String(id) => (id.to_value(&self.heap)).is_empty(),
                _ => false,
            })
        }
    }

    /// Collects all items from an object that implements the `__iter__` / `__next__` protocol.
    ///
    /// Returns:
    /// - `Ok(Some(Vec<Value>))` if the object is iterable and yielded values.
    /// - `Ok(None)` if the object does not implement `__iter__`.
    /// - `Err(..)` if iteration raised an error inside the VM.
    pub(crate) fn collect_items_from_iterable(
        &mut self,
        value: Value,
    ) -> VmResult<Option<Vec<Value>>> {
        let items_start = self.stack.len();
        let Some(iter) = self.invoke_method_by_name(&[value], "__iter__")? else {
            return Ok(None);
        };

        loop {
            let next = self.invoke_method_by_name_with_attribute_error(iter, "__next__")?;
            if next == Value::StopIteration {
                break;
            }
            // We put them onto the stack so that the GC is aware of them for the next iteration
            // This works because otherwise all used methods here are stack neutral
            self.stack.push(next);
        }

        Ok(Some(self.stack.drain(items_start..).collect()))
    }

    /// Compare two values for equality, with support for custom __eq__ methods.
    /// Optimized for use by hash collections - only pushes to stack when needed.
    pub(crate) fn compare_values(&mut self, left: Value, right: Value) -> VmResult<bool> {
        if let Some(equality_result) = self.invoke_method_by_name(&[left, right], "__eq__")? {
            if let Value::Bool(result) = equality_result {
                Ok(result)
            } else {
                Err(self
                    .throw_type_error(&format!(
                        "`__eq__` must return a boolean, got: {}",
                        equality_result.to_string(&self.heap)
                    ))
                    .unwrap_err())
            }
        } else {
            Ok(left.eq(&right, &self.heap))
        }
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
                s.to_value(&self.heap)
                    .nfc()
                    .collect::<String>()
                    .hash(&mut state);
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
}
