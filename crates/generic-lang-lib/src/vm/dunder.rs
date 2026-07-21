use super::VM;
use crate::vm::ExceptionKind::{AttributeError, TypeError};
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
    /// Invoke `method_name` on `values[0]` with the remaining values as
    /// arguments, if the receiver is an instance that has such a field or
    /// method; returns `Ok(None)` otherwise (so callers can fall back).
    ///
    /// On `Err(Exception)` the thrown exception is pending on the stack top
    /// with the VM otherwise restored to the pre-call state: pop it to
    /// handle, or propagate with `?`.
    pub fn invoke_method_by_name(
        &mut self,
        values: &[Value],
        method_name: &str,
    ) -> VmResult<Option<Value>> {
        let method_id = self.heap.string_id(&method_name);

        if let Value::Instance(instance) = values[0]
            && instance
                .to_value(&self.heap)
                .get_field_or_method(method_id, &self.heap)
                .is_some()
        {
            self.stack.extend_from_slice(values);
            self.invoke_and_run_function(method_id, (values.len() - 1).try_into().unwrap())?;
            Ok(Some(
                self.stack
                    .pop()
                    .expect("Stack underflow in invoke_method_by_name"),
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
                .throw(
                    AttributeError,
                    &format!("Undefined property '{method_name}'."),
                )
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
                    .throw(
                        TypeError,
                        &format!(
                            "`__str__` must return a string, got {}",
                            string_value.to_string(&self.heap)
                        ),
                    )
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
                    .throw(
                        TypeError,
                        &format!(
                            "`__bool__` must return a boolean, got: {}",
                            bool_result.to_string(&self.heap)
                        ),
                    )
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

    /// Root `values` on the VM stack, call `f` for each in order, then unroot
    /// them. Keeps the values reachable for the GC while `f` re-enters the
    /// interpreter (e.g. through `__hash__`/`__eq__`).
    ///
    /// `f` must be stack-neutral: it may push and pop while it runs (any
    /// interpreter re-entry does), but must return with the stack at the
    /// depth it was called with — the rooted values are addressed by index.
    ///
    /// On error the values are left on the stack below the pending
    /// exception; the eventual handler resolution truncates them.
    pub(crate) fn for_each_rooted<F>(&mut self, values: Vec<Value>, mut f: F) -> VmResult
    where
        F: FnMut(&mut Self, Value) -> VmResult,
    {
        let start = self.stack.len();
        self.stack.extend(values);
        let end = self.stack.len();
        for index in start..end {
            f(self, self.stack[index])?;
            debug_assert!(
                self.stack.len() == end,
                "for_each_rooted callback must be stack-neutral"
            );
        }
        self.stack.truncate(start);
        Ok(None)
    }

    /// Compare two values for equality, with support for custom __eq__ methods.
    pub(crate) fn compare_values(&mut self, left: Value, right: Value) -> VmResult<bool> {
        if let Some(equality_result) = self.invoke_method_by_name(&[left, right], "__eq__")? {
            if let Value::Bool(result) = equality_result {
                Ok(result)
            } else {
                Err(self
                    .throw(
                        TypeError,
                        &format!(
                            "`__eq__` must return a boolean, got: {}",
                            equality_result.to_string(&self.heap)
                        ),
                    )
                    .unwrap_err())
            }
        } else {
            Ok(left.eq(&right, &self.heap))
        }
    }

    /// Bucket hash for a numeric value — used both when a `Number` is a key
    /// directly and for the integer a user `__hash__` returns, so equal numbers
    /// (`5`, a big-integer-typed `5`, `5.0`, and an instance hashing as `5`)
    /// share a bucket. Integers hash by their `BigInt` value, so the `Small`
    /// and `Big` representations of the same integer agree.
    fn hash_number(&self, number: Number) -> u64 {
        let mut state = FxHasher::default();
        match number {
            Number::Float(f) => {
                let f = if f == 0.0 { 0.0 } else { f };
                if f.fract() == 0.0 {
                    #[allow(clippy::cast_possible_truncation)]
                    BigInt::from(f as i64).hash(&mut state);
                } else {
                    f.to_bits().hash(&mut state);
                }
            }
            Number::Integer(GenericInt::Small(n)) => BigInt::from(n).hash(&mut state),
            Number::Integer(GenericInt::Big(n)) => n.to_value(&self.heap).hash(&mut state),
            Number::Rational(rational) => rational.hash(&mut state, &self.heap),
        }
        state.finish()
    }

    pub(crate) fn compute_hash(&mut self, value: Value) -> VmResult<u64> {
        let mut state = FxHasher::default();

        match value {
            // For instances, try __hash__ method first, then fall back to object ID
            Value::Instance(instance_id) => {
                let hash_method_id = self.heap.string_id(&"__hash__");

                if instance_id
                    .to_value(&self.heap)
                    .get_field_or_method(hash_method_id, &self.heap)
                    .is_some()
                {
                    // Push the instance onto the stack for method call
                    self.stack.push(value);

                    self.invoke_and_run_function(hash_method_id, 0)?;

                    let result = self.stack.pop().expect("Stack underflow in compute_hash");
                    return match result {
                        Value::Number(number @ Number::Integer(_)) => Ok(self.hash_number(number)),
                        _ => Err(self
                            .throw(
                                TypeError,
                                &format!(
                                    "__hash__ method must return an integer, got: {}",
                                    result.to_string(&self.heap)
                                ),
                            )
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
            Value::Number(number) => return Ok(self.hash_number(number)),
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
