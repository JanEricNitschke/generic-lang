use super::{InterpretResult, VM};
use crate::chunk::CodeOffset;
use crate::value::{GenericInt, GenericRational, Number, Value};
use num_bigint::BigInt;
use rustc_hash::FxHasher;
use std::hash::{Hash, Hasher};

#[derive(PartialEq, Eq)]
pub(super) enum BinaryOpResult {
    Success,
    InvalidOperands,
    OperationError,
}

pub(super) trait IntoResultValue {
    fn into_result_value(self) -> Result<Value, String>;
}

impl<T> IntoResultValue for T
where
    T: Into<Value>,
{
    fn into_result_value(self) -> Result<Value, String> {
        Ok(self.into())
    }
}

impl<T> IntoResultValue for Result<T, String>
where
    T: Into<Value>,
{
    fn into_result_value(self) -> Result<Value, String> {
        self.map(Into::into)
    }
}

macro_rules! binary_op_invoke {
    ($self:ident, $a:ident, $b:ident, $method:ident, mut_heap) => {
        $a.$method(*$b, &mut $self.heap)
    };
    ($self:ident, $a:ident, $b:ident, $method:ident, non_mut_heap) => {
        $a.$method($b, &$self.heap)
    };
}

/// Handle binary operations between numbers.
#[macro_export]
macro_rules! binary_op {
    ($self:ident, $method:ident, $gen_method:tt, $int_only:tt, $heap_toggle:ident) => {{
        let slice_start = $self.stack.len() - 2;

        let gen_method_id = $self.heap.string_id(&$gen_method);

        let status = match &$self.stack[slice_start..] {
            [left, right] => {
                match (&left, &right) {
                    (Value::Number(a), Value::Number(b)) => {
                        if $int_only
                            & (!matches!(a, Number::Integer(_)) | !matches!(b, Number::Integer(_)))
                        {
                            BinaryOpResult::InvalidOperands
                        } else {
                            match binary_op_invoke!($self, a, b, $method, $heap_toggle)
                                .into_result_value()
                            {
                                Ok(value) => {
                                    $self.stack.pop();
                                    $self.stack.pop();
                                    $self.stack_push_value(value);
                                    BinaryOpResult::Success
                                }
                                Err(error) => {
                                    runtime_error!($self, "{error}");
                                    BinaryOpResult::OperationError
                                }
                            }
                        }
                    }
                    (Value::Instance(instance_id), _)
                        if instance_id
                            .to_value(&$self.heap)
                            .has_field_or_method(gen_method_id, &$self.heap) =>
                    {
                        if !$self.invoke(gen_method_id, 1) {
                            return InterpretResult::RuntimeError;
                        }
                        // If the invoke succeeds, decide what to return —
                        // keeping same flow as original code
                        BinaryOpResult::Success
                    }
                    _ => BinaryOpResult::InvalidOperands,
                }
            }
            _ => BinaryOpResult::InvalidOperands,
        };

        if status == BinaryOpResult::InvalidOperands {
            runtime_error!(
                $self,
                "Operands must be {}. Got: [{}]",
                if $int_only { "integers" } else { "numbers" },
                $self.stack[slice_start..]
                    .iter()
                    .map(|v| format!("{}", v.to_string(&$self.heap)))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }
        if status != BinaryOpResult::Success {
            return InterpretResult::RuntimeError;
        }
    }};
}

impl VM {
    pub(super) fn add(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        let gen_method_id = self.heap.string_id(&"__add__");
        let ok = match &self.stack[slice_start..] {
            [left, right] => match (&left, &right) {
                (Value::Number(a), Value::Number(b)) => {
                    let value = (a.add(*b, &mut self.heap)).into();
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(value);
                    true
                }
                (Value::String(a), Value::String(b)) => {
                    // This could be optimized by allowing mutations via the heap
                    let new_string = format!("{}{}", self.heap.strings[*a], self.heap.strings[*b]);
                    let new_string_id = self.heap.string_id(&new_string);
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(new_string_id.into());
                    true
                }
                (Value::Instance(instance_id), _)
                    if instance_id
                        .to_value(&self.heap)
                        .has_field_or_method(gen_method_id, &self.heap) =>
                {
                    if !self.invoke(gen_method_id, 1) {
                        return Some(InterpretResult::RuntimeError);
                    }
                    // If the invoke succeeds, decide what to return —
                    // keeping same flow as original code
                    true
                }
                _ => false,
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be two numbers or two strings. Got: [{}]",
                self.stack[slice_start..]
                    .iter()
                    .map(|v| v.to_string(&self.heap))
                    .collect::<Vec<_>>()
                    .join(", ")
            );

            return Some(InterpretResult::RuntimeError);
        }
        None
    }

    /// Negate the top value on the stack.
    ///
    /// # Panics
    ///
    /// If the stack is empty. This is an internal error and should never happen.
    pub(super) fn negate(&mut self) -> Option<InterpretResult> {
        let value_id = *self.peek(0).expect("stack underflow in OP_NEGATE");
        let value = &value_id;
        if let Value::Number(n) = value {
            self.stack.pop();
            let negated = n.neg(&mut self.heap);
            self.stack_push_value(negated.into());
        } else {
            runtime_error!(self, "Operand must be a number.");
            return Some(InterpretResult::RuntimeError);
        }
        None
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
            matches!(value, Value::Nil | Value::Bool(false))
        }
    }

    pub(crate) fn compute_hash(&mut self, value: Value) -> Result<u64, String> {
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

                    let invoke_result = self.invoke_and_run_function(
                        hash_method_id,
                        0,
                        matches!(hash_method, Value::NativeMethod(_)),
                    );

                    if invoke_result != InterpretResult::Ok {
                        return Err("__hash__ method failed".to_string());
                    }

                    let result = self.stack.pop().expect("Stack underflow in compute_hash");
                    return match result {
                        Value::Number(Number::Integer(GenericInt::Small(n))) => {
                            // Convert to u64, using absolute value for consistent hashing
                            #[allow(clippy::cast_sign_loss)]
                            let hash_val = n.wrapping_abs() as u64;
                            Ok(hash_val)
                        }
                        _ => Err(format!(
                            "__hash__ method must return an integer, got: {}",
                            result.to_string(&self.heap)
                        )),
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

    pub(crate) fn build_rational(&mut self) -> Option<InterpretResult> {
        let denominator = self
            .stack
            .pop()
            .expect("Stack underflow in OP_BUILD_RATIONAL");
        let numerator = self
            .stack
            .pop()
            .expect("Stack underflow in OP_BUILD_RATIONAL");
        match (numerator, denominator) {
            (
                Value::Number(Number::Integer(numerator)),
                Value::Number(Number::Integer(denominator)),
            ) if !denominator.is_zero(&self.heap) => {
                let rational = GenericRational::new(numerator, denominator, &mut self.heap)
                    .expect("Failed to create rational");
                self.stack_push_value(Value::Number(Number::Rational(rational)));
            }
            _ => {
                runtime_error!(
                    self,
                    "Invalid operands ({}, {}) for rational construction.",
                    numerator.to_string(&self.heap),
                    denominator.to_string(&self.heap)
                );
                return Some(InterpretResult::RuntimeError);
            }
        }
        None
    }
}
