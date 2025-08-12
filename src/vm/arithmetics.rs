use super::{InterpretResult, VM};
use crate::{chunk::CodeOffset, value::Value};

#[derive(PartialEq, Eq)]
pub(super) enum BinaryOpResult {
    Success,
    InvalidOperands,
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
                                    if $self.create_exception_instance("ValueError", &error) {
                                        let exception = $self.stack.pop().expect("Exception instance should be on stack");
                                        if let Some(result) = $self.unwind(exception) {
                                            return result;
                                        }
                                    } else {
                                        runtime_error!($self, "Failed to create ValueError: {}", error);
                                        return InterpretResult::RuntimeError;
                                    }
                                    BinaryOpResult::Success // Exception was thrown, continue execution
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
            let error_msg = format!(
                "Operands must be {}. Got: [{}]",
                if $int_only { "integers" } else { "numbers" },
                $self.stack[slice_start..]
                    .iter()
                    .map(|v| format!("{}", v.to_string(&$self.heap)))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            if $self.create_exception_instance("TypeError", &error_msg) {
                let exception = $self.stack.pop().expect("Exception instance should be on stack");
                if let Some(result) = $self.unwind(exception) {
                    return result;
                }
            } else {
                runtime_error!($self, "Failed to create TypeError: {}", error_msg);
                return InterpretResult::RuntimeError;
            }
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
            let error_msg = format!(
                "Operands must be two numbers or two strings. Got: [{}]",
                self.stack[slice_start..]
                    .iter()
                    .map(|v| v.to_string(&self.heap))
                    .collect::<Vec<_>>()
                    .join(", ")
            );

            if self.create_exception_instance("TypeError", &error_msg) {
                let exception = self.stack.pop().expect("Exception instance should be on stack");
                return self.unwind(exception);
            } else {
                runtime_error!(self, "Failed to create TypeError: {}", error_msg);
                return Some(InterpretResult::RuntimeError);
            }
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
            if self.create_exception_instance("TypeError", "Operand must be a number.") {
                let exception = self.stack.pop().expect("Exception instance should be on stack");
                return self.unwind(exception);
            } else {
                runtime_error!(self, "Failed to create TypeError: Operand must be a number.");
                return Some(InterpretResult::RuntimeError);
            }
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
}
