use super::{InterpretResult, VM};
use crate::chunk::CodeOffset;
use crate::value::{Value, Number};

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
    ($self:ident, $method:ident, $int_only:tt, $heap_toggle:ident) => {{
        let slice_start = $self.stack.len() - 2;

        let status = match &$self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
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
                } else {
                    BinaryOpResult::InvalidOperands
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
        
        // First try __add__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(_instance) = left {
                let add_method_name = self.heap.string_id(&"__add__");
                
                // Use the invoke method to call __add__ on the instance
                // Stack is currently: [..., left, right]
                // invoke expects: [..., instance, arg1, arg2, ...] where instance is at position -arg_count-1
                if self.invoke(add_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If invoke returned false, the method doesn't exist, fall through to default behavior
            }
        }
        
        // Fall back to default numeric/string addition behavior
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
}
