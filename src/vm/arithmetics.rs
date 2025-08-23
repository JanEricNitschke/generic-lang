use super::VM;
use crate::{
    value::{GenericRational, Number, Value},
    vm::errors::VmResult,
};

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

        let message = format!(
            "Operands must be {} or support `{}`. Got: [{}]",
            if $int_only { "integers" } else { "numbers" },
            $gen_method,
            $self.stack[slice_start..]
                .iter()
                .map(|v| v.to_string(&$self.heap))
                .collect::<Vec<_>>()
                .join(", ")
        );
        match &$self.stack[slice_start..] {
            [Value::Number(a), Value::Number(b)] => {
                if $int_only & (!matches!(a, Number::Integer(_)) | !matches!(b, Number::Integer(_)))
                {
                    $self.throw_type_error(&message)
                } else {
                    match binary_op_invoke!($self, a, b, $method, $heap_toggle).into_result_value()
                    {
                        Ok(value) => {
                            $self.stack.pop();
                            $self.stack.pop();
                            $self.stack_push_value(value);
                            Ok(())
                        }
                        Err(error) => $self.throw_value_error(&error),
                    }
                }
            }
            [Value::Instance(instance_id), _]
                if instance_id
                    .to_value(&$self.heap)
                    .has_field_or_method(gen_method_id, &$self.heap) =>
            {
                $self.invoke(gen_method_id, 1)
            }
            _ => $self.throw_type_error(&message),
        }
    }};
}

impl VM {
    pub(super) fn add(&mut self) -> VmResult {
        let slice_start = self.stack.len() - 2;
        let gen_method_id = self.heap.string_id(&"__add__");
        match &self.stack[slice_start..] {
            [Value::Number(a), Value::Number(b)] => {
                let value = (a.add(*b, &mut self.heap)).into();
                self.stack.pop();
                self.stack.pop();
                self.stack_push_value(value);
                Ok(())
            }
            [Value::String(a), Value::String(b)] => {
                // This could be optimized by allowing mutations via the heap
                let new_string = format!("{}{}", self.heap.strings[*a], self.heap.strings[*b]);
                let new_string_id = self.heap.string_id(&new_string);
                self.stack.pop();
                self.stack.pop();
                self.stack_push_value(new_string_id.into());
                Ok(())
            }
            [Value::Instance(instance_id), _]
                if instance_id
                    .to_value(&self.heap)
                    .has_field_or_method(gen_method_id, &self.heap) =>
            {
                self.invoke(gen_method_id, 1)
            }
            _ => {
                let message = format!(
                    "Operands must be two numbers, strings or support `__add__`. Got: [{}]",
                    self.stack[slice_start..]
                        .iter()
                        .map(|v| v.to_string(&self.heap))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                self.throw_type_error(&message)
            }
        }
    }

    /// Negate the top value on the stack.
    ///
    /// # Panics
    ///
    /// If the stack is empty. This is an internal error and should never happen.
    pub(super) fn negate(&mut self) -> VmResult {
        let value_id = *self.peek(0).expect("stack underflow in OP_NEGATE");
        let value = &value_id;
        if let Value::Number(n) = value {
            self.stack.pop();
            let negated = n.neg(&mut self.heap);
            self.stack_push_value(negated.into());
        } else {
            return self.throw_type_error("Operand must be a number.");
        }
        Ok(())
    }

    pub(crate) fn build_rational(&mut self) -> VmResult {
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
                let message = format!(
                    "Invalid operands ({}, {}) for rational construction.",
                    numerator.to_string(&self.heap),
                    denominator.to_string(&self.heap)
                );
                return self.throw_type_error(&message);
            }
        }
        Ok(())
    }
}
