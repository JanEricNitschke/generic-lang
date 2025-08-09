use super::{InterpretResult, VM};
use crate::chunk::CodeOffset;
use crate::value::Value;

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
            if let Value::Instance(instance) = left {
                let add_method_name = self.heap.string_id(&"__add__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__add__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&add_method_name);
                
                if has_method && self.invoke(add_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
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

    pub(super) fn subtract(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        
        // First try __sub__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(instance) = left {
                let sub_method_name = self.heap.string_id(&"__sub__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__sub__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&sub_method_name);
                
                if has_method && self.invoke(sub_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
            }
        }
        
        // Fall back to default numeric subtraction behavior
        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    let value = (a.sub(*b, &mut self.heap)).into();
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(value);
                    true
                } else {
                    false
                }
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be numbers. Got: [{}]",
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

    pub(super) fn multiply(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        
        // First try __mul__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(instance) = left {
                let mul_method_name = self.heap.string_id(&"__mul__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__mul__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&mul_method_name);
                
                if has_method && self.invoke(mul_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
            }
        }
        
        // Fall back to default numeric multiplication behavior
        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    let value = (a.mul(*b, &mut self.heap)).into();
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(value);
                    true
                } else {
                    false
                }
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be numbers. Got: [{}]",
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

    pub(super) fn divide(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        
        // First try __div__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(instance) = left {
                let div_method_name = self.heap.string_id(&"__div__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__div__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&div_method_name);
                
                if has_method && self.invoke(div_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
            }
        }
        
        // Fall back to default numeric division behavior
        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    match a.div(*b, &mut self.heap).into_result_value() {
                        Ok(value) => {
                            self.stack.pop();
                            self.stack.pop();
                            self.stack_push_value(value);
                            true
                        }
                        Err(error) => {
                            runtime_error!(self, "{error}");
                            return Some(InterpretResult::RuntimeError);
                        }
                    }
                } else {
                    false
                }
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be numbers. Got: [{}]",
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

    pub(super) fn modulo(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        
        // First try __mod__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(instance) = left {
                let mod_method_name = self.heap.string_id(&"__mod__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__mod__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&mod_method_name);
                
                if has_method && self.invoke(mod_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
            }
        }
        
        // Fall back to default numeric modulo behavior
        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    match a.rem(*b, &mut self.heap).into_result_value() {
                        Ok(value) => {
                            self.stack.pop();
                            self.stack.pop();
                            self.stack_push_value(value);
                            true
                        }
                        Err(error) => {
                            runtime_error!(self, "{error}");
                            return Some(InterpretResult::RuntimeError);
                        }
                    }
                } else {
                    false
                }
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be numbers. Got: [{}]",
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

    pub(super) fn power(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        
        // First try __pow__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(instance) = left {
                let pow_method_name = self.heap.string_id(&"__pow__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__pow__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&pow_method_name);
                
                if has_method && self.invoke(pow_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
            }
        }
        
        // Fall back to default numeric power behavior
        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    match a.pow(*b, &mut self.heap).into_result_value() {
                        Ok(value) => {
                            self.stack.pop();
                            self.stack.pop();
                            self.stack_push_value(value);
                            true
                        }
                        Err(error) => {
                            runtime_error!(self, "{error}");
                            return Some(InterpretResult::RuntimeError);
                        }
                    }
                } else {
                    false
                }
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be numbers. Got: [{}]",
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

    pub(super) fn floor_divide(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        
        // First try __floordiv__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(instance) = left {
                let floordiv_method_name = self.heap.string_id(&"__floordiv__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__floordiv__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&floordiv_method_name);
                
                if has_method && self.invoke(floordiv_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
            }
        }
        
        // Fall back to default numeric floor division behavior
        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    match a.floor_div(*b, &mut self.heap).into_result_value() {
                        Ok(value) => {
                            self.stack.pop();
                            self.stack.pop();
                            self.stack_push_value(value);
                            true
                        }
                        Err(error) => {
                            runtime_error!(self, "{error}");
                            return Some(InterpretResult::RuntimeError);
                        }
                    }
                } else {
                    false
                }
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be numbers. Got: [{}]",
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

    pub(super) fn greater(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        
        // First try __gt__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(instance) = left {
                let gt_method_name = self.heap.string_id(&"__gt__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__gt__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&gt_method_name);
                
                if has_method && self.invoke(gt_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
            }
        }
        
        // Fall back to default numeric comparison behavior
        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    let value = a.gt(b, &self.heap).into();
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(value);
                    true
                } else {
                    false
                }
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be numbers. Got: [{}]",
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

    pub(super) fn less(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        
        // First try __lt__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(instance) = left {
                let lt_method_name = self.heap.string_id(&"__lt__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__lt__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&lt_method_name);
                
                if has_method && self.invoke(lt_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
            }
        }
        
        // Fall back to default numeric comparison behavior
        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    let value = a.lt(b, &self.heap).into();
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(value);
                    true
                } else {
                    false
                }
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be numbers. Got: [{}]",
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

    pub(super) fn greater_equal(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        
        // First try __ge__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(instance) = left {
                let ge_method_name = self.heap.string_id(&"__ge__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__ge__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&ge_method_name);
                
                if has_method && self.invoke(ge_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
            }
        }
        
        // Fall back to default numeric comparison behavior
        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    let value = a.ge(b, &self.heap).into();
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(value);
                    true
                } else {
                    false
                }
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be numbers. Got: [{}]",
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

    pub(super) fn less_equal(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        
        // First try __le__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(instance) = left {
                let le_method_name = self.heap.string_id(&"__le__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__le__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&le_method_name);
                
                if has_method && self.invoke(le_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
            }
        }
        
        // Fall back to default numeric comparison behavior
        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    let value = a.le(b, &self.heap).into();
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(value);
                    true
                } else {
                    false
                }
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be numbers. Got: [{}]",
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

    pub(super) fn bitwise_xor(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        
        // First try __xor__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(instance) = left {
                let xor_method_name = self.heap.string_id(&"__xor__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__xor__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&xor_method_name);
                
                if has_method && self.invoke(xor_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
            }
        }
        
        // Fall back to default bitwise XOR behavior
        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    match a.bitxor(*b, &mut self.heap).into_result_value() {
                        Ok(value) => {
                            self.stack.pop();
                            self.stack.pop();
                            self.stack_push_value(value);
                            true
                        }
                        Err(error) => {
                            runtime_error!(self, "{error}");
                            return Some(InterpretResult::RuntimeError);
                        }
                    }
                } else {
                    false
                }
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be integers. Got: [{}]",
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

    pub(super) fn bitwise_and(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        
        // First try __and__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(instance) = left {
                let and_method_name = self.heap.string_id(&"__and__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__and__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&and_method_name);
                
                if has_method && self.invoke(and_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
            }
        }
        
        // Fall back to default bitwise AND behavior
        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    match a.bitand(*b, &mut self.heap).into_result_value() {
                        Ok(value) => {
                            self.stack.pop();
                            self.stack.pop();
                            self.stack_push_value(value);
                            true
                        }
                        Err(error) => {
                            runtime_error!(self, "{error}");
                            return Some(InterpretResult::RuntimeError);
                        }
                    }
                } else {
                    false
                }
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be integers. Got: [{}]",
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

    pub(super) fn bitwise_or(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        
        // First try __or__ overloading if left operand is an instance
        if let [left, _right] = &self.stack[slice_start..] {
            if let Value::Instance(instance) = left {
                let or_method_name = self.heap.string_id(&"__or__");
                let instance_data = instance.to_value(&self.heap);
                
                // Check if the method exists before invoking to avoid error messages
                let has_method = instance_data.fields.contains_key("__or__") 
                    || instance_data.class.to_value(&self.heap).methods.contains_key(&or_method_name);
                
                if has_method && self.invoke(or_method_name, 1) {
                    return None; // Method call successful, execution continues
                }
                // If method doesn't exist or invoke failed, fall through to default behavior
            }
        }
        
        // Fall back to default bitwise OR behavior
        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    match a.bitor(*b, &mut self.heap).into_result_value() {
                        Ok(value) => {
                            self.stack.pop();
                            self.stack.pop();
                            self.stack_push_value(value);
                            true
                        }
                        Err(error) => {
                            runtime_error!(self, "{error}");
                            return Some(InterpretResult::RuntimeError);
                        }
                    }
                } else {
                    false
                }
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be integers. Got: [{}]",
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
