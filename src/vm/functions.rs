#[cfg(feature = "trace_execution")]
use crate::chunk::InstructionDisassembler;
use crate::value::NativeClass;
use crate::{
    chunk::{CodeOffset, OpCode},
    heap::{NativeFunctionId, NativeMethodId, StringId, UpvalueId},
    types::JumpCondition,
    value::{Class, Closure, Instance, Number, Upvalue, Value},
};

use super::{Global, InterpretResult, RuntimeError, VM};
use crate::vm::arithmetics::{BinaryOpResult, IntoResultValue};

// Execute a function (rust side)
impl VM {
    /// Execute and immediately run a function.
    ///
    /// This is used when (runtime) class specific information is needed
    /// in native functions like `print` or `str`.
    ///
    /// Pushes the closure onto the stack and callstack. Then directly
    /// executes all of the bytecode for it before returning to the main loop.
    pub(crate) fn invoke_and_run_function(
        &mut self,
        method_name: StringId,
        arg_count: u8,
        method_is_native: bool,
    ) -> InterpretResult {
        if self.invoke(method_name, arg_count).is_err() {
            return InterpretResult::RuntimeError;
        }

        if method_is_native {
            InterpretResult::Ok
        } else {
            self.run_function()
        }
    }

    /// Run the closure currently on top of the callstack.
    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    pub(super) fn run_function(&mut self) -> InterpretResult {
        let call_depth = self.callstack.len();
        while self.callstack.len() >= call_depth {
            run_instruction!(self);
        }
        InterpretResult::Ok
    }
}

// Handle a call (generic side)
impl VM {
    pub(super) fn call(&mut self) -> Result<(), RuntimeError> {
        let arg_count = self.read_byte();
        let callee = self.stack[self.stack.len() - 1 - usize::from(arg_count)];
        self.call_value(callee, arg_count)?;
        Ok(())
    }

    /// Invoke a value retrieved from an instance or module.
    ///
    /// If it is an instance and the attribute is not a property of the instance
    /// then a method is looked up in the class.
    pub(crate) fn invoke(
        &mut self,
        method_name: StringId,
        arg_count: u8,
    ) -> Result<(), RuntimeError> {
        let receiver = *self
            .peek(arg_count.into())
            .expect("Stack underflow in OP_INVOKE");
        match receiver {
            Value::Instance(instance) => {
                // Callable attribute of the instance
                if let Some(value) = instance
                    .to_value(&self.heap)
                    .fields
                    .get(method_name.to_value(&self.heap))
                {
                    let new_stack_base = self.stack.len() - usize::from(arg_count) - 1;
                    self.stack[new_stack_base] = *value;
                    self.call_value(*value, arg_count)
                }
                // Method of the class. Attributes on the instance overwrite methods on the class.
                else {
                    self.invoke_from_class(
                        instance.to_value(&self.heap).class.into(),
                        method_name,
                        arg_count,
                    )
                }
            }
            Value::Module(module) => {
                if let Some(value) = module.to_value(&self.heap).globals.get(&method_name) {
                    let new_stack_base = self.stack.len() - usize::from(arg_count) - 1;
                    self.stack[new_stack_base] = value.value;
                    self.call_value(value.value, arg_count)
                } else {
                    runtime_error!(
                        self,
                        "Function '{}' not defined in module {}.",
                        method_name.to_value(&self.heap),
                        module.to_value(&self.heap).name.to_value(&self.heap)
                    );
                    Err(RuntimeError::new())
                }
            }
            _ => {
                runtime_error!(self, "Only instances have methods.");
                Err(RuntimeError::new())
            }
        }
    }

    /// Invoke a method on an instance directly from its class.
    pub(super) fn invoke_from_class(
        &mut self,
        class: Value,
        method_name: StringId,
        arg_count: u8,
    ) -> Result<(), RuntimeError> {
        let Some(method) = class
            .as_class()
            .to_value(&self.heap)
            .methods
            .get(&method_name)
        else {
            runtime_error!(
                self,
                "Undefined property '{}'.",
                self.heap.strings[method_name]
            );
            return Err(RuntimeError::new());
        };
        match method {
            Value::Closure(_) => self.execute_call(*method, arg_count),
            Value::NativeMethod(native) => {
                let mut receiver = *self.peek(arg_count as usize).unwrap();
                self.execute_native_method_call(*native, &mut receiver, arg_count)
            }
            x => unreachable!(
                "Can only invoke closure or native methods. Got `{}` instead.",
                x.to_string(&self.heap)
            ),
        }
    }

    /// Call the passed value with the passed number of arguments.
    ///
    /// The arguments should reside on top of the stack with the first (leftmost) argument
    /// being the deepest on the stack directly ontop of where the `callee` was taken from.
    ///
    /// Callable values are:
    /// - Closures:
    ///    - Are scheduled directly to be executed.
    /// - Native functions:
    ///   - Are executed directly.
    /// - Classes:
    ///     - Are instantiated and the initializer is called.
    /// - Bound methods:
    ///    - If the bound method is a standard one, it is scheduled for execution.
    ///    - If the bound method is a native one, it is executed directly.
    fn call_value(&mut self, callee: Value, arg_count: u8) -> Result<(), RuntimeError> {
        let call_id = self.heap.string_id(&"__call__");
        match callee {
            Value::NativeMethod(_) => {
                println!("Got a native method");
                Err(RuntimeError::new())
            }
            Value::Instance(instance)
                if instance
                    .to_value(&self.heap)
                    .has_field_or_method(call_id, &self.heap) =>
            {
                self.invoke(call_id, arg_count)
            }
            Value::Closure(_) => self.execute_call(callee, arg_count),
            Value::NativeFunction(f) => self.execute_native_function_call(f, arg_count),
            Value::Class(class) => {
                let class_data = class.to_value(&self.heap);
                let maybe_initializer = class_data
                    .methods
                    .get(&self.heap.builtin_constants().init_string)
                    .copied();

                let backing = class_data.get_native_superclass(&self.heap, class).map(
                    |native_superclass_id| {
                        let native_superclass = native_superclass_id.to_value(&self.heap);
                        NativeClass::new(native_superclass.name.to_value(&self.heap))
                    },
                );

                let mut instance_id = self.heap.add_instance(Instance::new(callee, backing));
                let stack_index = self.stack.len() - usize::from(arg_count) - 1;
                self.stack[stack_index] = instance_id;
                if let Some(initializer) = maybe_initializer {
                    match initializer {
                        Value::NativeMethod(native_method_id) => self.execute_native_method_call(
                            native_method_id,
                            &mut instance_id,
                            arg_count,
                        ),
                        _ => self.execute_call(initializer, arg_count),
                    }
                } else if arg_count != 0 {
                    runtime_error!(self, "Expected 0 arguments but got {arg_count}.");
                    Err(RuntimeError::new())
                } else {
                    Ok(())
                }
            }
            Value::BoundMethod(bound_method) => match bound_method.to_value(&self.heap).method {
                Value::Closure(_) => {
                    let bound_method = bound_method.to_value(&self.heap);
                    let new_stack_base = self.stack.len() - usize::from(arg_count) - 1;
                    self.stack[new_stack_base] = bound_method.receiver;
                    self.execute_call(bound_method.method, arg_count)
                }
                Value::NativeMethod(native_method) => self.execute_native_method_call(
                    native_method,
                    &mut bound_method.to_value(&self.heap).receiver.clone(),
                    arg_count,
                ),
                _ => {
                    runtime_error!(
                        self,
                        "Native methods only bind over closures or native methods."
                    );
                    Err(RuntimeError::new())
                }
            },
            _ => {
                runtime_error!(
                    self,
                    "Can only call functions, classes and instances with a `__call__` method."
                );
                Err(RuntimeError::new())
            }
        }
    }

    /// Execute a normal closure call.
    ///
    /// The arity of the closure is checked against the provided number of arguments.
    /// Then the closure is pushed onto the callstack.
    pub(super) fn execute_call(
        &mut self,
        closure_id: Value,
        arg_count: u8,
    ) -> Result<(), RuntimeError> {
        let closure = closure_id.as_closure();
        let arity = closure
            .to_value(&self.heap)
            .function
            .to_value(&self.heap)
            .arity;
        let arg_count = usize::from(arg_count);
        if arg_count != arity {
            runtime_error!(
                self,
                "Expected {} argument{} but got {}.",
                arity,
                { if arity == 1 { "" } else { "s" } },
                arg_count
            );
            return Err(RuntimeError::new());
        }

        if self.callstack.len() == crate::config::FRAMES_MAX {
            runtime_error!(self, "Stack overflow.");
            return Err(RuntimeError::new());
        }

        self.callstack.push(
            *closure_id.as_closure(),
            self.stack.len() - arg_count - 1,
            &self.heap,
        );
        Ok(())
    }

    /// Execute a call to a native function.
    ///
    /// Checks that the number of arguments matches to the arity of the function.
    /// After the call the stack is truncated to remove the arguments and the function
    /// and the result is pushed onto the stack.
    #[allow(clippy::branches_sharing_code)]
    fn execute_native_function_call(
        &mut self,
        f: NativeFunctionId,
        arg_count: u8,
    ) -> Result<(), RuntimeError> {
        let f = f.to_value(&self.heap);
        let arity = f.arity;
        if !arity.contains(&arg_count) {
            if arity.len() == 1 {
                runtime_error!(
                    self,
                    "Native function '{}' expected {} argument{}, got {}.",
                    f.name.to_value(&self.heap),
                    arity[0],
                    { if arity[0] == 1 { "" } else { "s" } },
                    arg_count
                );
            } else {
                runtime_error!(
                    self,
                    "Native function '{}' expected any of {:?} arguments, got {}.",
                    f.name.to_value(&self.heap),
                    arity,
                    arg_count
                );
            }
            return Err(RuntimeError::new());
        }
        let fun = f.fun;
        let start_index = self.stack.len() - usize::from(arg_count);
        let mut args: Vec<Value> = self.stack[start_index..].to_vec();
        let mut ref_args: Vec<&mut Value> = args.iter_mut().collect();
        let result = fun(self, ref_args.as_mut_slice());
        match result {
            Ok(value) => {
                self.stack.truncate(start_index - 1);
                self.stack_push(value);
                Ok(())
            }
            Err(e) => {
                runtime_error!(self, "{}", e);
                Err(RuntimeError::new())
            }
        }
    }

    /// Execute a call to a native method.
    ///
    /// Checks that the number of arguments matches to the arity of the method.
    /// After the call the stack is truncated to remove the arguments and the receiver
    /// and the result is pushed onto the stack.
    #[allow(clippy::branches_sharing_code)]
    fn execute_native_method_call(
        &mut self,
        f: NativeMethodId,
        receiver: &mut Value,
        arg_count: u8,
    ) -> Result<(), RuntimeError> {
        let f = f.to_value(&self.heap);
        let arity = f.arity;
        if !arity.contains(&arg_count) {
            if arity.len() == 1 {
                runtime_error!(
                    self,
                    "Native method '{}' of class {} expected {} argument{}, got {}.",
                    f.name.to_value(&self.heap),
                    *receiver.class_name(&self.heap).to_value(&self.heap),
                    arity[0],
                    { if arity[0] == 1 { "" } else { "s" } },
                    arg_count
                );
            } else {
                runtime_error!(
                    self,
                    "Native method '{}' of class {} expected any of {:?} arguments, got {}.",
                    *f.name.to_value(&self.heap),
                    *receiver.class_name(&self.heap).to_value(&self.heap),
                    arity,
                    arg_count
                );
            }
            return Err(RuntimeError::new());
        }
        let fun = f.fun;
        let start_index = self.stack.len() - usize::from(arg_count);
        let mut args: Vec<Value> = self.stack[start_index..].to_vec();
        let mut ref_args: Vec<&mut Value> = args.iter_mut().collect();
        let result = fun(self, receiver, ref_args.as_mut_slice());
        match result {
            Ok(value) => {
                self.stack
                    .truncate(self.stack.len() - usize::from(arg_count) - 1);
                self.stack_push(value);
                Ok(())
            }
            Err(e) => {
                runtime_error!(self, "{}", e);
                Err(RuntimeError::new())
            }
        }
    }
}

// Define methods, classes, and modules, functions
impl VM {
    /// Bind a method to an instance.
    ///
    /// The instance is still on top of the stack.
    pub(super) fn bind_method(&mut self, class: Value, name: StringId) -> bool {
        let class = class.as_class();
        let Some(method) = class.to_value(&self.heap).methods.get(&name) else {
            return false;
        };
        let bound_method = Value::bound_method(
            // the instance
            *self.peek(0).expect("Buffer underflow in OP_METHOD"),
            *method,
            &mut self.heap,
        );
        self.stack.pop(); // instance
        self.stack_push_value(bound_method);
        true
    }

    /// Capture upvalues from the surrounding scope.
    ///
    /// Iterate over all open upvalues up to the desired index.
    /// If the the requested value has already been captured then reuse that.
    /// Otherwise create a new upvalue and insert it into the list of open upvalues.
    pub(super) fn capture_upvalue(&mut self, local: usize) -> UpvalueId {
        let local = self.callstack.current().stack_base + local;
        let mut upvalue_index = 0;
        let mut upvalue = None;

        for (i, this) in self.open_upvalues.iter().enumerate() {
            upvalue = Some(this);
            upvalue_index = i;
            if this.to_value(&self.heap).as_open() <= local {
                break;
            }
        }

        if let Some(upvalue) = upvalue
            && upvalue.to_value(&self.heap).as_open() == local
        {
            return *upvalue;
        }
        let upvalue = self.heap.add_upvalue(Upvalue::Open(local));
        let upvalue_id = upvalue.upvalue_location();
        self.open_upvalues.insert(upvalue_index, *upvalue_id);

        *upvalue_id
    }

    /// Close the upvalue from the specified position.
    ///
    /// This is used to close upvalues when a their defining scope ends
    /// and they are still captured by a closure.
    pub(super) fn close_upvalue(&mut self, last: usize) {
        while self
            .open_upvalues
            .front()
            .is_some_and(|v| v.to_value(&self.heap).as_open() >= last)
        {
            let upvalue = self
                .open_upvalues
                .pop_front()
                .unwrap()
                .to_value_mut(&mut self.heap);

            let pointed_value = self.stack[upvalue.as_open()];
            *upvalue = Upvalue::Closed(pointed_value);
        }
    }

    pub(super) fn define_method(&mut self, method_name: StringId) {
        let method = *self.peek(0).expect("Stack underflow in OP_METHOD");
        let class = *self
            .peek_mut(1)
            .expect("Stack underflow in OP_METHOD")
            .as_class_mut();
        class
            .to_value_mut(&mut self.heap)
            .methods
            .insert(method_name, method);
        self.stack.pop();
    }
}

// Return
impl VM {
    /// Return from the current module or function.
    ///
    /// If the current frame is a module then the module is imported into the
    /// current module. If the current module is the main script that we return
    /// from the main loop.
    ///
    /// If the current frame is a function, we return the value and close the upvalues.
    pub(super) fn return_(&mut self) -> Result<Option<InterpretResult>, RuntimeError> {
        // Pop the return value. If none was specified (empty return, missing return, module)
        // then the value is nil. This is handled by the compiler.
        let result = self.stack.pop();
        let frame = self
            .callstack
            .pop(&self.heap)
            .expect("Call stack underflow in OP_RETURN");
        // We just popped the main script
        if self.callstack.is_empty() {
            self.stack.pop();
            return Ok(Some(InterpretResult::Ok));
        }
        if frame.is_module {
            // Pop the module itself from the stack
            self.stack.pop();
            let last_module = self.modules.pop().expect("Module underflow in OP_RETURN");
            let last_module_alias = last_module.to_value(&self.heap).alias;
            let names_to_import =
                std::mem::take(&mut last_module.to_value_mut(&mut self.heap).names_to_import);
            let was_local_import = last_module.to_value(&self.heap).local_import;

            // This has to be modified to put imports into the correct scope.
            // Currently they are just put into the global scope.
            if let Some(names) = names_to_import {
                for name in names {
                    let Some(value) = last_module.to_value(&self.heap).globals.get(&name).copied()
                    else {
                        runtime_error!(
                            self,
                            "Could not find name to import `{}`.",
                            name.to_value(&self.heap)
                        );
                        return Err(RuntimeError::new());
                    };
                    if was_local_import {
                        self.stack_push(value.value);
                    } else {
                        self.globals().insert(name, value);
                    }
                }
            } else if was_local_import {
                self.stack_push(last_module.into());
            } else {
                self.globals().insert(
                    last_module_alias,
                    Global {
                        value: last_module.into(),
                        mutable: true,
                    },
                );
            }

            let script_name = self.heap.builtin_constants().script_name;
            let module_name: Value = self
                .modules
                .last()
                .expect("Module underflow in OP_RETURN")
                .to_value(&self.heap)
                .name
                .into();
            self.globals().insert(
                script_name,
                Global {
                    value: module_name,
                    mutable: true,
                },
            );
            return Ok(None);
        }
        // Normal function return
        self.close_upvalue(frame.stack_base);
        // Pop all of the arguments and locals as well as the function itself.
        self.stack.truncate(frame.stack_base);
        self.stack_push(result.expect("Stack underflow in OP_RETURN"));
        Ok(None)
    }
}
