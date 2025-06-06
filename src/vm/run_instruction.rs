/// Main switch for the `OpCode` execution.
///
/// This is a macro for performance reasons.
/// with macro/inlined it takes ~300ms to run fib.gen
/// with function and wrapping return value in option takes ~450ms
///
/// Directly inlining does not work in order to handle dynamic execution
/// from native functions which need direct access to this.
macro_rules! run_instruction {
    ($self:ident) => {
        #[cfg(feature = "trace_execution")]
        {
            let function = &$self.callstack.function();
            let mut disassembler =
                InstructionDisassembler::new(&function.to_value(&$self.heap).chunk, &$self.heap);
            *disassembler.offset = $self.callstack.current().ip;
            #[cfg(feature = "trace_execution_verbose")]
            {
                println!(
                    "Current module: {} at module depth {} and total call depth {}.",
                    $self
                        .modules
                        .last()
                        .expect("Module underflow in disassembler")
                        .to_value(&$self.heap)
                        .name
                        .to_value(&$self.heap),
                    $self.modules.len(),
                    $self.callstack.len()
                );
            }
            println!(
                "          [ { } ]",
                $self
                    .stack
                    .iter()
                    .map(|v| format!("{}", v.to_string(&$self.heap)))
                    .collect::<Vec<_>>()
                    .join(" ][ ")
            );
            print!("{disassembler:?}");
        }
        $self.collect_garbage();
        match OpCode::try_from($self.read_byte()).expect("Internal error: unrecognized opcode") {
            OpCode::Pop => {
                $self.stack.pop().expect("Stack underflow in OP_POP.");
            }
            OpCode::Dup => {
                $self.stack_push_value(*$self.peek(0).expect("Stack underflow in OP_DUP"));
            }
            OpCode::DupN => {
                // -1 because Dup1 should peek at the top most element
                let depth = usize::from($self.read_byte()) - 1;
                for _ in (0..=depth).rev() {
                    // Always look at depth because each iteration pushes an
                    // additional item onto the stack.
                    // So for N = 2
                    // 1 2 3 4 (depth = 1) -> grab 3
                    // 1 2 3 4 3 (again depth = 1) -> grab 4
                    // 1 2 3 4 3 4
                    $self.stack_push_value(*$self.peek(depth).expect("Stack underflow in OP_DUP"));
                }
            }
            OpCode::Swap => {
                let len = $self.stack.len();
                $self.stack.swap(len - 1, len - 2);
            }
            OpCode::LoadOne => {
                $self.stack.push(1.into());
            }
            OpCode::LoadTwo => {
                $self.stack.push(2.into());
            }
            OpCode::LoadZero => {
                $self.stack.push(0.into());
            }
            OpCode::LoadMinusOne => {
                $self.stack.push((-1).into());
            }
            OpCode::LoadZerof => {
                $self.stack.push((0.0).into());
            }
            OpCode::LoadOnef => {
                $self.stack.push((1.0).into());
            }
            // Grabs index (into the stack) as the operand (next bytecode)
            op @ (OpCode::GetLocal | OpCode::GetLocalLong) => $self.get_local(op),
            // Index is the operand again, value to set is on the stack
            op @ (OpCode::SetLocal | OpCode::SetLocalLong) => $self.set_local(op),
            // Global to get passed as operand
            op @ (OpCode::GetGlobal | OpCode::GetGlobalLong) => {
                if let Some(value) = $self.get_global(op) {
                    return value;
                }
            }
            // Global whose value to set is operand, value to use is on the stack
            op @ (OpCode::SetGlobal | OpCode::SetGlobalLong) => {
                if let Some(value) = $self.set_global(op) {
                    return value;
                }
            }
            // Name of the global to define comes from the operand, value
            op @ (OpCode::DefineGlobal
            | OpCode::DefineGlobalLong
            | OpCode::DefineGlobalConst
            | OpCode::DefineGlobalConstLong) => $self.define_global(op),
            OpCode::JumpIfFalse => $self.jump_conditional(false),
            OpCode::JumpIfTrue => $self.jump_conditional(true),
            // Arg count is passed as the operand
            // The function to call is on the stack followed by all arguments
            // in order from left to right.
            OpCode::Call => {
                if let Some(value) = $self.call() {
                    return value;
                }
            }
            // Value to return is on the stack
            OpCode::Return => {
                if let Some(value) = $self.return_() {
                    return value;
                }
            }
            // Index of the constant is the operand, value is in the constants table
            OpCode::Constant => {
                let value = $self.read_constant(false);
                $self.stack_push(value);
            }
            OpCode::ConstantLong => {
                let value = $self.read_constant(true);
                $self.stack_push(value);
            }
            // `Negate` and `Not` work on the stack value
            OpCode::Negate => {
                if let Some(value) = $self.negate() {
                    return value;
                }
            }
            OpCode::Not => $self.not_(),
            OpCode::Nil => $self.stack_push(Value::Nil),
            OpCode::True => $self.stack_push(Value::Bool(true)),
            OpCode::False => $self.stack_push(Value::Bool(false)),
            OpCode::StopIteration => $self.stack.push(Value::StopIteration),
            OpCode::Equal => $self.equal(false),
            // All of these work on the top two stack values.
            // Top most is right operand, second is left.
            OpCode::Add => {
                if let Some(value) = $self.add() {
                    return value;
                }
            }
            OpCode::Subtract => binary_op!($self, sub, false, mut_heap),
            OpCode::Multiply => binary_op!($self, mul, false, mut_heap),
            OpCode::Divide => binary_op!($self, div, false, mut_heap),
            OpCode::BitXor => binary_op!($self, bitxor, true, mut_heap),
            OpCode::BitAnd => binary_op!($self, bitand, true, mut_heap),
            OpCode::BitOr => binary_op!($self, bitor, true, mut_heap),
            OpCode::Exp => binary_op!($self, pow, false, mut_heap),
            OpCode::Mod => binary_op!($self, rem, false, mut_heap),
            OpCode::FloorDiv => binary_op!($self, floor_div, false, mut_heap),
            OpCode::Greater => binary_op!($self, gt, false, non_mut_heap),
            OpCode::Less => binary_op!($self, lt, false, non_mut_heap),
            OpCode::GreaterEqual => binary_op!($self, ge, false, non_mut_heap),
            OpCode::LessEqual => binary_op!($self, le, false, non_mut_heap),
            OpCode::NotEqual => $self.equal(true),
            OpCode::Jump => {
                let offset = $self.read_16bit_number();
                $self.callstack.current_mut().ip += offset;
            }
            // Offset to jump backwards is the operand(s)
            OpCode::Loop => {
                let offset = $self.read_16bit_number();
                $self.callstack.current_mut().ip -= offset;
            }
            // Get the function with the actual bytecode as a value from the operand
            // Capture the upvalues and push the closure onto the stack
            OpCode::Closure => {
                let value = $self.read_constant(false);
                let function = value.as_function();
                let mut closure =
                    Closure::new(*function, false, $self.modules.last().copied(), &$self.heap);

                for _ in 0..closure.upvalue_count {
                    let is_local = $self.read_byte();
                    debug_assert!(
                        is_local == 0 || is_local == 1,
                        "'is_local` must be 0 or 1, got {is_local}"
                    );
                    let is_local = is_local == 1;

                    let index = usize::from($self.read_byte());
                    if is_local {
                        closure.upvalues.push($self.capture_upvalue(index));
                    } else {
                        closure
                            .upvalues
                            .push($self.callstack.closure().to_value(&$self.heap).upvalues[index]);
                    }
                }
                let closure_id = $self.heap.add_closure(closure);
                $self.stack_push(closure_id);
            }
            // Upvalue index is the operand
            // Closure is the one on the callstack
            OpCode::GetUpvalue => {
                let upvalue_index = usize::from($self.read_byte());
                let closure = $self.callstack.closure();
                let upvalue_location =
                    closure.to_value(&$self.heap).upvalues[upvalue_index].to_value(&$self.heap);
                match *upvalue_location {
                    Upvalue::Open(absolute_local_index) => {
                        $self.stack_push($self.stack[absolute_local_index]);
                    }
                    Upvalue::Closed(value) => $self.stack_push(value),
                }
            }
            // Upvalue index is the operand, closure is one the callstack,
            // value to set is on the stack
            OpCode::SetUpvalue => {
                let upvalue_index = usize::from($self.read_byte());
                let closure = $self.callstack.closure();
                let upvalue_location = closure.to_value(&$self.heap).upvalues[upvalue_index]
                    .clone()
                    .to_value_mut(&mut $self.heap);
                let new_value = $self
                    .stack
                    .last()
                    .copied()
                    .expect("Stack underflow in OP_SET_UPVALUE");
                match *upvalue_location {
                    Upvalue::Open(absolute_local_index) => {
                        $self.stack[absolute_local_index] = new_value;
                    }
                    Upvalue::Closed(ref mut value) => {
                        *value = new_value;
                    }
                }
            }
            // CLose the upvalue on top of the stack
            OpCode::CloseUpvalue => {
                $self.close_upvalue($self.stack.len() - 1);
                $self.stack.pop();
            }
            // Classname is the operand, create a new class and push it onto the stack
            OpCode::Class => {
                let class_name = $self.read_string("OP_CLASS");
                let class = $self.heap.add_class(Class::new(class_name, false));
                $self.stack_push_value(class);
            }
            // Property to get is the operand, instance/module is on the stack
            OpCode::GetProperty => {
                let field = $self.read_string("GET_PROPERTY");
                let value = *$self.peek(0).expect("Stack underflow in GET_PROPERTY");
                match value {
                    Value::Instance(instance) => {
                        // Can either be a normal property
                        if let Some(value) = instance
                            .to_value(&$self.heap)
                            .fields
                            .get(field.to_value(&$self.heap))
                        {
                            $self.stack.pop(); // instance
                            $self.stack_push(*value);
                        } else if $self
                            .bind_method(instance.to_value(&$self.heap).class.into(), field)
                        {
                            // Or could be a method that has to be bound to the
                            // instance so that it can later be called separately.
                            // Just using the side effects
                        } else {
                            runtime_error!(
                                $self,
                                "Undefined property '{}'.",
                                field.to_value(&$self.heap)
                            );
                            return InterpretResult::RuntimeError;
                        }
                    }
                    Value::Module(module) => {
                        if let Some(value) = module.to_value(&$self.heap).globals.get(&field) {
                            $self.stack.pop(); // module
                            $self.stack_push(value.value);
                        } else {
                            runtime_error!(
                                $self,
                                "Undefined name '{}' in module {}.",
                                field.to_value(&$self.heap),
                                module.to_value(&$self.heap).name.to_value(&$self.heap)
                            );
                            return InterpretResult::RuntimeError;
                        }
                    }
                    x => {
                        runtime_error!(
                            $self,
                            "Tried to get property '{}' of non-instance `{}`.",
                            field.to_value(&$self.heap),
                            x.to_string(&$self.heap)
                        );
                        return InterpretResult::RuntimeError;
                    }
                };
            }
            // Property to set is the operand, instance is on the stack
            // as is the value to set.
            OpCode::SetProperty => {
                let field_string_id = $self.read_string("SET_PROPERTY");
                let field = field_string_id.to_value(&$self.heap).clone();
                let value = $self.stack.pop().expect("Stack underflow in SET_PROPERTY");
                let mut receiver = $self.stack.pop().expect("Stack underflow in SET_PROPERTY");
                match &mut receiver {
                    Value::Instance(instance) => {
                        instance
                            .to_value_mut(&mut $self.heap)
                            .fields
                            .insert(field, value);
                    }
                    Value::Module(module) => {
                        if let Some(global) = module
                            .to_value_mut(&mut $self.heap)
                            .globals
                            .get_mut(&field_string_id)
                        {
                            if !global.mutable {
                                runtime_error!($self, "Reassignment to global 'const'.");
                                return InterpretResult::RuntimeError;
                            }
                            global.value = value;
                        }
                    }
                    x => {
                        runtime_error!(
                            $self,
                            "Tried to set property '{}' of non-instance `{}`",
                            field,
                            x.to_string(&$self.heap)
                        );
                        return InterpretResult::RuntimeError;
                    }
                };
                $self.stack_push(value);
            }
            // Name of the method is the operand, actual method to is on the stack
            // together with the class (... --- Class --- Method)
            OpCode::Method => {
                let method_name = $self.read_string("OP_METHOD");
                $self.define_method(method_name);
            }
            // Operands are method name to invoke as well as the number of arguments
            // Stack contains the instance followed by the arguments.
            // (... --- Instance --- arg1 --- arg2 --- ... --- argN)
            OpCode::Invoke => {
                let method_name = $self.read_string("OP_INVOKE");
                let arg_count = $self.read_byte();
                if !$self.invoke(method_name, arg_count) {
                    return InterpretResult::RuntimeError;
                }
            }
            // Stack has (... --- Superclass --- Class)
            OpCode::Inherit => {
                let superclass_id = $self.peek(1).expect("Stack underflow in OP_INHERIT");
                let superclass = if let Value::Class(superclass) = &superclass_id {
                    if superclass.to_value(&$self.heap).is_native {
                        runtime_error!($self, "Can not inherit from native classes yet.");
                        return InterpretResult::RuntimeError;
                    }
                    superclass
                } else {
                    runtime_error!($self, "Superclass must be a class.");
                    return InterpretResult::RuntimeError;
                };
                let methods = superclass.to_value(&$self.heap).methods.clone();
                let mut subclass = $self.stack.pop().expect("Stack underflow in OP_INHERIT");
                subclass
                    .as_class_mut()
                    .to_value_mut(&mut $self.heap)
                    .methods
                    .extend(methods);
            }
            // Grab and bind a method from the superclass
            // Operand is the name of the method to get and the stack has the superclass
            OpCode::GetSuper => {
                let method_name = $self.read_string("OP_GET_SUPER");
                let superclass = $self.stack.pop().expect("Stack underflow in OP_GET_SUPER");
                if !$self.bind_method(superclass, method_name) {
                    return InterpretResult::RuntimeError;
                }
            }
            // Invoke a method from the superclass
            // Operands are the name of the method and number of arguments
            // Stack has the superclass followed by the arguments
            OpCode::SuperInvoke => {
                let method_name = $self.read_string("OP_SUPER_INVOKE");
                let arg_count = $self.read_byte();
                let superclass = $self
                    .stack
                    .pop()
                    .expect("Stack underflow in OP_SUPER_INVOKE");
                if !$self.invoke_from_class(superclass, method_name, arg_count) {
                    return InterpretResult::RuntimeError;
                }
            }
            // Build a list. The number of items is the operand.
            // Items are on the stack in order from left to right
            // (... --- item1 --- item2 --- ... --- itemN)
            OpCode::BuildList => {
                let mut list = List::new();

                let arg_count = $self.read_byte();
                for index in (0..arg_count).rev() {
                    list.items.push(*$self.peek(index as usize).unwrap());
                }
                for _ in 0..arg_count {
                    $self.stack.pop();
                }
                let instance = Instance::new(
                    *$self.heap.native_classes.get("List").unwrap(),
                    Some(list.into()),
                );
                let instance_value = $self.heap.add_instance(instance);
                $self.stack_push_value(instance_value);
            }
            // Build a set. The number of items is the operand.
            // Items are on the stack in order from left to right
            // (... --- item1 --- item2 --- ... --- itemN)
            OpCode::BuildSet => {
                let mut set = Set::new();

                let arg_count = $self.read_byte();
                for index in (0..arg_count).rev() {
                    let value = $self.peek(index as usize).unwrap();
                    if !value.is_hasheable() {
                        runtime_error!(
                            $self,
                            "Value `{}` is not hashable when this is required for items in a set.",
                            value.to_string(&$self.heap)
                        );
                        return InterpretResult::RuntimeError;
                    }
                    set.add(*value, &$self.heap);
                }
                for _ in 0..arg_count {
                    $self.stack.pop();
                }
                let instance = Instance::new(
                    *$self.heap.native_classes.get("Set").unwrap(),
                    Some(set.into()),
                );
                let instance_value = $self.heap.add_instance(instance);
                $self.stack_push_value(instance_value);
            }
            // Build a dict. The number of key-value-pairs is the operand.
            // Items are on the stack in order from left to right
            // (... --- key1 --- value1 --- key2 --- value2 --- ... --- keyN --- valueN)
            OpCode::BuildDict => {
                let mut dict = Dict::new();
                // Number of key, value pairs.
                let arg_count = $self.read_byte();
                for index in (0..arg_count).rev() {
                    let key = $self.peek((2 * index + 1) as usize).unwrap();
                    let value = $self.peek((2 * index) as usize).unwrap();
                    dict.add(*key, *value, &$self.heap);
                }
                for _ in 0..arg_count {
                    // Pop key AND value
                    $self.stack.pop();
                    $self.stack.pop();
                }
                let instance = Instance::new(
                    *$self.heap.native_classes.get("Dict").unwrap(),
                    Some(dict.into()),
                );
                let instance_value = $self.heap.add_instance(instance);
                $self.stack_push_value(instance_value);
            }
            // Import a module by filepath without qualifiers.
            // Expects either the path to the module or the name of
            // a stdlib module as a string as an operand.
            OpCode::Import => {
                let file_path = $self.read_string("OP_IMPORT_AS");
                let local_import = $self.read_byte() == 1;
                if let Some(value) = $self.import_file(file_path, None, None, local_import) {
                    return value;
                }
            }
            // Import a module by filepath with an alias.
            // Name of the module to import is on the stack, alias is the operand.
            OpCode::ImportAs => {
                let file_path = $self.read_string("OP_IMPORT_AS");
                let alias = $self.read_string("OP_IMPORT_AS");
                let local_import = $self.read_byte() == 1;
                if let Some(value) = $self.import_file(file_path, None, Some(alias), local_import) {
                    return value;
                }
            }
            // Import a set of names from a module.
            // Number of names to import are the operand.
            // Module to import them from and the names are on the stack.
            // (... --- modulename --- name1 --- name2 --- ... --- nameN)
            OpCode::ImportFrom => {
                let file_path = $self.read_string("OP_IMPORT_AS");
                let local_import = $self.read_byte() == 1;
                let n_names_to_import = $self.read_byte();
                let names_to_import = if n_names_to_import > 0 {
                    Some(
                        (0..n_names_to_import)
                            .map(|_| $self.read_string("OP_IMPORT_FROM"))
                            .collect::<Vec<_>>(),
                    )
                } else {
                    None
                };

                if let Some(value) =
                    $self.import_file(file_path, names_to_import, None, local_import)
                {
                    return value;
                }
            }
        };
    };
}
