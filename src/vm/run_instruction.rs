/// Main switch for the `OpCode` execution.
///
/// This is a macro for performance reasons.
/// with macro/inlined it takes ~300ms to run fib.gen
/// with function and wrapping return value in option takes ~450ms
///
/// Directly inlining does not work in order to handle dynamic execution
/// from native functions which need direct access to this.
macro_rules! run_instruction {
    ($self:ident) => {{
        #[cfg(feature = "trace_execution")]
        $self.trace_execution();

        $self.handling_exception = false;
        $self.collect_garbage();
        match OpCode::try_from($self.read_byte()).expect("Internal error: unrecognized opcode") {
            OpCode::Pop => {
                $self.stack.pop().expect("Stack underflow in OP_POP.");
                Ok(None)
            }
            OpCode::Dup => {
                $self.stack_push(*$self.peek(0).expect("Stack underflow in OP_DUP"));
                Ok(None)
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
                    $self.stack_push(*$self.peek(depth).expect("Stack underflow in OP_DUP"));
                }
                Ok(None)
            }
            OpCode::Swap => {
                let len = $self.stack.len();
                $self.stack.swap(len - 1, len - 2);
                Ok(None)
            }
            OpCode::LoadOne => {
                $self.stack.push(1.into());
                Ok(None)
            }
            OpCode::LoadTwo => {
                $self.stack.push(2.into());
                Ok(None)
            }
            OpCode::LoadZero => {
                $self.stack.push(0.into());
                Ok(None)
            }
            OpCode::LoadMinusOne => {
                $self.stack.push((-1).into());
                Ok(None)
            }
            OpCode::LoadZerof => {
                $self.stack.push((0.0).into());
                Ok(None)
            }
            OpCode::LoadOnef => {
                $self.stack.push((1.0).into());
                Ok(None)
            }
            // Grabs index (into the stack) as the operand (next bytecode)
            op @ (OpCode::GetLocal | OpCode::GetLocalLong) => {
                $self.get_local(op);
                Ok(None)
            }
            // Index is the operand again, value to set is on the stack
            op @ (OpCode::SetLocal | OpCode::SetLocalLong) => {
                $self.set_local(op);
                Ok(None)
            }
            // Global to get passed as operand
            op @ (OpCode::GetGlobal | OpCode::GetGlobalLong) => $self.get_global(op),
            // Global whose value to set is operand, value to use is on the stack
            op @ (OpCode::SetGlobal | OpCode::SetGlobalLong) => $self.set_global(op),
            // Name of the global to define comes from the operand, value
            op @ (OpCode::DefineGlobal
            | OpCode::DefineGlobalLong
            | OpCode::DefineGlobalConst
            | OpCode::DefineGlobalConstLong) => {
                $self.define_global(op);
                Ok(None)
            }
            OpCode::JumpIfFalse => $self.jump_conditional(JumpCondition::IfFalse),
            OpCode::JumpIfTrue => $self.jump_conditional(JumpCondition::IfTrue),
            OpCode::PopJumpIfFalse => $self.pop_jump_conditional(JumpCondition::IfFalse),
            OpCode::PopJumpIfTrue => $self.pop_jump_conditional(JumpCondition::IfTrue),
            OpCode::JumpIfTrueOrPop => $self.jump_if_or_pop(JumpCondition::IfTrue),
            OpCode::JumpIfFalseOrPop => $self.jump_if_or_pop(JumpCondition::IfFalse),
            // Arg count is passed as the operand
            // The function to call is on the stack followed by all arguments
            // in order from left to right.
            OpCode::Call => $self.call(),
            // Value to return is on the stack
            OpCode::Return => match $self.return_() {
                Ok(Return::Program(frame)) => return Ok(Some(frame)),
                Ok(Return::Function(frame)) => Ok(Some(frame)),
                Err(err) => Err(err),
            },
            OpCode::Yield => $self.yield_().into(),
            OpCode::ReturnGenerator => {
                $self.create_generator();
                Ok(None)
            }
            // Index of the constant is the operand, value is in the constants table
            OpCode::Constant => {
                let value = $self.read_constant(NumberEncoding::Short);
                $self.stack_push(value);
                Ok(None)
            }
            OpCode::ConstantLong => {
                let value = $self.read_constant(NumberEncoding::Long);
                $self.stack_push(value);
                Ok(None)
            }
            // `Negate` and `Not` work on the stack value
            OpCode::Negate => $self.negate(),
            OpCode::Not => $self.not_(),
            OpCode::Nil => {
                $self.stack_push(Value::Nil);
                Ok(None)
            }
            OpCode::True => {
                $self.stack_push(Value::Bool(true));
                Ok(None)
            }
            OpCode::False => {
                $self.stack_push(Value::Bool(false));
                Ok(None)
            }
            OpCode::StopIteration => {
                $self.stack_push(Value::StopIteration);
                Ok(None)
            }
            OpCode::Equal => $self.equal(EqualityMode::Equal),
            OpCode::NotEqual => $self.equal(EqualityMode::NotEqual),
            OpCode::Is => {
                $self.identical();
                Ok(None)
            }
            // All of these work on the top two stack values.
            // Top most is right operand, second is left.
            OpCode::Add => $self.add(),
            OpCode::Subtract => binary_op!($self, sub, "__sub__", false, mut_heap),
            OpCode::Multiply => binary_op!($self, mul, "__mul__", false, mut_heap),
            OpCode::Divide => binary_op!($self, div, "__div__", false, mut_heap),
            OpCode::BitXor => binary_op!($self, bitxor, "__bitxor__", true, mut_heap),
            OpCode::BitAnd => binary_op!($self, bitand, "__bitand__", true, mut_heap),
            OpCode::BitOr => binary_op!($self, bitor, "__bitor__", true, mut_heap),
            OpCode::Exp => binary_op!($self, pow, "__pow__", false, mut_heap),
            OpCode::Mod => binary_op!($self, rem, "__mod__", false, mut_heap),
            OpCode::FloorDiv => binary_op!($self, floor_div, "__floor_div__", false, mut_heap),
            OpCode::Greater => binary_op!($self, gt, "__gt__", false, non_mut_heap),
            OpCode::Less => binary_op!($self, lt, "__lt__", false, non_mut_heap),
            OpCode::GreaterEqual => binary_op!($self, ge, "__ge__", false, non_mut_heap),
            OpCode::LessEqual => binary_op!($self, le, "__le__", false, non_mut_heap),
            OpCode::Jump => {
                let offset = $self.read_16bit_number();
                $self.callstack.current_mut().ip += offset;
                Ok(None)
            }
            // Offset to jump backwards is the operand(s)
            OpCode::Loop => {
                let offset = $self.read_16bit_number();
                $self.callstack.current_mut().ip -= offset;
                Ok(None)
            }
            // Get the function with the actual bytecode as a value from the operand
            // Capture the upvalues and push the closure onto the stack
            OpCode::Closure => {
                let value = $self.read_constant(NumberEncoding::Short);
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
                Ok(None)
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
                Ok(None)
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
                Ok(None)
            }
            // CLose the upvalue on top of the stack
            OpCode::CloseUpvalue => {
                $self.close_upvalue($self.stack.len() - 1);
                $self.stack.pop();
                Ok(None)
            }
            // Classname is the operand, create a new class and push it onto the stack
            OpCode::Class => {
                let class_name = $self.read_string("OP_CLASS");
                let class = $self.heap.add_class(Class::new(class_name, false));
                $self.stack_push(class);
                Ok(None)
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
                            Ok(None)
                        } else if $self
                            .bind_method(instance.to_value(&$self.heap).class.into(), field)
                        {
                            // Or could be a method that has to be bound to the
                            // instance so that it can later be called separately.
                            // Just using the side effects
                            Ok(None)
                        } else {
                            let message =
                                format!("Undefined property '{}'.", field.to_value(&$self.heap));
                            $self.throw_attribute_error(&message)
                        }
                    }
                    Value::Module(module) => {
                        if let Some(value) = module.to_value(&$self.heap).globals.get(&field) {
                            $self.stack.pop(); // module
                            $self.stack_push(value.value);
                            Ok(None)
                        } else {
                            let message = format!(
                                "Undefined name '{}' in module {}.",
                                field.to_value(&$self.heap),
                                module.to_value(&$self.heap).name.to_value(&$self.heap)
                            );
                            $self.throw_attribute_error(&message)
                        }
                    }
                    x => {
                        let message = format!(
                            "Tried to get property '{}' of non-instance `{}`.",
                            field.to_value(&$self.heap),
                            x.to_string(&$self.heap)
                        );
                        $self.throw_type_error(&message)
                    }
                }
            }
            // Property to set is the operand, instance is on the stack
            // as is the value to set.
            OpCode::SetProperty => {
                let field_string_id = $self.read_string("SET_PROPERTY");
                let field = field_string_id.to_value(&$self.heap).clone();
                let value = $self.stack.pop().expect("Stack underflow in SET_PROPERTY");
                let mut receiver = $self.stack.pop().expect("Stack underflow in SET_PROPERTY");
                let result = match &mut receiver {
                    Value::Instance(instance) => {
                        instance
                            .to_value_mut(&mut $self.heap)
                            .fields
                            .insert(field, value);
                        Ok(None)
                    }
                    Value::Module(module) => {
                        if let Some(global) = module
                            .to_value_mut(&mut $self.heap)
                            .globals
                            .get_mut(&field_string_id)
                        {
                            if !global.mutable {
                                Err($self
                                    .throw_const_reassignment_error(
                                        "Cannot reassign const variable.",
                                    )
                                    .unwrap_err())
                            } else {
                                global.value = value;
                                Ok(None)
                            }
                        } else {
                            module.to_value_mut(&mut $self.heap).globals.insert(
                                field_string_id,
                                Global {
                                    value,
                                    mutable: true,
                                },
                            );
                            Ok(None)
                        }
                    }
                    x => {
                        let message = format!(
                            "Tried to set property '{}' of non-instance `{}`",
                            field,
                            x.to_string(&$self.heap)
                        );
                        Err($self.throw_type_error(&message).unwrap_err())
                    }
                };
                $self.stack_push(value);
                result
            }
            // Name of the method is the operand, actual method to is on the stack
            // together with the class (... --- Class --- Method)
            OpCode::Method => {
                let method_name = $self.read_string("OP_METHOD");
                $self.define_method(method_name);
                Ok(None)
            }
            // Operands are method name to invoke as well as the number of arguments
            // Stack contains the instance followed by the arguments.
            // (... --- Instance --- arg1 --- arg2 --- ... --- argN)
            OpCode::Invoke => {
                let method_name = $self.read_string("OP_INVOKE");
                let arg_count = $self.read_byte();
                $self.invoke(method_name, arg_count)
            }
            // Stack has (... --- Superclass --- Class)
            OpCode::Inherit => {
                let superclass_id = *$self.peek(1).expect("Stack underflow in OP_INHERIT");
                if let Value::Class(superclass) = &superclass_id {
                    let methods = superclass.to_value(&$self.heap).methods.clone();
                    let subclass = $self
                        .stack
                        .pop()
                        .expect("Stack underflow in OP_INHERIT")
                        .as_class()
                        .to_value_mut(&mut $self.heap);
                    subclass.methods.extend(methods);
                    subclass.superclass = Some(*superclass);
                    Ok(None)
                } else {
                    $self.throw_type_error("Superclass must be a class.")
                }
            }
            // Grab and bind a method from the superclass
            // Operand is the name of the method to get and the stack has the superclass
            OpCode::GetSuper => {
                let method_name = $self.read_string("OP_GET_SUPER");
                let superclass = $self.stack.pop().expect("Stack underflow in OP_GET_SUPER");
                if $self.bind_method(superclass, method_name) {
                    Ok(None)
                } else {
                    let message =
                        format!("Undefined property '{}'.", $self.heap.strings[method_name]);
                    $self.throw_attribute_error(&message)
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
                $self.invoke_from_class(*superclass.as_class(), method_name, arg_count)
            }
            OpCode::BuildList => {
                $self.build_list();
                Ok(None)
            }
            OpCode::BuildTuple => {
                $self.build_tuple();
                Ok(None)
            }
            OpCode::BuildSet => $self.build_set(),
            OpCode::BuildDict => $self.build_dict(),
            OpCode::BuildRangeExclusive => $self.build_range(RangeType::Exclusive),
            OpCode::BuildRangeInclusive => $self.build_range(RangeType::Inclusive),
            OpCode::BuildRational => $self.build_rational(),
            OpCode::BuildFstring => $self.build_fstring(),
            // Import a module by filepath without qualifiers.
            // Expects either the path to the module or the name of
            // a stdlib module as a string as an operand.
            OpCode::Import => {
                let file_path = $self.read_string("OP_IMPORT_AS");
                let local_import = $self.read_byte() == 1;
                $self.import_file(file_path, None, None, local_import)
            }
            // Import a module by filepath with an alias.
            // Name of the module to import is on the stack, alias is the operand.
            OpCode::ImportAs => {
                let file_path = $self.read_string("OP_IMPORT_AS");
                let alias = $self.read_string("OP_IMPORT_AS");
                let local_import = $self.read_byte() == 1;
                $self.import_file(file_path, None, Some(alias), local_import)
            }
            // Import a set of names from a module.
            // Number of names to import are the operand.
            // Module to import them from and the names are on the stack.
            // Items are on the stack in order from left to right
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
                $self.import_file(file_path, names_to_import, None, local_import)
            }
            // Register an exception handler.
            // The operand is the offset from the current instruction right before
            // the try block to the start of the first catch block.
            // The know where to  resume the handler we need the frame we are -> the frames to keep
            // as well as the instruction pointer to jump to in that frame.
            // We also need to know the stack length to be able to remove any
            // left over values on there.
            OpCode::RegisterCatches => {
                let offset = $self.read_16bit_number();
                let target_ip = $self.callstack.current().ip + offset;
                let frames_to_keep = $self.callstack.len();
                let stack_length = $self.stack.len();
                let modules_to_keep = $self.modules.len();
                $self.register_exception_handler(
                    frames_to_keep,
                    target_ip,
                    stack_length,
                    modules_to_keep,
                );
                Ok(None)
            }
            // Just pop the top most handler. No operand, no work with the stack.
            OpCode::PopHandler => {
                $self
                    .pop_exception_handler()
                    .expect("Exception handler unflow in OP_POP_HANDLER");
                Ok(None)
            }
            // Throw the exception on the top of the stack.
            // We pop the exception, unwind to the handler and push the exception again.
            OpCode::Throw => {
                let exception = $self.stack.pop().expect("Stack underflow in OP_THROW.");
                $self.unwind(exception)
            }
            // Layout is Stack Top: [exception_class_to_catch, exception_value_raised]
            OpCode::CompareException => $self.compare_exception(),
            //  We expect either the exception at the stop of the stack that should be reraised
            // or nil if we handled the exception.
            OpCode::Reraise => $self.reraise_exception(),
        }
    }};
}
