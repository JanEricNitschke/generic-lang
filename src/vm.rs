use std::collections::VecDeque;
use std::pin::Pin;

use rustc_hash::FxHashMap as HashMap;

use crate::chunk::InstructionDisassembler;
use crate::heap::{FunctionId, ValueId};
use crate::native_functions::Natives;
use crate::value::{Class, Closure, Instance, List, Number, Upvalue};
use crate::{
    chunk::{CodeOffset, OpCode},
    compiler::Compiler,
    config,
    heap::{Heap, StringId},
    scanner::Scanner,
    value::{NativeFunction, NativeFunctionImpl, NativeMethod, NativeMethodImpl, Value},
};

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

macro_rules! runtime_error {
    ($self:ident, $($arg:expr),* $(,)?) => {
        eprintln!($($arg),*);
        for frame in $self.callstack.iter().rev() {
            let line = frame.closure().function.chunk.get_line(CodeOffset(frame.ip - 1));
            eprintln!("[line {}] in {}", *line, *frame.closure().function.name);
        }
    };
}

macro_rules! binary_op {
    ($self:ident, $op:tt, $intonly:tt) => {
        if !$self.binary_op(|a, b| a $op b, $intonly) {
            return InterpretResult::RuntimeError;
        }
    }
}

type BinaryOp<T> = fn(Number, Number) -> T;

struct Global {
    value: ValueId,
    mutable: bool,
}

pub struct CallFrame {
    closure: ValueId,
    ip: usize,
    stack_base: usize,
}

impl CallFrame {
    pub fn closure(&self) -> &Closure {
        (*self.closure).as_closure()
    }
}

struct CallStack {
    frames: Vec<CallFrame>,
    current_closure: Option<ValueId>,
    current_function: Option<FunctionId>,
}

impl CallStack {
    #[must_use]
    fn new() -> Self {
        Self {
            frames: Vec::with_capacity(crate::config::FRAMES_MAX),
            current_closure: None,
            current_function: None,
        }
    }

    fn iter(&self) -> std::slice::Iter<CallFrame> {
        self.frames.iter()
    }

    fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    fn pop(&mut self) -> Option<CallFrame> {
        let retval = self.frames.pop();
        self.current_closure = self.frames.last().map(|f| f.closure);
        self.current_function = self.current_closure.map(|c| c.as_closure().function);
        retval
    }

    fn push(&mut self, closure: ValueId, stack_base: usize) {
        self.frames.push(CallFrame {
            closure,
            ip: 0,
            stack_base,
        });
        self.current_closure = Some(closure);
        self.current_function = Some(closure.as_closure().function);
    }

    fn current_mut(&mut self) -> &mut CallFrame {
        let i = self.frames.len() - 1;
        &mut self.frames[i]
    }

    fn current(&self) -> &CallFrame {
        let i = self.frames.len() - 1;
        &self.frames[i]
    }

    fn code_byte(&self, index: usize) -> u8 {
        self.current_function.unwrap().chunk.code()[index]
    }

    fn closure(&self) -> ValueId {
        self.current_closure.unwrap()
    }

    fn function(&self) -> FunctionId {
        self.current_function.unwrap()
    }

    fn len(&self) -> usize {
        self.frames.len()
    }
}

pub struct VM {
    heap: Pin<Box<Heap>>,
    callstack: CallStack,
    stack: Vec<ValueId>,
    globals: HashMap<StringId, Global>,
    open_upvalues: VecDeque<ValueId>,
}

impl VM {
    #[must_use]
    pub fn new() -> Self {
        Self {
            heap: Heap::new(),
            callstack: CallStack::new(),
            stack: Vec::with_capacity(crate::config::STACK_MAX),
            globals: HashMap::default(),
            open_upvalues: VecDeque::new(),
        }
    }

    pub fn init_string(&self) -> StringId {
        self.heap.builtin_constants().init_string
    }

    pub fn interpret(&mut self, source: &[u8]) -> InterpretResult {
        let scanner = Scanner::new(source);
        let mut natives = Natives::new();
        natives.create_names(&mut self.heap);
        let mut compiler = Compiler::new(scanner, &mut self.heap);
        natives.register_names(&mut compiler);

        let result = if let Some(function) = compiler.compile() {
            natives.define_natives(self);
            let function_id = self.heap.add_function(function);
            let closure = Value::closure(function_id);
            let value_id = self.heap.add_value(closure);
            self.stack_push(value_id);
            self.execute_call(value_id, 0);
            self.run()
        } else {
            InterpretResult::CompileError
        };

        if result == InterpretResult::Ok {
            assert_eq!(self.stack.len(), 0);
        }
        result
    }

    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    fn run(&mut self) -> InterpretResult {
        let trace_execution = config::TRACE_EXECUTION.load();
        let stress_gc = config::STRESS_GC.load();
        loop {
            if trace_execution {
                let function = &self.callstack.function();
                let mut disassembler = InstructionDisassembler::new(&function.chunk);
                *disassembler.offset = self.callstack.current().ip;
                println!(
                    "          [ { } ]",
                    self.stack
                        .iter()
                        .map(|v| format!("{}", self.heap.values[v]))
                        .collect::<Vec<_>>()
                        .join(" ][ ")
                );
                print!("{disassembler:?}");
            }
            self.collect_garbage(stress_gc);
            match OpCode::try_from(self.read_byte()).expect("Internal error: unrecognized opcode") {
                OpCode::Pop => {
                    self.stack.pop().expect("Stack underflow in OP_POP.");
                }
                OpCode::Dup => {
                    self.stack_push_value(
                        self.heap.values[self.peek(0).expect("stack underflow in OP_DUP")].clone(),
                    );
                }
                OpCode::DupN => {
                    // -1 because Dup1 should peek at the top most elemnt
                    let depth = usize::from(self.read_byte()) - 1;
                    for _ in (0..=depth).rev() {
                        // Always look at depth because each iteration pushes an
                        // additional item onto the stack.
                        // So for N = 2
                        // 1 2 3 4 (depth = 1) -> grab 3
                        // 1 2 3 4 3 (again depth = 1) -> grab 4
                        // 1 2 3 4 3 4
                        self.stack_push_value(
                            self.heap.values[self.peek(depth).expect("stack underflow in OP_DUP")]
                                .clone(),
                        );
                    }
                }
                op @ (OpCode::GetLocal | OpCode::GetLocalLong) => self.get_local(op),
                op @ (OpCode::SetLocal | OpCode::SetLocalLong) => self.set_local(op),
                op @ (OpCode::GetGlobal | OpCode::GetGlobalLong) => {
                    if let Some(value) = self.get_global(op) {
                        return value;
                    }
                }
                op @ (OpCode::SetGlobal | OpCode::SetGlobalLong) => {
                    if let Some(value) = self.set_global(op) {
                        return value;
                    }
                }
                op @ (OpCode::DefineGlobal
                | OpCode::DefineGlobalLong
                | OpCode::DefineGlobalConst
                | OpCode::DefineGlobalConstLong) => self.define_global(op),
                OpCode::JumpIfFalse => self.jump_conditional(false),
                OpCode::JumpIfTrue => self.jump_conditional(true),
                OpCode::Call => {
                    if let Some(value) = self.call() {
                        return value;
                    }
                }
                OpCode::Return => {
                    if let Some(value) = self.return_() {
                        return value;
                    }
                }
                OpCode::Constant => {
                    let value = self.read_constant(false);
                    self.stack_push(value);
                }
                OpCode::ConstantLong => {
                    let value = self.read_constant(true);
                    self.stack_push(value);
                }
                OpCode::Negate => {
                    if let Some(value) = self.negate() {
                        return value;
                    }
                }
                OpCode::Not => self.not_(),
                OpCode::Nil => self.stack_push(self.heap.builtin_constants().nil),
                OpCode::True => self.stack_push(self.heap.builtin_constants().true_),
                OpCode::False => self.stack_push(self.heap.builtin_constants().false_),
                OpCode::Equal => self.equal(),
                OpCode::Add => {
                    if let Some(value) = self.add() {
                        return value;
                    }
                }
                OpCode::Subtract => binary_op!(self, -, false),
                OpCode::Multiply => binary_op!(self, *, false),
                OpCode::Divide => binary_op!(self, /, false),
                OpCode::BitXor => binary_op!(self, ^, true),
                OpCode::BitAnd => binary_op!(self, &, true),
                OpCode::BitOr => binary_op!(self, |, true),
                OpCode::Exp => {
                    if let Some(value) = self.exponatiate() {
                        return value;
                    }
                }
                OpCode::Mod => binary_op!(self, %, false),
                OpCode::FloorDiv => {
                    if let Some(value) = self.floor_div() {
                        return value;
                    }
                }
                OpCode::Greater => binary_op!(self, >, false),
                OpCode::Less => binary_op!(self, <, false),
                OpCode::Jump => {
                    let offset = self.read_16bit_number();
                    self.callstack.current_mut().ip += offset;
                }
                OpCode::Loop => {
                    let offset = self.read_16bit_number();
                    self.callstack.current_mut().ip -= offset;
                }
                OpCode::Closure => {
                    let value = self.read_constant(false);
                    let function = value.as_function();
                    let mut closure = Closure::new(*function);

                    for _ in 0..closure.upvalue_count {
                        let is_local = self.read_byte();
                        debug_assert!(
                            is_local == 0 || is_local == 1,
                            "'is_local` must be 0 or 1, got {is_local}"
                        );
                        let is_local = is_local == 1;

                        let index = usize::from(self.read_byte());
                        if is_local {
                            closure.upvalues.push(self.capture_upvalue(index));
                        } else {
                            closure
                                .upvalues
                                .push((*self.callstack.closure()).as_closure().upvalues[index]);
                        }
                    }
                    let closure_id = self.heap.add_value(Value::from(closure));
                    self.stack_push(closure_id);
                }
                OpCode::GetUpvalue => {
                    let upvalue_index = usize::from(self.read_byte());
                    let closure_value = &*self.callstack.closure();
                    let closure = closure_value.as_closure();
                    let upvalue_location = closure.upvalues[upvalue_index].upvalue_location();
                    match *upvalue_location {
                        Upvalue::Open(absolute_local_index) => {
                            self.stack_push(self.stack[absolute_local_index]);
                        }
                        Upvalue::Closed(value_id) => self.stack_push(value_id),
                    }
                }
                OpCode::SetUpvalue => {
                    let upvalue_index = usize::from(self.read_byte());
                    let closure = self.callstack.closure();
                    let upvalue_location =
                        closure.as_closure().upvalues[upvalue_index].upvalue_location();
                    let new_value = self
                        .stack
                        .last()
                        .map(|x| self.heap.values[x].clone())
                        .expect("Stack underflow in OP_SET_UPVALUE");
                    match upvalue_location {
                        Upvalue::Open(absolute_local_index) => {
                            *self.stack[*absolute_local_index] = new_value;
                        }
                        Upvalue::Closed(mut value_id) => {
                            *value_id = new_value;
                        }
                    }
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalue(self.stack.len() - 1);
                    self.stack.pop();
                }
                OpCode::Class => {
                    let class_name = self.read_string("OP_CLASS");
                    let class = Class::new(class_name, false);
                    self.stack_push_value(class.into());
                }
                OpCode::GetProperty => {
                    let field = self.read_string("GET_PROPERTY");

                    // Probaby better to just grab the class and ask if it is native
                    match &self.heap.values[self.peek(0).expect("Stack underflow in GET_PROPERTY")]
                    {
                        Value::Instance(instance) => {
                            if let Some(value) = instance.fields.get(&self.heap.strings[&field]) {
                                self.stack.pop(); // instance
                                self.stack_push(*value);
                            } else if self.bind_method(instance.class, field) {
                                // Just using the side effects
                            } else {
                                runtime_error!(self, "Undefined property '{}'.", *field);
                                return InterpretResult::RuntimeError;
                            }
                        }
                        Value::List(list) => {
                            if self.bind_method(list.class, field) {
                                // Just using the side effects
                            } else {
                                runtime_error!(self, "Undefined property '{}'.", *field);
                                return InterpretResult::RuntimeError;
                            }
                        }
                        x => {
                            runtime_error!(
                                self,
                                "Tried to get property '{}' of non-instance `{}`.",
                                *field,
                                x
                            );
                            return InterpretResult::RuntimeError;
                        }
                    };
                }
                OpCode::SetProperty => {
                    let field_string_id = self.read_string("SET_PROPERTY");
                    let field = &self.heap.strings[&field_string_id];
                    match &self.heap.values[self.peek(1).expect("Stack underflow in SET_PROPERTY")]
                    {
                        Value::Instance(instance) => instance,
                        x => {
                            runtime_error!(
                                self,
                                "Tried to set propery '{}' of non-instance `{}`",
                                field,
                                x
                            );
                            return InterpretResult::RuntimeError;
                        }
                    };
                    let value = self.stack.pop().expect("Stack underflow in SET_PROPERTY");
                    let mut instance = self.stack.pop().expect("Stack underflow in SET_PROPERTY");
                    instance
                        .as_instance_mut()
                        .fields
                        .insert(field.to_string(), value);
                    self.stack_push(value);
                }
                OpCode::Method => {
                    let method_name = self.read_string("OP_METHOD");
                    self.define_method(method_name);
                }
                OpCode::Invoke => {
                    let method_name = self.read_string("OP_INVOKE");
                    let arg_count = self.read_byte();
                    if !self.invoke(method_name, arg_count) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Inherit => {
                    let superclass_id = self.peek(1).expect("Stack underflow in OP_INHERIT");
                    let superclass =
                        if let Value::Class(superclass) = &self.heap.values[superclass_id] {
                            if superclass.is_native {
                                runtime_error!(self, "Can not inherit from native classes yet.");
                                return InterpretResult::RuntimeError;
                            }
                            superclass
                        } else {
                            runtime_error!(self, "Superclass must be a class.");
                            return InterpretResult::RuntimeError;
                        };
                    let methods = superclass.methods.clone();
                    let mut subclass = self.stack.pop().expect("Stack underflow in OP_INHERIT");
                    subclass.as_class_mut().methods.extend(methods);
                }
                OpCode::GetSuper => {
                    let method_name = self.read_string("OP_GET_SUPER");
                    let superclass = self.stack.pop().expect("Stack underflow in OP_GET_SUPER");
                    if !self.bind_method(superclass, method_name) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::SuperInvoke => {
                    let method_name = self.read_string("OP_SUPER_INVOKE");
                    let arg_count = self.read_byte();
                    let superclass = self
                        .stack
                        .pop()
                        .expect("Stack underflow in OP_SUPER_INVOKE");
                    if !self.invoke_from_class(superclass, method_name, arg_count) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::BuildList => {
                    let mut list = List::new(*self.heap.native_classes.get("List").unwrap());

                    let arg_count = self.read_byte();
                    for index in (0..arg_count).rev() {
                        list.items.push(*self.peek(index as usize).unwrap());
                    }
                    for _ in 0..arg_count {
                        self.stack.pop();
                    }
                    self.stack_push_value(list.into());
                }
                OpCode::IndexSubscript => {
                    if let Some(value) = self.index_subscript() {
                        return value;
                    }
                }
                OpCode::StoreSubscript => {
                    if let Some(value) = self.store_subscript() {
                        return value;
                    }
                }
            };
        }
    }

    fn peek(&self, n: usize) -> Option<&ValueId> {
        let len = self.stack.len();
        if n >= len {
            None
        } else {
            Some(&self.stack[len - n - 1])
        }
    }

    fn peek_mut(&mut self, n: usize) -> Option<&mut ValueId> {
        let len = self.stack.len();
        if n >= len {
            None
        } else {
            Some(&mut self.stack[len - n - 1])
        }
    }

    fn read_string(&mut self, opcode_name: &str) -> StringId {
        let constant = self.read_constant(false);
        match &self.heap.values[&constant] {
            Value::String(string_id) => *string_id,
            x => {
                panic!("Non-string method name to {opcode_name}: `{x}`");
            }
        }
    }

    fn binary_op<T: Into<Value>>(&mut self, op: BinaryOp<T>, int_only: bool) -> bool {
        let slice_start = self.stack.len() - 2;

        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) =
                    (&self.heap.values[left], &self.heap.values[right])
                {
                    if int_only
                        && (!matches!(a, Number::Integer(_)) | !matches!(b, Number::Integer(_)))
                    {
                        false
                    } else {
                        let value = op(*a, *b).into();
                        self.stack.pop();
                        self.stack.pop();
                        self.stack_push_value(value);
                        true
                    }
                } else {
                    false
                }
            }
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be {}. Got: [{}]",
                if int_only { "integers" } else { "numbers" },
                self.stack[slice_start..]
                    .iter()
                    .map(|v| format!("{}", self.heap.values[v]))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }
        ok
    }

    fn add(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        let ok = match &self.stack[slice_start..] {
            [left, right] => match (&self.heap.values[left], &self.heap.values[right]) {
                (Value::Number(a), Value::Number(b)) => {
                    let value = (*a + *b).into();
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(value);
                    true
                }
                (Value::String(a), Value::String(b)) => {
                    // This could be optimized by allowing mutations via the heap
                    let new_string = format!("{}{}", self.heap.strings[a], self.heap.strings[b]);
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
                    .map(|v| format!("{}", self.heap.values[v]))
                    .collect::<Vec<_>>()
                    .join(", ")
            );

            return Some(InterpretResult::RuntimeError);
        }
        None
    }

    fn exponatiate(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;

        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) =
                    (&self.heap.values[left], &self.heap.values[right])
                {
                    let value = a.pow(*b).into();
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(value);
                    true
                } else {
                    false
                }
            }
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be numbers. Got: [{}]",
                self.stack[slice_start..]
                    .iter()
                    .map(|v| format!("{}", self.heap.values[v]))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            return Some(InterpretResult::RuntimeError);
        }
        None
    }

    fn floor_div(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;

        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) =
                    (&self.heap.values[left], &self.heap.values[right])
                {
                    let value = a.floor_div(*b).into();
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(value);
                    true
                } else {
                    false
                }
            }
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be numbers. Got: [{}]",
                self.stack[slice_start..]
                    .iter()
                    .map(|v| format!("{}", self.heap.values[v]))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            return Some(InterpretResult::RuntimeError);
        }
        None
    }

    fn index_subscript(&mut self) -> Option<InterpretResult> {
        let index = match &self.heap.values[&self
            .stack
            .pop()
            .expect("Stack underflow in OP_INDEX_SUBSCRIPT")]
        {
            Value::Number(Number::Integer(n)) => {
                if let Ok(value) = usize::try_from(*n) {
                    value
                } else {
                    runtime_error!(
                        self,
                        "Can not index into list with negative or too large numbers, got `{}`.",
                        n
                    );
                    return Some(InterpretResult::RuntimeError);
                }
            }

            x => {
                runtime_error!(self, "Can only index into list with integer, got `{}`.", x);
                return Some(InterpretResult::RuntimeError);
            }
        };

        let list = match &self.heap.values[&self
            .stack
            .pop()
            .expect("Stack underflow in OP_INDEX_SUBSCRIPT")]
        {
            Value::List(list) => list,
            x => {
                runtime_error!(self, "Can only index into lists, got `{}`.", x);
                return Some(InterpretResult::RuntimeError);
            }
        };
        let Some(value) = list.items.get(index) else {
            runtime_error!(
                self,
                "Index `{}` is out of bounds of list  with len `{}`.",
                index,
                list.items.len()
            );
            return Some(InterpretResult::RuntimeError);
        };

        self.stack_push(*value);
        None
    }

    fn store_subscript(&mut self) -> Option<InterpretResult> {
        let item = self
            .stack
            .pop()
            .expect("Stack underflow in OP_STORE_SUBSCRIPT");
        let index = match &self.heap.values[&self
            .stack
            .pop()
            .expect("Stack underflow in OP_STORE_SUBSCRIPT")]
        {
            Value::Number(Number::Integer(n)) => {
                if let Ok(value) = usize::try_from(*n) {
                    value
                } else {
                    runtime_error!(
                        self,
                        "Can not index into list with negative or too large numbers, got `{}`.",
                        n
                    );
                    return Some(InterpretResult::RuntimeError);
                }
            }

            x => {
                runtime_error!(self, "Can only index into list with integer, got `{}`.", x);
                return Some(InterpretResult::RuntimeError);
            }
        };
        let list = match &mut self.heap.values[&self
            .stack
            .pop()
            .expect("Stack underflow in OP_INDEX_SUBSCRIPT")]
        {
            Value::List(list) => list,
            x => {
                runtime_error!(self, "Can only index into lists, got `{}`.", x);
                return Some(InterpretResult::RuntimeError);
            }
        };

        if let Some(elem) = list.items.get_mut(index) {
            *elem = item;
        } else {
            runtime_error!(
                self,
                "Index `{}` is out of bounds of list  with len `{}`.",
                index,
                list.items.len()
            );
            return Some(InterpretResult::RuntimeError);
        }
        self.stack_push(item);
        None
    }

    fn set_local(&mut self, op: OpCode) {
        let slot = if op == OpCode::GetLocalLong {
            self.read_24bit_number()
        } else {
            usize::from(self.read_byte())
        };
        *self.stack_get_mut(slot) = *self.peek(0).expect("stack underflow in OP_SET_LOCAL");
    }

    fn get_local(&mut self, op: OpCode) {
        let slot = if op == OpCode::GetLocalLong {
            self.read_24bit_number()
        } else {
            usize::from(self.read_byte())
        };
        self.stack_push(*self.stack_get(slot));
    }

    fn get_global(&mut self, op: OpCode) -> Option<InterpretResult> {
        let constant_index = self.read_constant_index(op == OpCode::GetGlobalLong);
        let constant_value = self.read_constant_value(constant_index);
        match &self.heap.values[&constant_value] {
            Value::String(name) => {
                if let Some(global) = self.globals.get(name) {
                    self.stack_push(global.value);
                } else {
                    runtime_error!(self, "Undefined variable '{}'.", self.heap.strings[name]);
                    return Some(InterpretResult::RuntimeError);
                }
            }

            x => panic!("Internal error: non-string operand to {op:?}: {x:?}"),
        }
        None
    }

    fn set_global(&mut self, op: OpCode) -> Option<InterpretResult> {
        let constant_index = self.read_constant_index(op == OpCode::SetGlobalLong);
        let constant_value = self.read_constant_value(constant_index);
        let name = match &self.heap.values[&constant_value] {
            Value::String(name) => *name,
            x => panic!("Internal error: non-string operand to OP_SET_GLOBAL: {x:?}"),
        };

        if let Some(global) = self.globals.get_mut(&name) {
            if !global.mutable {
                runtime_error!(self, "Reassignment to global 'const'.");
                return Some(InterpretResult::RuntimeError);
            }
            global.value = *self
                .stack
                .last()
                .unwrap_or_else(|| panic!("stack underflow in {op:?}"));
        } else {
            runtime_error!(self, "Undefined variable '{}'.", *name);
            return Some(InterpretResult::RuntimeError);
        }

        None
    }

    fn define_global(&mut self, op: OpCode) {
        let constant = self.read_constant(op == OpCode::DefineGlobalLong);
        match &self.heap.values[&constant] {
            Value::String(name) => {
                let name = *name;
                self.globals.insert(
                    name,
                    Global {
                        value: *self
                            .stack
                            .last()
                            .unwrap_or_else(|| panic!("stack underflow in {op:?}")),
                        mutable: op != OpCode::DefineGlobalConst
                            && op != OpCode::DefineGlobalConstLong,
                    },
                );
                self.stack.pop();
            }
            x => panic!("Internal error: non-string operand to {op:?}: {x:?}"),
        }
    }

    fn jump_conditional(&mut self, if_true: bool) {
        let offset = self.read_16bit_number();
        // if_true = True -> jump_if_true
        // -> ! (is_falsey())
        // if_true - is_falsey() ->:
        // true ^ false = true
        // true ^ true = false
        // if_true = False -> jump_if_false
        // -> is_falsey
        // if_true - is_falsey() ->:
        // false ^ true = true
        // false ^ false = false
        if self
            .stack
            .last()
            .expect("stack underflow in OP_JUMP_IF_FALSE")
            .is_falsey()
            ^ if_true
        {
            self.callstack.current_mut().ip += offset;
        }
    }

    fn call(&mut self) -> Option<InterpretResult> {
        let arg_count = self.read_byte();
        let callee = self.stack[self.stack.len() - 1 - usize::from(arg_count)];
        if !self.call_value(callee, arg_count) {
            return Some(InterpretResult::RuntimeError);
        }
        None
    }

    fn define_method(&mut self, method_name: StringId) {
        let method = *self.peek(0).expect("Stack underflow in OP_METHOD");
        let class = self
            .peek_mut(1)
            .expect("Stack underflow in OP_METHOD")
            .as_class_mut();
        class.methods.insert(method_name, method);
        self.stack.pop();
    }

    fn return_(&mut self) -> Option<InterpretResult> {
        let result = self.stack.pop();
        let frame = self
            .callstack
            .pop()
            .expect("Call stack underflow in OP_RETURN");
        if self.callstack.is_empty() {
            self.stack.pop();
            return Some(InterpretResult::Ok);
        }

        self.close_upvalue(frame.stack_base);
        self.stack.truncate(frame.stack_base);
        self.stack_push(result.expect("Stack underflow in OP_RETURN"));
        None
    }

    fn negate(&mut self) -> Option<InterpretResult> {
        let value_id = *self.peek(0).expect("stack underflow in OP_NEGATE");
        let value = &self.heap.values[&value_id];
        if let Value::Number(n) = value {
            self.stack.pop();
            self.stack_push_value((-*n).into());
        } else {
            runtime_error!(self, "Operand must be a number.");
            return Some(InterpretResult::RuntimeError);
        }
        None
    }

    fn not_(&mut self) {
        let value = self
            .stack
            .pop()
            .expect("Stack underflow in OP_NOT.")
            .is_falsey();
        self.stack_push(self.heap.builtin_constants().bool(value));
    }

    fn equal(&mut self) {
        let left_id = self
            .stack
            .pop()
            .expect("stack underflow in OP_EQUAL (first)");
        let right_id = self
            .stack
            .pop()
            .expect("stack underflow in OP_EQUAL (second)");
        let left = &self.heap.values[&left_id];
        let right = &self.heap.values[&right_id];

        // There is one case where equality-by-reference does not imply *actual* equality: NaN
        let value = match (left, right) {
            (Value::Number(Number::Float(left)), Value::Number(Number::Float(right)))
                if left.is_nan() && right.is_nan() =>
            {
                false
            }
            (left, right) => left_id == right_id || left == right,
        };

        self.stack_push(self.heap.builtin_constants().bool(value));
    }

    fn read_byte(&mut self) -> u8 {
        let frame = self.callstack.current_mut();
        let index = frame.ip;
        frame.ip += 1;
        self.callstack.code_byte(index)
    }

    fn read_24bit_number(&mut self) -> usize {
        (usize::from(self.read_byte()) << 16)
            + (usize::from(self.read_byte()) << 8)
            + (usize::from(self.read_byte()))
    }

    fn read_16bit_number(&mut self) -> usize {
        (usize::from(self.read_byte()) << 8) + (usize::from(self.read_byte()))
    }

    fn read_constant_index(&mut self, long: bool) -> usize {
        if long {
            self.read_24bit_number()
        } else {
            usize::from(self.read_byte())
        }
    }

    fn read_constant_value(&self, index: usize) -> ValueId {
        *self.callstack.function().chunk.get_constant(index)
    }

    fn read_constant(&mut self, long: bool) -> ValueId {
        let index = self.read_constant_index(long);
        self.read_constant_value(index)
    }

    #[inline]
    fn stack_push(&mut self, value_id: ValueId) {
        self.stack.push(value_id);
        // This check has a pretty big performance overhead; disabled for now
        // TODO find a better way: keep the check and minimize overhead
        /*
        if self.stack.len() > STACK_MAX {
            runtime_error!(self, "Stack overflow");
        }
        */
    }

    #[inline]
    fn stack_push_value(&mut self, value: Value) {
        let value_id = match value {
            Value::Bool(bool) => self.heap.builtin_constants().bool(bool),
            Value::Nil => self.heap.builtin_constants().nil,
            Value::Number(n) => self
                .heap
                .builtin_constants()
                .number(n)
                .unwrap_or_else(|| self.heap.add_value(value)),
            value => self.heap.add_value(value),
        };
        self.stack.push(value_id);
    }

    fn stack_get(&self, slot: usize) -> &ValueId {
        &self.stack[self.stack_base() + slot]
    }

    fn stack_get_mut(&mut self, slot: usize) -> &mut ValueId {
        let offset = self.stack_base();
        &mut self.stack[offset + slot]
    }

    fn stack_base(&self) -> usize {
        self.callstack.current().stack_base
    }

    fn call_value(&mut self, callee: ValueId, arg_count: u8) -> bool {
        match &*callee {
            Value::NativeMethod(_) => {
                println!("Got a native method");
                false
            }
            Value::Closure(_) => self.execute_call(callee, arg_count),
            Value::NativeFunction(f) => self.execute_native_function_call(f, arg_count),
            Value::Class(class) => {
                let is_native = class.is_native;
                let maybe_initializer = class
                    .methods
                    .get(&self.heap.builtin_constants().init_string);
                let instance_id: ValueId = self.heap.add_value(Instance::new(callee).into());
                let stack_index = self.stack.len() - usize::from(arg_count) - 1;
                self.stack[stack_index] = instance_id;
                if let Some(initializer) = maybe_initializer {
                    if is_native {
                        self.execute_native_method_call(
                            initializer.as_native_method(),
                            instance_id,
                            arg_count,
                        )
                    } else {
                        self.execute_call(*initializer, arg_count)
                    }
                } else if arg_count != 0 {
                    runtime_error!(self, "Expected 0 arguments but got {arg_count}.");
                    false
                } else {
                    true
                }
            }
            Value::BoundMethod(bound_method) => match &*bound_method.method {
                Value::Closure(_) => {
                    let new_stack_base = self.stack.len() - usize::from(arg_count) - 1;
                    self.stack[new_stack_base] = bound_method.receiver;
                    self.execute_call(bound_method.method, arg_count)
                }
                Value::NativeMethod(native_method) => {
                    self.execute_native_method_call(native_method, bound_method.receiver, arg_count)
                }
                _ => {
                    runtime_error!(
                        self,
                        "Native methods only bind over  closures or native methods."
                    );
                    false
                }
            },
            _ => {
                runtime_error!(self, "Can only call functions and classes.");
                false
            }
        }
    }

    #[allow(clippy::branches_sharing_code)]
    fn execute_native_method_call(
        &mut self,
        f: &NativeMethod,
        receiver: ValueId,
        arg_count: u8,
    ) -> bool {
        let arity = f.arity;
        if !arity.contains(&arg_count) {
            if arity.len() == 1 {
                runtime_error!(
                    self,
                    "Native method '{}' of class {} expected {} argument{}, got {}.",
                    *f.name,
                    *receiver.class_name(),
                    arity[0],
                    {
                        if arity[0] == 1 {
                            ""
                        } else {
                            "s"
                        }
                    },
                    arg_count
                );
            } else {
                runtime_error!(
                    self,
                    "Native method '{}' of class {} expected any of {:?} arguments, got {}.",
                    *f.name,
                    *receiver.class_name(),
                    arity,
                    arg_count
                );
            };
            return false;
        }
        let fun = f.fun;
        let start_index = self.stack.len() - usize::from(arg_count);
        let args = self.stack[start_index..].iter().collect::<Vec<_>>();
        match fun(&mut self.heap, &receiver, &args) {
            Ok(value) => {
                self.stack
                    .truncate(self.stack.len() - usize::from(arg_count) - 1);
                self.stack_push(value);
                true
            }
            Err(e) => {
                runtime_error!(self, "{}", e);
                false
            }
        }
    }

    #[allow(clippy::branches_sharing_code)]
    fn execute_native_function_call(&mut self, f: &NativeFunction, arg_count: u8) -> bool {
        let arity = f.arity;
        if !arity.contains(&arg_count) {
            if arity.len() == 1 {
                runtime_error!(
                    self,
                    "Native function '{}' expected {} argument{}, got {}.",
                    *f.name,
                    arity[0],
                    {
                        if arity[0] == 1 {
                            ""
                        } else {
                            "s"
                        }
                    },
                    arg_count
                );
            } else {
                runtime_error!(
                    self,
                    "Native function '{}' expected any of {:?} arguments, got {}.",
                    *f.name,
                    arity,
                    arg_count
                );
            };
            return false;
        }
        let fun = f.fun;
        let start_index = self.stack.len() - usize::from(arg_count);
        let args = self.stack[start_index..].iter().collect::<Vec<_>>();
        match fun(&mut self.heap, &args) {
            Ok(value) => {
                self.stack
                    .truncate(self.stack.len() - usize::from(arg_count) - 1);
                self.stack_push(value);
                true
            }
            Err(e) => {
                runtime_error!(self, "{}", e);
                false
            }
        }
    }

    fn invoke_from_class(&mut self, class: ValueId, method_name: StringId, arg_count: u8) -> bool {
        let Some(method) = class.as_class().methods.get(&method_name) else {
            runtime_error!(self, "Undefined property '{}'.", self.heap.strings[&method_name]);
            return false;
        };
        match &**method {
            Value::Closure(_) => self.execute_call(*method, arg_count),
            Value::NativeMethod(native) => {
                let receiver = *self.peek(arg_count as usize).unwrap();
                self.execute_native_method_call(native, receiver, arg_count)
            }
            x => unreachable!(
                "Can only invoke closure or native methods. Got `{}` instead.",
                x
            ),
        }
    }

    fn invoke(&mut self, method_name: StringId, arg_count: u8) -> bool {
        let receiver = self
            .peek(arg_count.into())
            .expect("Stack underflow in OP_INVOKE");
        if let Value::Instance(instance) = &self.heap.values[receiver] {
            if let Some(value) = instance.fields.get(&self.heap.strings[&method_name]) {
                let new_stack_base = self.stack.len() - usize::from(arg_count) - 1;
                self.stack[new_stack_base] = *value;
                self.call_value(*value, arg_count)
            } else {
                self.invoke_from_class(instance.class, method_name, arg_count)
            }
        } else if let Value::List(list) = &self.heap.values[receiver] {
            self.invoke_from_class(list.class, method_name, arg_count)
        } else {
            runtime_error!(self, "Only instances have methods.");
            false
        }
    }

    fn bind_method(&mut self, class: ValueId, name: StringId) -> bool {
        let class = class.as_class();
        let Some(method) = class.methods.get(&name) else {return false};
        let bound_method = Value::bound_method(
            *self.peek(0).expect("Buffer underflow in OP_METHOD"),
            *method,
        );
        self.stack.pop();
        self.stack_push_value(bound_method);
        true
    }

    fn capture_upvalue(&mut self, local: usize) -> ValueId {
        let local = self.callstack.current().stack_base + local;
        let mut upvalue_index = 0;
        let mut upvalue = None;

        for (i, this) in self.open_upvalues.iter().enumerate() {
            if this.upvalue_location().as_open() <= local {
                break;
            }
            upvalue = Some(this);
            upvalue_index = i;
        }

        if let Some(upvalue) = upvalue {
            if upvalue.upvalue_location().as_open() == local {
                return *upvalue;
            }
        }

        let upvalue = Value::Upvalue(Upvalue::Open(local));
        let upvalue_id = self.heap.add_value(upvalue);
        self.open_upvalues.insert(upvalue_index, upvalue_id);

        upvalue_id
    }

    fn close_upvalue(&mut self, last: usize) {
        while self
            .open_upvalues
            .get(0)
            .map_or(false, |v| v.upvalue_location().as_open() >= last)
        {
            let mut upvalue = self.open_upvalues.pop_front().unwrap();
            debug_assert!(matches!(*upvalue, Value::Upvalue(_)));

            let pointed_value = self.stack[upvalue.upvalue_location().as_open()];
            *upvalue.upvalue_location_mut() = Upvalue::Closed(pointed_value);
        }
    }

    fn execute_call(&mut self, closure: ValueId, arg_count: u8) -> bool {
        let arity = closure.as_closure().function.arity;
        let arg_count = usize::from(arg_count);
        if arg_count != arity {
            runtime_error!(
                self,
                "Expected {} argument{} but got {}.",
                arity,
                {
                    if arity == 1 {
                        ""
                    } else {
                        "s"
                    }
                },
                arg_count
            );
            return false;
        }

        if self.callstack.len() == crate::config::FRAMES_MAX {
            runtime_error!(self, "Stack overflow.");
            return false;
        }

        debug_assert!(
            matches!(*closure, Value::Closure(_)),
            "`execute_call` must be called with a `Closure`, got: {}",
            *closure
        );

        self.callstack
            .push(closure, self.stack.len() - arg_count - 1);
        true
    }

    pub fn define_native_function(
        &mut self,
        name: StringId,
        arity: &'static [u8],
        fun: NativeFunctionImpl,
    ) {
        let value = Value::NativeFunction(NativeFunction { name, arity, fun });
        let value_id = self.heap.add_value(value);
        self.globals.insert(
            name,
            Global {
                value: value_id,
                mutable: true,
            },
        );
    }

    pub fn define_native_class(&mut self, name_id: StringId) {
        let value = Value::Class(Class::new(name_id, true));
        let value_id = self.heap.add_value(value);
        self.globals.insert(
            name_id,
            Global {
                value: value_id,
                mutable: true,
            },
        );
        self.heap
            .native_classes
            .insert(name_id.to_string(), value_id);
    }

    pub fn define_native_method(
        &mut self,
        class: StringId,
        name: StringId,
        arity: &'static [u8],
        fun: NativeMethodImpl,
    ) {
        let value = Value::NativeMethod(NativeMethod {
            class,
            name,
            arity,
            fun,
        });
        let value_id = self.heap.add_value(value);
        let target_class = self
            .heap
            .native_classes
            .get_mut(&*class)
            .unwrap()
            .as_class_mut();
        target_class.methods.insert(name, value_id);
    }

    fn collect_garbage(&mut self, stress_gc: bool) {
        if !stress_gc && !self.heap.needs_gc() {
            return;
        }
        let black_value = self.heap.black_value;

        self.heap.gc_start();

        // Mark roots
        for value in &self.stack {
            self.heap.mark_value(value);
        }
        for (key, value) in &self.globals {
            self.heap.mark_string(key);
            self.heap.mark_value(&value.value);
        }
        for frame in self.callstack.iter() {
            self.heap.mark_function(&frame.closure().function);
        }
        for upvalue in &self.open_upvalues {
            self.heap.mark_value(upvalue);
        }

        // Trace references
        self.heap.trace();

        // Remove references to unmarked strings in `self.globals` and `self.heap.strings_by_name`
        let globals_to_remove = self
            .globals
            .keys()
            .filter(|string_id| !string_id.marked(black_value))
            .copied()
            .collect::<Vec<_>>();
        for id in globals_to_remove {
            self.globals.remove(&id);
        }
        self.heap
            .strings_by_name
            .retain(|_, string_id| !string_id.marked(black_value));

        // Finally, sweep
        self.heap.sweep();
    }
}
