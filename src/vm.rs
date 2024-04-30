use path_slash::PathBufExt;
use rustc_hash::FxHashMap as HashMap;
use std::collections::VecDeque;
use std::path::PathBuf;
use std::pin::Pin;

#[cfg(feature = "trace_execution")]
use crate::chunk::InstructionDisassembler;
use crate::natives;
use crate::value::NativeClass;
use crate::{
    chunk::{CodeOffset, OpCode},
    compiler::Compiler,
    heap::{ClosureId, FunctionId, Heap, ModuleId, StringId, UpvalueId},
    scanner::Scanner,
    stdlib,
    value::{
        Class, Closure, Dict, Function, Instance, List, Module, ModuleContents, NativeFunction,
        NativeFunctionImpl, NativeMethod, NativeMethodImpl, Number, Set, Upvalue, Value,
    },
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Copy)]
pub struct Global {
    pub value: Value,
    mutable: bool,
}

impl std::fmt::Display for Global {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Value: {}, mutable: {}", self.value, self.mutable)
    }
}

#[derive(Debug)]
pub struct CallFrame {
    closure: ClosureId,
    ip: usize,
    stack_base: usize,
    is_module: bool,
}

impl CallFrame {
    pub fn closure(&self) -> &Closure {
        &self.closure
    }
}

#[derive(Debug)]
pub struct CallStack {
    frames: Vec<CallFrame>,
    current_closure: Option<ClosureId>,
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
        self.current_function = self.current_closure.map(|c| c.function);
        retval
    }

    fn push(&mut self, closure: ClosureId, stack_base: usize) {
        self.frames.push(CallFrame {
            closure,
            ip: 0,
            stack_base,
            is_module: closure.is_module,
        });
        self.current_closure = Some(closure);
        self.current_function = Some(closure.function);
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

    fn closure(&self) -> ClosureId {
        self.current_closure.unwrap()
    }

    fn function(&self) -> FunctionId {
        self.current_function.unwrap()
    }

    pub fn len(&self) -> usize {
        self.frames.len()
    }
}

// Macro for performance reasons
// with macro/inlined it takes ~300ms to run fib.gen
// with function and wrapping return value in option takes ~450ms
macro_rules! run_instruction {
    ($self:ident) => {
        #[cfg(feature = "trace_execution")]
        {
            let function = &$self.callstack.function();
            let mut disassembler = InstructionDisassembler::new(&function.chunk);
            *disassembler.offset = $self.callstack.current().ip;
            #[cfg(feature = "trace_execution_verbose")]
            {
                println!(
                    "Current module: {} at module depth {} and total call depth {}.",
                    *$self.modules.last().expect("Module underflow in disassembler").name,
                    $self.modules.len(),
                    $self.callstack.len()
                );
            }
            println!(
                "          [ { } ]",
                $self.stack.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(" ][ ")
            );
            print!("{disassembler:?}");
        }
        $self.collect_garbage();
        match OpCode::try_from($self.read_byte()).expect("Internal error: unrecognized opcode") {
            OpCode::Pop => {
                $self.stack.pop().expect("Stack underflow in OP_POP.");
            }
            OpCode::Dup => {
                $self.stack_push_value(*$self.peek(0).expect("stack underflow in OP_DUP"));
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
                    $self.stack_push_value(
                        *$self.peek(depth).expect("stack underflow in OP_DUP"),
                    );
                }
            }
            OpCode::Swap => {
                let len = $self.stack.len();
                $self.stack.swap(len-1, len-2);
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
            op @ (OpCode::GetLocal | OpCode::GetLocalLong) => $self.get_local(op),
            op @ (OpCode::SetLocal | OpCode::SetLocalLong) => $self.set_local(op),
            op @ (OpCode::GetGlobal | OpCode::GetGlobalLong) => {
                if let Some(value) = $self.get_global(op) {
                    return value;
                }
            }
            op @ (OpCode::SetGlobal | OpCode::SetGlobalLong) => {
                if let Some(value) = $self.set_global(op) {
                    return value;
                }
            }
            op @ (OpCode::DefineGlobal
            | OpCode::DefineGlobalLong
            | OpCode::DefineGlobalConst
            | OpCode::DefineGlobalConstLong) => $self.define_global(op),
            OpCode::JumpIfFalse => $self.jump_conditional(false),
            OpCode::JumpIfTrue => $self.jump_conditional(true),
            OpCode::Call => {
                if let Some(value) = $self.call() {
                    return value;
                }
            }
            OpCode::Return => {
                if let Some(value) = $self.return_() {
                    return value;
                }
            }
            OpCode::Constant => {
                let value = $self.read_constant(false);
                $self.stack_push(value);
            }
            OpCode::ConstantLong => {
                let value = $self.read_constant(true);
                $self.stack_push(value);
            }
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
            OpCode::Add => {
                if let Some(value) = $self.add() {
                    return value;
                }
            }
            OpCode::Subtract => binary_op!($self, -, false),
            OpCode::Multiply => binary_op!($self, *, false),
            OpCode::Divide => binary_op!($self, /, false),
            OpCode::BitXor => binary_op!($self, ^, true),
            OpCode::BitAnd => binary_op!($self, &, true),
            OpCode::BitOr => binary_op!($self, |, true),
            OpCode::Exp => {
                if let Some(value) = $self.exponatiate() {
                    return value;
                }
            }
            OpCode::Mod => binary_op!($self, %, false),
            OpCode::FloorDiv => {
                if let Some(value) = $self.floor_div() {
                    return value;
                }
            }
            OpCode::Greater => binary_op!($self, >, false),
            OpCode::Less => binary_op!($self, <, false),
            OpCode::GreaterEqual => binary_op!($self, >=, false),
            OpCode::LessEqual => binary_op!($self, <=, false),
            OpCode::NotEqual => $self.equal(true),
            OpCode::Jump => {
                let offset = $self.read_16bit_number();
                $self.callstack.current_mut().ip += offset;
            }
            OpCode::Loop => {
                let offset = $self.read_16bit_number();
                $self.callstack.current_mut().ip -= offset;
            }
            OpCode::Closure => {
                let value = $self.read_constant(false);
                let function = value.as_function();
                let mut closure = Closure::new(*function, false, $self.modules.last().copied());

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
                            .push($self.callstack.closure().upvalues[index]);
                    }
                }
                let closure_id = $self.heap.add_closure(closure);
                $self.stack_push(closure_id);
            }
            OpCode::GetUpvalue => {
                let upvalue_index = usize::from($self.read_byte());
                let closure = $self.callstack.closure();
                let upvalue_location = closure.upvalues[upvalue_index];
                match *upvalue_location {
                    Upvalue::Open(absolute_local_index) => {
                        $self.stack_push($self.stack[absolute_local_index]);
                    }
                    Upvalue::Closed(value) => $self.stack_push(value),
                }
            }
            OpCode::SetUpvalue => {
                let upvalue_index = usize::from($self.read_byte());
                let closure = $self.callstack.closure();
                let mut upvalue_location = closure.upvalues[upvalue_index];
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
            OpCode::CloseUpvalue => {
                $self.close_upvalue($self.stack.len() - 1);
                $self.stack.pop();
            }
            OpCode::Class => {
                let class_name = $self.read_string("OP_CLASS");
                let class = $self.heap.add_class(Class::new(class_name, false));
                $self.stack_push_value(class);
            }
            OpCode::GetProperty => {
                let field = $self.read_string("GET_PROPERTY");
                let value = *$self.peek(0).expect("Stack underflow in GET_PROPERTY");
                // Probably better to just grab the class and ask if it is native
                match value {
                    Value::Instance(instance) => {
                        if let Some(value) = instance.fields.get(&*field) {
                            $self.stack.pop(); // instance
                            $self.stack_push(*value);
                        } else if $self.bind_method(instance.class.into(), field) {
                            // Just using the side effects
                        } else {
                            runtime_error!($self, "Undefined property '{}'.", *field);
                            return InterpretResult::RuntimeError;
                        }
                    }
                    Value::Module(module) => {
                        if let Some(value) = module.globals.get(&field) {
                            $self.stack.pop(); // instance
                            $self.stack_push(value.value);
                        } else {
                            runtime_error!(
                                $self,
                                "Undefined name '{}' in module {}.",
                                *field,
                                *module.name
                            );
                            return InterpretResult::RuntimeError;
                        }
                    }
                    x => {
                        runtime_error!(
                            $self,
                            "Tried to get property '{}' of non-instance `{}`.",
                            *field,
                            x
                        );
                        return InterpretResult::RuntimeError;
                    }
                };
            }
            OpCode::SetProperty => {
                let field_string_id = $self.read_string("SET_PROPERTY");
                let field = &$self.heap.strings[&field_string_id];
                match &$self.peek(1).expect("Stack underflow in SET_PROPERTY") {
                    Value::Instance(instance) => instance,
                    x => {
                        runtime_error!(
                            $self,
                            "Tried to set property '{}' of non-instance `{}`",
                            field,
                            x
                        );
                        return InterpretResult::RuntimeError;
                    }
                };
                let value = $self.stack.pop().expect("Stack underflow in SET_PROPERTY");
                let mut instance = $self.stack.pop().expect("Stack underflow in SET_PROPERTY");
                instance
                    .as_instance_mut()
                    .fields
                    .insert(field.to_string(), value);
                $self.stack_push(value);
            }
            OpCode::Method => {
                let method_name = $self.read_string("OP_METHOD");
                $self.define_method(method_name);
            }
            OpCode::Invoke => {
                let method_name = $self.read_string("OP_INVOKE");
                let arg_count = $self.read_byte();
                if !$self.invoke(method_name, arg_count) {
                    return InterpretResult::RuntimeError;
                }
            }
            OpCode::Inherit => {
                let superclass_id = $self.peek(1).expect("Stack underflow in OP_INHERIT");
                let superclass = if let Value::Class(superclass) = &superclass_id {
                    if superclass.is_native {
                        runtime_error!($self, "Can not inherit from native classes yet.");
                        return InterpretResult::RuntimeError;
                    }
                    superclass
                } else {
                    runtime_error!($self, "Superclass must be a class.");
                    return InterpretResult::RuntimeError;
                };
                let methods = superclass.methods.clone();
                let mut subclass = $self.stack.pop().expect("Stack underflow in OP_INHERIT");
                subclass.as_class_mut().methods.extend(methods);
            }
            OpCode::GetSuper => {
                let method_name = $self.read_string("OP_GET_SUPER");
                let superclass = $self.stack.pop().expect("Stack underflow in OP_GET_SUPER");
                if !$self.bind_method(superclass, method_name) {
                    return InterpretResult::RuntimeError;
                }
            }
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
            OpCode::BuildList => {
                let mut list = List::new();

                let arg_count = $self.read_byte();
                for index in (0..arg_count).rev() {
                    list.items.push(*$self.peek(index as usize).unwrap());
                }
                for _ in 0..arg_count {
                    $self.stack.pop();
                }
                let instance = Instance::new(*$self.heap.native_classes.get("List").unwrap(), Some(list.into()));
                let instance_value = $self.heap.add_instance(instance);
                $self.stack_push_value(instance_value);
            }
            OpCode::BuildSet => {
                let mut set = Set::new();

                let arg_count = $self.read_byte();
                for index in (0..arg_count).rev() {
                    let value = $self.peek(index as usize).unwrap();
                    if !value.is_hasheable() {
                        runtime_error!($self, "Value `{}` is not hashable when this is required for items in a set.", value);
                        return InterpretResult::RuntimeError;
                    }
                    set.items.insert(*$self.peek(index as usize).unwrap());
                }
                for _ in 0..arg_count {
                    $self.stack.pop();
                }
                let instance = Instance::new(*$self.heap.native_classes.get("Set").unwrap(), Some(set.into()));
                let instance_value = $self.heap.add_instance(instance);
                $self.stack_push_value(instance_value);
            }
            OpCode::BuildDict => {
                let mut dict = Dict::new();
                // Number of key, value pairs.
                let arg_count = $self.read_byte();
                for index in (0..arg_count).rev() {
                    let key = $self.peek((2*index+1) as usize).unwrap();
                    let value = $self.peek((2*index) as usize).unwrap();
                    dict.items.insert(*key, *value);
                }
                for _ in 0..arg_count {
                    // Pop key AND value
                    $self.stack.pop();
                    $self.stack.pop();
                }
                let instance = Instance::new(*$self.heap.native_classes.get("Dict").unwrap(), Some(dict.into()));
                let instance_value = $self.heap.add_instance(instance);
                $self.stack_push_value(instance_value);
            }
            OpCode::Import => {
                let file_path = $self.stack.pop().expect("Stack underflow in OP_IMPORT");
                if let Some(value) = $self.import_file(file_path, None, None) {
                    return value;
                }
            }
            OpCode::ImportAs => {
                let alias = $self.read_string("OP_IMPORT_AS");
                let file_path = $self.stack.pop().expect("Stack underflow in OP_IMPORT_AS");
                if let Some(value) = $self.import_file(file_path, None, Some(alias)) {
                    return value;
                }
            }
            OpCode::ImportFrom => {
                let n_names_to_import = $self.read_byte();
                let names_to_import = if n_names_to_import > 0 {
                    let mut names = Vec::with_capacity(n_names_to_import as usize);
                    for _ in 0..n_names_to_import {
                        let name = $self.stack.pop().expect("Stack underflow in OP_IMPORT_FROM");
                        if let Value::String(name) = name {
                            names.push(name);
                        } else {
                            runtime_error!(
                                $self,
                                "Imported names must be strings, got `{}` instead.",
                                name
                            );
                            return InterpretResult::RuntimeError;
                        }
                    }
                    Some(names)
                } else {
                    None
                };

                let file_path = $self.stack.pop().expect("Stack underflow in OP_IMPORT_FROM");
                if let Some(value) = $self.import_file(file_path, names_to_import, None) {
                    return value;
                }
            }
        };
    };
}

pub struct VM {
    pub heap: Pin<Box<Heap>>,
    pub callstack: CallStack,
    pub stack: Vec<Value>,
    open_upvalues: VecDeque<UpvalueId>,
    // Could also keep a cache of the last module or its globals for performance
    modules: Vec<ModuleId>,
    path: PathBuf,
    builtins: HashMap<StringId, Global>,
    stdlib: HashMap<StringId, ModuleContents>,
}

impl VM {
    #[must_use]
    pub fn new(path: PathBuf) -> Self {
        Self {
            heap: Heap::new(),
            callstack: CallStack::new(),
            stack: Vec::with_capacity(crate::config::STACK_MAX),
            open_upvalues: VecDeque::new(),
            modules: Vec::new(),
            path,
            builtins: HashMap::default(),
            stdlib: HashMap::default(),
        }
    }

    pub fn globals(&mut self) -> &mut HashMap<StringId, Global> {
        &mut self.modules.last_mut().unwrap().globals
    }

    pub fn current_module(&mut self) -> ModuleId {
        *self.modules.last().unwrap()
    }

    pub fn defining_module(&mut self) -> ModuleId {
        let current_closure = self.callstack.current().closure;
        match current_closure.containing_module {
            Some(module) if !current_closure.is_module => module,
            _ => self.current_module(),
        }
    }

    fn compile(&mut self, source: &[u8], name: &str) -> Option<Function> {
        let scanner = Scanner::new(source);
        let compiler = Compiler::new(scanner, &mut self.heap, name);
        compiler.compile()
    }

    pub fn interpret(&mut self, source: &[u8]) -> InterpretResult {
        let result = if let Some(function) = self.compile(source, "<script>") {
            let function_id = self.heap.add_function(function);

            let closure = Closure::new(*function_id.as_function(), true, None);

            self.add_closure_to_modules(&closure, self.path.clone(), None, None);

            let value_id = self.heap.add_closure(closure);
            self.stack_push(value_id);
            self.execute_call(value_id, 0);

            // Need to have the first module loaded before defining natives
            natives::define(self);
            stdlib::register(self);

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
        loop {
            run_instruction!(self);
        }
    }

    pub fn execute_and_run_function(&mut self, closure: Value, arg_count: u8) -> InterpretResult {
        self.stack_push(closure);
        self.execute_call(closure, arg_count);
        self.run_function()
    }

    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    pub fn run_function(&mut self) -> InterpretResult {
        let call_depth = self.callstack.len();
        while self.callstack.len() >= call_depth {
            run_instruction!(self);
        }
        InterpretResult::Ok
    }

    pub fn peek(&self, n: usize) -> Option<&Value> {
        let len = self.stack.len();
        if n >= len {
            None
        } else {
            Some(&self.stack[len - n - 1])
        }
    }

    fn peek_mut(&mut self, n: usize) -> Option<&mut Value> {
        let len = self.stack.len();
        if n >= len {
            None
        } else {
            Some(&mut self.stack[len - n - 1])
        }
    }

    fn read_string(&mut self, opcode_name: &str) -> StringId {
        let constant = self.read_constant(false);
        match &constant {
            Value::String(string_id) => *string_id,
            x => {
                panic!("Non-string method name to {opcode_name}: `{x}`");
            }
        }
    }

    fn add_closure_to_modules(
        &mut self,
        closure: &Closure,
        file_path: PathBuf,
        names_to_import: Option<Vec<StringId>>,
        alias: Option<StringId>,
    ) {
        if closure.is_module {
            let value_id = closure.function.name;
            let script_name = self.heap.builtin_constants().script_name;
            let alias = alias.map_or(value_id, |alias| alias);
            let module_id =
                self.heap
                    .add_module(Module::new(value_id, file_path, names_to_import, alias));
            self.modules.push(*module_id.as_module());
            // Scriptname has to be set in the globals of the new module.
            self.globals().insert(
                script_name,
                Global {
                    value: value_id.into(),
                    mutable: true,
                },
            );
        }
    }

    #[allow(clippy::option_if_let_else)]
    fn import_file(
        &mut self,
        file_path_value: Value,
        names_to_import: Option<Vec<StringId>>,
        alias: Option<StringId>,
    ) -> Option<InterpretResult> {
        let string_id = match file_path_value {
            Value::String(string_id) => string_id,
            x => {
                runtime_error!(
                    self,
                    "Imported file path must be a string, got `{}` instead.",
                    x
                );
                return Some(InterpretResult::RuntimeError);
            }
        };

        let file_path = self.clean_filepath(string_id);

        let name = if let Some(stem) = file_path.file_stem() {
            stem.to_str().unwrap().to_string()
        } else {
            runtime_error!(self, "Import path should have a filestem.");
            return Some(InterpretResult::RuntimeError);
        };
        let name_id = self.heap.string_id(&name);

        for module in &self.modules {
            if module.path.canonicalize().unwrap() == file_path {
                runtime_error!(self, "Circular import of module `{}` detected.", *name_id);
                return Some(InterpretResult::RuntimeError);
            }
        }

        let mut generic_stdlib_path = PathBuf::from(file!());
        generic_stdlib_path.pop();
        generic_stdlib_path.push("stdlib");
        generic_stdlib_path.push(format!("{name}.gen"));
        let generic_stdlib_path = match generic_stdlib_path.canonicalize() {
            Ok(path) => path,
            Err(_) => generic_stdlib_path,
        };

        // User defined generic module
        if let Ok(contents) = std::fs::read(&file_path) {
            if let Some(value) =
                self.import_generic_module(&contents, &name, file_path, names_to_import, alias)
            {
                return Some(value);
            }
        } else if let Ok(contents) = std::fs::read(generic_stdlib_path) {
            // stdlib generic module
            if let Some(value) =
                self.import_generic_module(&contents, &name, file_path, names_to_import, alias)
            {
                return Some(value);
            }
        } else if let Some(stdlib_functions) = self.stdlib.get(&string_id).cloned() {
            // These clones are only necessary because this is extracted into a function.
            // If they cause performance issues this can be inlined or turned into a macro.
            if let Some(value) = self.import_rust_stdlib(
                string_id,
                file_path,
                alias,
                &stdlib_functions,
                names_to_import,
            ) {
                return Some(value);
            }
        } else {
            runtime_error!(
                self,
                "Could not find the file to be imported. Attempted path {:?} and stdlib.",
                file_path.to_slash_lossy()
            );
            return Some(InterpretResult::RuntimeError);
        }
        None
    }

    fn import_rust_stdlib(
        &mut self,
        string_id: StringId,
        file_path: PathBuf,
        alias: Option<StringId>,
        stdlib_functions: &ModuleContents,
        names_to_import: Option<Vec<StringId>>,
    ) -> Option<InterpretResult> {
        let mut module = Module::new(
            string_id,
            file_path,
            None,
            alias.map_or(string_id, |alias| alias),
        );
        for (name, arity, fun) in stdlib_functions {
            let name_id = self.heap.string_id(name);
            self.heap
                .strings_by_name
                .insert((*name).to_string(), name_id);
            let value = self.heap.add_native_function(NativeFunction {
                name: name_id,
                arity,
                fun: *fun,
            });
            module.globals.insert(
                name_id,
                Global {
                    value,
                    mutable: false,
                },
            );
        }
        // Stdlib rust module
        //  Add all the functions to the modules globals
        // If we only want to import some functions then we just move them
        // from the new module to the current globals, the module then gets dropped.
        if let Some(names_to_import) = names_to_import {
            for name in names_to_import {
                if let Some(global) = module.globals.remove(&name) {
                    self.globals().insert(name, global);
                } else {
                    runtime_error!(self, "Could not find name to import.");
                    return Some(InterpretResult::RuntimeError);
                }
            }
        } else {
            // Otherwise we add the whole module to the current globals.
            let module_id = self.heap.add_module(module);
            self.globals().insert(
                string_id,
                Global {
                    value: module_id,
                    mutable: false,
                },
            );
        }
        None
    }

    fn import_generic_module(
        &mut self,
        contents: &[u8],
        name: &str,
        file_path: PathBuf,
        names_to_import: Option<Vec<StringId>>,
        alias: Option<StringId>,
    ) -> Option<InterpretResult> {
        if let Some(function) = self.compile(contents, name) {
            let function = self.heap.add_function(function);
            let function_id = function.as_function();
            let closure = Closure::new(*function_id, true, self.modules.last().copied());

            self.add_closure_to_modules(&closure, file_path, names_to_import, alias);

            let value_id = self.heap.add_closure(closure);
            self.stack_push(value_id);
            self.execute_call(value_id, 0);
        } else {
            return Some(InterpretResult::RuntimeError);
        }
        None
    }

    #[allow(clippy::option_if_let_else)]
    fn clean_filepath(&mut self, string_id: StringId) -> PathBuf {
        let file_path = self.modules.last().map_or_else(
            || PathBuf::from(&*string_id),
            |module| {
                let mut path = module.path.clone();
                path.pop();
                path.push(&*string_id);
                path
            },
        );

        let file_path = match file_path.strip_prefix("./") {
            Ok(file_path) => file_path.to_owned(),
            Err(_) => file_path,
        };
        match file_path.canonicalize() {
            Ok(file_path) => file_path,
            Err(_) => file_path,
        }
    }

    fn binary_op<T: Into<Value>>(&mut self, op: BinaryOp<T>, int_only: bool) -> bool {
        let slice_start = self.stack.len() - 2;

        let ok = match &self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
                    if int_only
                        & (!matches!(a, Number::Integer(_)) | !matches!(b, Number::Integer(_)))
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
                    .map(|v| format!("{v}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }
        ok
    }

    fn add(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        let ok = match &self.stack[slice_start..] {
            [left, right] => match (&left, &right) {
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
                    .map(|v| format!("{v}"))
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
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
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
                    .map(|v| format!("{v}"))
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
                if let (Value::Number(a), Value::Number(b)) = (&left, &right) {
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
                    .map(|v| format!("{v}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            return Some(InterpretResult::RuntimeError);
        }
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
        match &constant_value {
            Value::String(name) => {
                let maybe_value = self
                    .defining_module()
                    .globals
                    .get(name)
                    .map(|global| global.value);
                if let Some(value) = maybe_value {
                    self.stack_push(value);
                } else {
                    let maybe_builtin = self.builtins.get(name).map(|global| global.value);
                    if let Some(value) = maybe_builtin {
                        self.stack_push(value);
                    } else {
                        runtime_error!(self, "Undefined variable '{}'.", self.heap.strings[name]);
                        return Some(InterpretResult::RuntimeError);
                    }
                }
            }

            x => panic!("Internal error: non-string operand to {op:?}: {x:?}"),
        }
        None
    }

    fn set_global(&mut self, op: OpCode) -> Option<InterpretResult> {
        let constant_index = self.read_constant_index(op == OpCode::SetGlobalLong);
        let constant_value = self.read_constant_value(constant_index);
        let name = match &constant_value {
            Value::String(name) => *name,
            x => panic!("Internal error: non-string operand to OP_SET_GLOBAL: {x:?}"),
        };
        let stack_top_value = *self
            .stack
            .last()
            .unwrap_or_else(|| panic!("stack underflow in {op:?}"));
        if let Some(global) = self.defining_module().globals.get_mut(&name) {
            if !global.mutable {
                runtime_error!(self, "Reassignment to global 'const'.");
                return Some(InterpretResult::RuntimeError);
            }
            global.value = stack_top_value;
        } else {
            let maybe_builtin = self.builtins.get_mut(&name);
            if let Some(global) = maybe_builtin {
                if !global.mutable {
                    runtime_error!(self, "Reassignment to global 'const'.");
                    return Some(InterpretResult::RuntimeError);
                }
                global.value = stack_top_value;
            } else {
                runtime_error!(self, "Undefined variable '{}'.", *name);
                return Some(InterpretResult::RuntimeError);
            }
        }

        None
    }

    fn define_global(&mut self, op: OpCode) {
        let constant = self.read_constant(op == OpCode::DefineGlobalLong);
        match &constant {
            Value::String(name) => {
                let name = *name;
                let stack_top_value = *self
                    .stack
                    .last()
                    .unwrap_or_else(|| panic!("stack underflow in {op:?}"));
                self.globals().insert(
                    name,
                    Global {
                        value: stack_top_value,
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
        if frame.is_module {
            self.stack.pop();
            let mut last_module = self.modules.pop().expect("Module underflow in OP_RETURN");
            let last_module_alias = last_module.alias;
            let names_to_import = std::mem::take(&mut last_module.names_to_import);

            if let Some(names) = names_to_import {
                for name in names {
                    let value = last_module
                        .globals
                        .get(&name)
                        .expect("Imported name not found.");
                    self.globals().insert(name, *value);
                }
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
                .name
                .into();
            self.globals().insert(
                script_name,
                Global {
                    value: module_name,
                    mutable: true,
                },
            );
            return None;
        }

        self.close_upvalue(frame.stack_base);
        self.stack.truncate(frame.stack_base);
        self.stack_push(result.expect("Stack underflow in OP_RETURN"));
        None
    }

    fn negate(&mut self) -> Option<InterpretResult> {
        let value_id = *self.peek(0).expect("stack underflow in OP_NEGATE");
        let value = &value_id;
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
        self.stack_push(value.into());
    }

    fn equal(&mut self, negate: bool) {
        let left_id = self
            .stack
            .pop()
            .expect("stack underflow in OP_EQUAL (first)");
        let right_id = self
            .stack
            .pop()
            .expect("stack underflow in OP_EQUAL (second)");
        let result = if negate {
            left_id != right_id
        } else {
            left_id == right_id
        };
        self.stack_push(result.into());
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

    fn read_constant_value(&self, index: usize) -> Value {
        *self.callstack.function().chunk.get_constant(index)
    }

    fn read_constant(&mut self, long: bool) -> Value {
        let index = self.read_constant_index(long);
        self.read_constant_value(index)
    }

    #[inline]
    fn stack_push(&mut self, value_id: Value) {
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
        self.stack.push(value);
    }

    fn stack_get(&self, slot: usize) -> &Value {
        &self.stack[self.stack_base() + slot]
    }

    fn stack_get_mut(&mut self, slot: usize) -> &mut Value {
        let offset = self.stack_base();
        &mut self.stack[offset + slot]
    }

    fn stack_base(&self) -> usize {
        self.callstack.current().stack_base
    }

    fn call_value(&mut self, callee: Value, arg_count: u8) -> bool {
        match callee {
            Value::NativeMethod(_) => {
                println!("Got a native method");
                false
            }
            Value::Closure(_) => self.execute_call(callee, arg_count),
            Value::NativeFunction(f) => self.execute_native_function_call(&f, arg_count),
            Value::Class(class) => {
                let is_native = class.is_native;
                let maybe_initializer = class
                    .methods
                    .get(&self.heap.builtin_constants().init_string);
                let backing = if is_native {
                    Some(NativeClass::new(&class.name))
                } else {
                    None
                };
                let mut instance_id = self.heap.add_instance(Instance::new(callee, backing));
                let stack_index = self.stack.len() - usize::from(arg_count) - 1;
                self.stack[stack_index] = instance_id;
                if let Some(initializer) = maybe_initializer {
                    if is_native {
                        self.execute_native_method_call(
                            initializer.as_native_method(),
                            &mut instance_id,
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
            Value::BoundMethod(mut bound_method) => match bound_method.method {
                Value::Closure(_) => {
                    let new_stack_base = self.stack.len() - usize::from(arg_count) - 1;
                    self.stack[new_stack_base] = bound_method.receiver;
                    self.execute_call(bound_method.method, arg_count)
                }
                Value::NativeMethod(native_method) => self.execute_native_method_call(
                    &native_method,
                    &mut bound_method.receiver,
                    arg_count,
                ),
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
        receiver: &mut Value,
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
        let mut args: Vec<Value> = self.stack[start_index..].to_vec();
        let mut ref_args: Vec<&mut Value> = args.iter_mut().collect();
        let result = fun(self, receiver, ref_args.as_mut_slice());
        match result {
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
        let mut args: Vec<Value> = self.stack[start_index..].to_vec();
        let mut ref_args: Vec<&mut Value> = args.iter_mut().collect();
        let result = fun(self, ref_args.as_mut_slice());
        match result {
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

    fn invoke_from_class(&mut self, class: Value, method_name: StringId, arg_count: u8) -> bool {
        let Some(method) = class.as_class().methods.get(&method_name) else {
            runtime_error!(
                self,
                "Undefined property '{}'.",
                self.heap.strings[&method_name]
            );
            return false;
        };
        match method {
            Value::Closure(_) => self.execute_call(*method, arg_count),
            Value::NativeMethod(native) => {
                let mut receiver = *self.peek(arg_count as usize).unwrap();
                self.execute_native_method_call(native, &mut receiver, arg_count)
            }
            x => unreachable!(
                "Can only invoke closure or native methods. Got `{}` instead.",
                x
            ),
        }
    }

    fn invoke(&mut self, method_name: StringId, arg_count: u8) -> bool {
        let receiver = *self
            .peek(arg_count.into())
            .expect("Stack underflow in OP_INVOKE");
        match receiver {
            Value::Instance(instance) => {
                if let Some(value) = instance.fields.get(&*method_name) {
                    let new_stack_base = self.stack.len() - usize::from(arg_count) - 1;
                    self.stack[new_stack_base] = *value;
                    self.call_value(*value, arg_count)
                } else {
                    self.invoke_from_class(instance.class.into(), method_name, arg_count)
                }
            }
            Value::Module(module) => {
                if let Some(value) = module.globals.get(&method_name) {
                    let new_stack_base = self.stack.len() - usize::from(arg_count) - 1;
                    self.stack[new_stack_base] = value.value;
                    self.call_value(value.value, arg_count)
                } else {
                    runtime_error!(
                        self,
                        "Function '{}' not defined in module {}.",
                        &*method_name,
                        *module.name
                    );
                    false
                }
            }
            _ => {
                runtime_error!(self, "Only instances have methods.");
                false
            }
        }
    }

    fn bind_method(&mut self, class: Value, name: StringId) -> bool {
        let class = class.as_class();
        let Some(method) = class.methods.get(&name) else {
            return false;
        };
        let bound_method = Value::bound_method(
            *self.peek(0).expect("Buffer underflow in OP_METHOD"),
            *method,
            &mut self.heap,
        );
        self.stack.pop();
        self.stack_push_value(bound_method);
        true
    }

    fn capture_upvalue(&mut self, local: usize) -> UpvalueId {
        let local = self.callstack.current().stack_base + local;
        let mut upvalue_index = 0;
        let mut upvalue = None;

        for (i, this) in self.open_upvalues.iter().enumerate() {
            upvalue = Some(this);
            upvalue_index = i;
            if this.as_open() <= local {
                break;
            }
        }

        if let Some(upvalue) = upvalue {
            if upvalue.as_open() == local {
                return *upvalue;
            }
        }
        let upvalue = self.heap.add_upvalue(Upvalue::Open(local));
        let upvalue_id = upvalue.upvalue_location();
        self.open_upvalues.insert(upvalue_index, *upvalue_id);

        *upvalue_id
    }

    fn close_upvalue(&mut self, last: usize) {
        while self
            .open_upvalues
            .front()
            .map_or(false, |v| v.as_open() >= last)
        {
            let mut upvalue = self.open_upvalues.pop_front().unwrap();

            let pointed_value = self.stack[upvalue.as_open()];
            *upvalue = Upvalue::Closed(pointed_value);
        }
    }

    pub fn execute_call(&mut self, closure_id: Value, arg_count: u8) -> bool {
        let closure = closure_id.as_closure();
        let arity = closure.function.arity;
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
            matches!(closure_id, Value::Closure(_)),
            "`execute_call` must be called with a `Closure`, got: {closure_id}"
        );

        self.callstack
            .push(*closure_id.as_closure(), self.stack.len() - arg_count - 1);
        true
    }

    pub fn define_native_function<T: ToString>(
        &mut self,
        name: &T,
        arity: &'static [u8],
        fun: NativeFunctionImpl,
    ) {
        let name_id = self.heap.string_id(name);
        self.heap.strings_by_name.insert(name.to_string(), name_id);
        let value = self.heap.add_native_function(NativeFunction {
            name: name_id,
            arity,
            fun,
        });
        self.builtins.insert(
            name_id,
            Global {
                value,
                mutable: true,
            },
        );
    }

    pub fn register_stdlib_module<T: ToString>(
        &mut self,
        name: &T,
        functions: Vec<(&'static str, &'static [u8], NativeFunctionImpl)>,
    ) {
        let name_id = self.heap.string_id(name);
        self.heap.strings_by_name.insert(name.to_string(), name_id);
        self.stdlib.insert(name_id, functions);
    }

    pub fn define_native_class<T: ToString>(&mut self, name: &T, add_to_builtins: bool) {
        let name_id = self.heap.string_id(name);
        self.heap.strings_by_name.insert(name.to_string(), name_id);
        let value = self.heap.add_class(Class::new(name_id, true));
        if add_to_builtins {
            self.builtins.insert(
                name_id,
                Global {
                    value,
                    mutable: true,
                },
            );
        }
        self.heap.native_classes.insert(name_id.to_string(), value);
    }

    pub fn define_native_method<C: ToString, N: ToString>(
        &mut self,
        class: &C,
        name: &N,
        arity: &'static [u8],
        fun: NativeMethodImpl,
    ) {
        let class_id = self.heap.string_id(class);
        self.heap
            .strings_by_name
            .insert(class.to_string(), class_id);
        let name_id = self.heap.string_id(name);
        self.heap.strings_by_name.insert(name.to_string(), name_id);
        let value_id = self.heap.add_native_method(NativeMethod {
            class: class_id,
            name: name_id,
            arity,
            fun,
        });
        let target_class = self
            .heap
            .native_classes
            .get_mut(&*class_id)
            .unwrap()
            .as_class_mut();
        target_class.methods.insert(name_id, value_id);
    }

    fn collect_garbage(&mut self) {
        #[cfg(not(feature = "stress_gc"))]
        if !self.heap.needs_gc() {
            return;
        }
        let black_value = self.heap.black_value;

        self.heap.gc_start();

        // Mark roots
        for value in &self.stack {
            self.heap.mark_value(value);
        }
        for frame in self.callstack.iter() {
            self.heap.mark_function(&frame.closure().function);
        }
        for upvalue in &self.open_upvalues {
            self.heap.mark_upvalue(upvalue);
        }
        for module in &self.modules {
            self.heap.mark_module(module);
        }
        for builtin in self.builtins.values() {
            self.heap.mark_value(&builtin.value);
        }

        // Trace references
        self.heap.trace();

        // Remove references to unmarked strings in `globals` and `heap.strings_by_name`
        // and `builtins`
        for module in &mut self.modules {
            let globals_to_remove = module
                .globals
                .keys()
                .filter(|string_id| !string_id.marked(black_value))
                .copied()
                .collect::<Vec<_>>();

            for id in globals_to_remove {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("String/{:?} free {}", id, *id);
                }
                module.globals.remove(&id);
            }
        }

        let builtins_to_remove = self
            .builtins
            .keys()
            .filter(|string_id| !string_id.marked(black_value))
            .copied()
            .collect::<Vec<_>>();
        for id in builtins_to_remove {
            self.builtins.remove(&id);
        }

        self.heap.strings_by_name.retain(|_, string_id| {
            #[cfg(feature = "log_gc")]
            if !string_id.marked(black_value) {
                eprintln!("String/{:?} free {}", string_id, **string_id);
            }
            string_id.marked(black_value)
        });

        // Finally, sweep
        self.heap.sweep();
    }
}
