#![allow(clippy::unnecessary_wraps)]

use rand::Rng;
use std::io;
use std::thread;
use std::time::Duration;
use std::time::{SystemTime, UNIX_EPOCH};

use rustc_hash::FxHashMap as HashMap;

use crate::{
    compiler::Compiler,
    heap::{Heap, StringId},
    value::{ias_f64, ias_u64, List, Number, Value},
    vm::VM,
};

fn clock_native(heap: &mut Heap, _args: &[&Value]) -> Result<Value, String> {
    Ok(Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64()
            .into(),
    ))
}

fn sleep_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    match args[0] {
        Value::Number(Number::Integer(i)) if i >= &0 => {
            thread::sleep(Duration::from_secs(ias_u64(*i)));
        }
        x => {
            return Err(format!(
                "'sleep' expected positive integer argument, got: `{}`",
                *x
            ));
        }
    };
    Ok(Value::Nil)
}

fn assert_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    let value = args[0];
    if value.is_falsey() {
        Err(format!("Assertion on `{value}` failed!"))
    } else {
        Ok(Value::Nil)
    }
}

fn sqrt_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    match args[0] {
        Value::Number(Number::Float(n)) => Ok(n.sqrt().into()),
        Value::Number(Number::Integer(n)) => Ok((ias_f64(*n)).sqrt().into()),
        x => Err(format!("'sqrt' expected numeric argument, got: {}", *x)),
    }
}

fn input_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    match &args[0] {
        Value::String(prompt) => {
            println!("{}", &heap.strings[prompt]);
            let mut choice = String::new();
            match io::stdin().read_line(&mut choice) {
                Ok(_) => {
                    let string = Value::String(heap.string_id(&choice.trim()));
                    Ok(string)
                }
                Err(e) => Err(format!("'input' could not read line: {e}")),
            }
        }
        x => Err(format!("'input' expected string argument, got: {x}")),
    }
}

#[allow(clippy::option_if_let_else)]
fn to_float_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    match args[0] {
        Value::String(string_id) => {
            let string = &heap.strings[string_id];
            let converted: Result<f64, _> = string.parse();
            match converted {
                Ok(result) => Ok(Value::Number(result.into())),
                Err(_) => Err(format!(
                    "'float' could not convert string '{string}' to a float."
                )),
            }
        }
        Value::Number(n) => Ok(Value::Number(f64::from(*n).into())),
        Value::Bool(value) => Ok(Value::Number(f64::from(*value).into())),
        x => Err(format!(
            "'float' expected string, number or bool argument, got: {x}"
        )),
    }
}

#[allow(clippy::option_if_let_else)]
fn to_int_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    match args[0] {
        Value::String(string_id) => {
            let string = &heap.strings[string_id];
            let converted: Result<i64, _> = string.parse();
            match converted {
                Ok(result) => Ok(Value::Number(result.into())),
                Err(_) => Err(format!(
                    "'int' could not convert string '{string}' to an integer."
                )),
            }
        }
        Value::Number(n) => Ok(Value::Number(i64::from(*n).into())),
        Value::Bool(value) => Ok(Value::Number(i64::from(*value).into())),
        x => Err(format!(
            "'int' expected string, number or bool argument, got: {x}"
        )),
    }
}

#[allow(clippy::option_if_let_else)]
fn is_int_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    match args[0] {
        Value::String(string_id) => {
            let string = &heap.strings[string_id];
            let converted: Result<i64, _> = string.parse();
            match converted {
                Ok(_) => Ok(Value::Bool(true)),
                Err(_) => Ok(Value::Bool(false)),
            }
        }
        Value::Number(_) | Value::Bool(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

fn to_string_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    let value = args[0];
    let string = Value::String(heap.string_id(&value.to_string()));
    Ok(string)
}

fn type_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    let string = match args[0] {
        Value::Bool(_) => Value::String(heap.string_id(&"<type bool>")),
        Value::BoundMethod(_) => Value::String(heap.string_id(&"<type bound method>")),
        Value::Class(_) => Value::String(heap.string_id(&"<type class>")),
        Value::Closure(_) => Value::String(heap.string_id(&"<type closure>")),
        Value::Function(_) => Value::String(heap.string_id(&"<type function>")),
        Value::Instance(instance) => Value::String(
            heap.string_id(&("<type ".to_string() + instance.class.name.as_str() + ">")),
        ),
        Value::NativeFunction(_) => Value::String(heap.string_id(&"<type native function>")),
        Value::NativeMethod(_) => Value::String(heap.string_id(&"<type native method>")),
        Value::Nil => Value::String(heap.string_id(&"<type nil>")),
        Value::Number(n) => match n {
            Number::Float(_) => Value::String(heap.string_id(&"<type float>")),
            Number::Integer(_) => Value::String(heap.string_id(&"<type int>")),
        },
        Value::String(_) => Value::String(heap.string_id(&"<type string>")),
        Value::Upvalue(_) => Value::String(heap.string_id(&"<type upvalue>")),
        Value::List(_) => Value::String(heap.string_id(&"<type list>")),
    };
    Ok(string)
}

fn print_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    let end = if args.len() == 2 {
        match args[1] {
            Value::String(string_id) => &heap.strings[string_id],
            x => {
                return Err(format!(
                    "Optional second argument to 'print' has to be a string, got: {x}"
                ))
            }
        }
    } else {
        "\n"
    };
    let value = args[0];
    print!("{value}{end}");
    Ok(Value::Nil)
}

fn rng_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    match (args[0], args[1]) {
        (Value::Number(Number::Integer(min)), Value::Number(Number::Integer(max))) => Ok(
            Value::Number(rand::thread_rng().gen_range(*min..*max).into()),
        ),
        (other_1, other_2) => Err(format!(
            "'rng' expected two integers as arguments, got: `{other_1}` and `{other_2}` instead."
        )),
    }
}

fn init_list_native(
    heap: &mut Heap,
    _receiver: &mut Value,
    _args: &[&Value],
) -> Result<Value, String> {
    let list = List::new(*heap.native_classes.get("List").unwrap());
    Ok(heap.add_list(list))
}

fn append_native(heap: &mut Heap, receiver: &mut Value, args: &[&Value]) -> Result<Value, String> {
    match receiver {
        Value::List(list) => {
            list.items.push(*args[0]);
            Ok(Value::Nil)
        }
        x => Err(format!(
            "'append' expects its first argument to be a list, got `{x}` instead."
        )),
    }
}

fn pop_native(heap: &mut Heap, receiver: &mut Value, args: &[&Value]) -> Result<Value, String> {
    let index = if args.is_empty() {
        None
    } else {
        let index = match args[0] {
            Value::Number(Number::Integer(n)) => match usize::try_from(*n) {
                Ok(index) => index,
                Err(_) => {
                    return Err(format!(
                        "Can not index into list with negative or too large numbers, got `{n}`."
                    ));
                }
            },
            x => {
                return Err(format!("Can only index into list with integer, got `{x}`."));
            }
        };
        Some(index)
    };

    let my_list = match receiver {
        Value::List(list) => list,
        x => {
            return Err(format!(
                "'pop' expects its first argument to be a list, got `{x}` instead."
            ))
        }
    };

    match index {
        Some(index) => {
            let length = my_list.items.len();
            if index >= length {
                Err(format!(
                    "Index `{index}` is out of bounds of list with len `{length}`."
                ))
            } else {
                Ok(my_list.items.remove(index))
            }
        }
        None => my_list
            .items
            .pop()
            .map_or_else(|| Err("Can't 'pop' from an empty list.".to_string()), Ok),
    }
}

fn insert_native(heap: &mut Heap, receiver: &mut Value, args: &[&Value]) -> Result<Value, String> {
    let index = match args[0] {
        Value::Number(Number::Integer(n)) => match usize::try_from(*n) {
            Ok(index) => index,
            Err(_) => {
                return Err(format!(
                    "Can not index into list with negative or too large numbers, got `{n}`."
                ));
            }
        },
        x => {
            return Err(format!("Can only index into list with integer, got `{x}`."));
        }
    };

    let my_list = match receiver {
        Value::List(list) => list,
        x => {
            return Err(format!(
                "'insert' expects its first argument to be a list, got `{x}` instead."
            ))
        }
    };

    let length = my_list.items.len();
    if index > length {
        Err(format!(
            "Index `{index}` is out of bounds of list with len `{length}`."
        ))
    } else {
        my_list.items.insert(index, *args[1]);
        Ok(Value::Nil)
    }
}

fn contains_native(
    heap: &mut Heap,
    receiver: &mut Value,
    args: &[&Value],
) -> Result<Value, String> {
    let my_list = match receiver {
        Value::List(list) => list,
        x => {
            return Err(format!(
                "'contains' expects to be called on a list, got `{x}` instead."
            ))
        }
    };

    for value in &my_list.items {
        if value == args[0] {
            return Ok(Value::Bool(true));
        }
    }
    Ok(Value::Bool(false))
}

#[allow(clippy::cast_possible_wrap)]
fn len_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    match args[0] {
        Value::List(list) => Ok((list.items.len() as i64).into()),
        x => Err(format!("'len' expected list argument, got: `{x}` instead.")),
    }
}

fn getattr_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    match (args[0], args[1]) {
        (Value::Instance(instance), Value::String(string_id)) => {
            let field = &heap.strings[string_id];
            instance.fields.get(field).map_or_else(
                || Err(format!("Undefined property '{}'.", *field)),
                |value_id| Ok(*value_id),
            )
        }
        (instance @ Value::Instance(_), x) => Err(format!(
            "`getattr` can only index with string indexes, got: `{x}` (instance: `{instance}`)"
        )),
        (not_instance, _) => Err(format!(
            "`getattr` only works on instances, got `{not_instance}`"
        )),
    }
}

fn setattr_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    if let Value::String(string_id) = args[1] {
        let field = heap.strings[string_id].clone();
        if let Value::Instance(instance) = args[0] {
            instance.fields.insert(field, *args[2]);
            Ok(Value::Nil)
        } else {
            Err(format!(
                "`setattr` only works on instances, got `{}`",
                args[0]
            ))
        }
    } else {
        Err(format!(
            "`setattr` can only index with string indexes, got: `{}` (instance: `{}`)",
            args[1], args[0]
        ))
    }
}

fn hasattr_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    match (args[0], args[1]) {
        (Value::Instance(instance), Value::String(string_id)) => Ok(Value::Bool(
            instance.fields.contains_key(&heap.strings[string_id]),
        )),
        (instance @ Value::Instance(_), x) => Err(format!(
            "`hasattr` can only index with string indexes, got: `{x}` (instance: `{instance}`)"
        )),
        (not_instance, _) => Err(format!(
            "`hasattr` only works on instances, got `{not_instance}`"
        )),
    }
}

fn delattr_native(heap: &mut Heap, args: &[&Value]) -> Result<Value, String> {
    if let Value::String(string_id) = args[1] {
        let field = &heap.strings[string_id];
        if let Value::Instance(instance) = args[0] {
            match instance.fields.remove(field) {
                Some(_) => Ok(Value::Nil),
                None => Err(format!("Undefined property '{field}'.")),
            }
        } else {
            Err(format!(
                "`delattr` only works on instances, got `{}`",
                args[0]
            ))
        }
    } else {
        Err(format!(
            "`delattr` can only index with string indexes, got: `{}` (instance: `{}`)",
            args[1], args[0]
        ))
    }
}

pub struct Natives {
    string_ids: HashMap<String, StringId>,
}

impl Natives {
    #[must_use]
    pub fn new() -> Self {
        Self {
            string_ids: HashMap::default(),
        }
    }

    pub fn create_names(&mut self, heap: &mut Heap) {
        for name in [
            "clock", "sleep", "assert", "sqrt", "input", "float", "int", "str", "type", "getattr",
            "setattr", "hasattr", "delattr", "rng", "print", "append", "pop", "insert", "len",
            "List", "contains", "is_int",
        ] {
            let string_id = heap.string_id(&name);
            self.string_ids.insert(name.to_string(), string_id);
        }
    }

    pub fn register_names(&self, compiler: &mut Compiler) {
        compiler.inject_strings(&self.string_ids);
    }

    pub fn define_natives(&self, vm: &mut VM) {
        vm.define_native_function(self.string_ids["clock"], &[0], clock_native);
        vm.define_native_function(self.string_ids["assert"], &[1], assert_native);
        vm.define_native_function(self.string_ids["sleep"], &[1], sleep_native);
        vm.define_native_function(self.string_ids["sqrt"], &[1], sqrt_native);
        vm.define_native_function(self.string_ids["input"], &[1], input_native);
        vm.define_native_function(self.string_ids["float"], &[1], to_float_native);
        vm.define_native_function(self.string_ids["int"], &[1], to_int_native);
        vm.define_native_function(self.string_ids["is_int"], &[1], is_int_native);
        vm.define_native_function(self.string_ids["str"], &[1], to_string_native);
        vm.define_native_function(self.string_ids["type"], &[1], type_native);
        vm.define_native_function(self.string_ids["print"], &[1, 2], print_native);
        vm.define_native_function(self.string_ids["getattr"], &[2], getattr_native);
        vm.define_native_function(self.string_ids["setattr"], &[3], setattr_native);
        vm.define_native_function(self.string_ids["hasattr"], &[2], hasattr_native);
        vm.define_native_function(self.string_ids["delattr"], &[2], delattr_native);
        vm.define_native_function(self.string_ids["rng"], &[2], rng_native);
        vm.define_native_function(self.string_ids["len"], &[1], len_native);

        vm.define_native_class(self.string_ids["List"]);
        vm.define_native_method(
            self.string_ids["List"],
            self.string_ids["append"],
            &[1],
            append_native,
        );
        vm.define_native_method(
            self.string_ids["List"],
            vm.init_string(),
            &[0],
            init_list_native,
        );
        vm.define_native_method(
            self.string_ids["List"],
            self.string_ids["pop"],
            &[0, 1],
            pop_native,
        );
        vm.define_native_method(
            self.string_ids["List"],
            self.string_ids["insert"],
            &[2],
            insert_native,
        );
        vm.define_native_method(
            self.string_ids["List"],
            self.string_ids["contains"],
            &[1],
            contains_native,
        );
    }
}
