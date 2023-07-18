use rustc_hash::FxHashMap as HashMap;
use std::collections::hash_map::Entry;
use std::{
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr::NonNull,
};

use derivative::Derivative;
use slotmap::{new_key_type, HopSlotMap as SlotMap, Key};
use std::fmt::{Debug, Display};

use crate::value::{Function, Number, Upvalue, Value};

pub trait ArenaValue: Debug + Display + PartialEq {}
impl<T> ArenaValue for T where T: Debug + Display + PartialEq {}

new_key_type! {
    pub struct ValueKey;
    pub struct FunctionKey;
    pub struct StringKey;
}

#[derive(Clone, Debug, PartialOrd, Derivative)]
#[derivative(Hash, PartialEq, Eq)]
pub struct ArenaId<K: Key, T: ArenaValue> {
    id: K,
    #[derivative(Hash = "ignore")]
    pub arena: NonNull<Arena<K, T>>, // Yes this is terrible, yes I'm OK with it for this projec
}

impl<K: Key, T: ArenaValue + Clone> Copy for ArenaId<K, T> {}

impl<K: Key, T: ArenaValue> Deref for ArenaId<K, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.arena.as_ref()[self] }
    }
}

impl<K: Key, T: ArenaValue> DerefMut for ArenaId<K, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut self.arena.as_mut()[self as &Self] }
    }
}

impl<K: Key, T: ArenaValue> ArenaId<K, T> {
    pub fn marked(&self, black_value: bool) -> bool {
        unsafe { self.arena.as_ref().is_marked(self.id, black_value) }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Item<T> {
    marked: bool,
    item: T,
}

impl<T> Item<T> {
    const fn new(item: T, marked: bool) -> Self {
        Self { marked, item }
    }
}

pub type ValueId = ArenaId<ValueKey, Value>;
pub type StringId = ArenaId<StringKey, String>;
pub type FunctionId = ArenaId<FunctionKey, Function>;

#[derive(Clone, Debug)]
pub struct Arena<K: Key, V: ArenaValue> {
    name: &'static str,
    log_gc: bool,

    data: SlotMap<K, Item<V>>,
    bytes_allocated: usize,

    gray: Vec<K>,
}

impl<K: Key, V: ArenaValue> Arena<K, V> {
    #[must_use]
    fn new(name: &'static str, log_gc: bool) -> Self {
        Self {
            name,
            log_gc,
            data: SlotMap::with_key(),
            bytes_allocated: 0,
            gray: Vec::new(),
        }
    }

    fn add(&mut self, value: V, black_value: bool) -> ArenaId<K, V> {
        let id = self.data.insert(Item::new(value, !black_value));
        self.bytes_allocated += std::mem::size_of::<V>();

        if self.log_gc {
            eprintln!(
                "{}/{:?} allocate {} for {}",
                self.name,
                id,
                humansize::format_size(std::mem::size_of::<V>(), humansize::BINARY),
                self.data[id].item
            );
        }

        ArenaId {
            id,
            arena: (&mut *self).into(),
        }
    }

    fn is_marked(&self, index: K, black_value: bool) -> bool {
        self.data[index].marked == black_value
    }

    fn flush_gray(&mut self) -> Vec<K> {
        let capacity = self.gray.capacity();
        std::mem::replace(&mut self.gray, Vec::with_capacity(capacity))
    }

    fn mark(&mut self, index: &ArenaId<K, V>, black_value: bool) -> bool {
        debug_assert_eq!(index.arena.as_ptr().cast_const(), self);
        self.mark_raw(index.id, black_value)
    }

    fn mark_raw(&mut self, index: K, black_value: bool) -> bool {
        let value = &mut self.data[index];
        if value.marked == black_value {
            return false;
        }
        if self.log_gc {
            eprintln!("{}/{:?} mark {}", self.name, index, value.item);
        }
        value.marked = black_value;
        self.gray.push(index);
        true
    }

    fn sweep(&mut self, black_value: bool) {
        self.data.retain(|key, value| {
            let retain = value.marked == black_value;
            if !retain && self.log_gc {
                eprintln!("{}/{:?} free {}", self.name, key, value.item);
            }
            retain
        });
        self.bytes_allocated = std::mem::size_of::<V>() * self.data.len();
    }

    const fn bytes_allocated(&self) -> usize {
        self.bytes_allocated
    }
}

impl<K: Key, V: ArenaValue> std::ops::Index<&ArenaId<K, V>> for Arena<K, V> {
    type Output = V;

    fn index(&self, index: &ArenaId<K, V>) -> &Self::Output {
        debug_assert_eq!(index.arena.as_ptr().cast_const(), self);
        &self[index.id]
    }
}

impl<K: Key, V: ArenaValue> std::ops::Index<K> for Arena<K, V> {
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        &self.data[index].item
    }
}

impl<K: Key, V: ArenaValue> std::ops::IndexMut<&ArenaId<K, V>> for Arena<K, V> {
    fn index_mut(&mut self, index: &ArenaId<K, V>) -> &mut Self::Output {
        debug_assert_eq!(index.arena.as_ptr().cast_const(), self);
        &mut self[index.id]
    }
}

impl<K: Key, V: ArenaValue> std::ops::IndexMut<K> for Arena<K, V> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.data[index].item
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinConstants {
    pub nil: ValueId,
    pub true_: ValueId,
    pub false_: ValueId,
    pub init_string: StringId,
    pub integers: Vec<ValueId>,
    pub floats: Vec<ValueId>,
}

impl BuiltinConstants {
    #[must_use]
    pub fn new(heap: &mut Heap) -> Self {
        Self {
            nil: heap.add_value(Value::Nil),
            true_: heap.add_value(Value::Bool(true)),
            false_: heap.add_value(Value::Bool(false)),
            init_string: heap.string_id(&"init".to_string()),
            integers: (0..1024)
                .map(|n| heap.add_value(Value::Number(Number::Integer(n))))
                .collect(),
            floats: (0..1024)
                .map(|n| heap.add_value(Value::Number(Number::Float(n.into()))))
                .collect(),
        }
    }

    pub const fn bool(&self, val: bool) -> ValueId {
        if val {
            self.true_
        } else {
            self.false_
        }
    }

    #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
    pub fn number(&self, n: Number) -> Option<ValueId> {
        match n {
            // Negative integers wrap too high and get gives None as expected
            Number::Integer(i) => self.integers.get(i as usize).copied(),
            Number::Float(f) => {
                if f.fract() != 0.0 || f.is_nan() || f.is_infinite() || f.signum() < 0.0 {
                    None
                } else {
                    self.floats.get(f as usize).copied()
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Heap {
    builtin_constants: Option<BuiltinConstants>,
    pub strings_by_name: HashMap<String, StringId>,
    pub native_classes: HashMap<String, ValueId>,
    pub strings: Arena<StringKey, String>,
    pub values: Arena<ValueKey, Value>,
    pub functions: Arena<FunctionKey, Function>,

    log_gc: bool,
    next_gc: usize,
    pub black_value: bool,
}

impl Heap {
    pub fn new() -> Pin<Box<Self>> {
        let log_gc = crate::config::LOG_GC.load();

        let strings_by_name: HashMap<String, StringId> = HashMap::default();

        let mut heap = Box::pin(Self {
            builtin_constants: None,
            strings_by_name,
            native_classes: HashMap::default(),

            strings: Arena::new("String", log_gc),
            values: Arena::new("Value", log_gc),
            functions: Arena::new("Function", log_gc),

            log_gc,
            next_gc: 1024 * 1024,
            black_value: true,
        });
        // Very important: first pin, *then* initialize the constants, as the `ArenaId`s generated
        // here will carry a raw pointer that needs to remain valid
        heap.builtin_constants = Some(BuiltinConstants::new(&mut heap));
        let init_string = heap.builtin_constants().init_string;
        heap.strings_by_name
            .insert(init_string.to_string(), init_string);

        heap
    }

    pub fn builtin_constants(&self) -> &BuiltinConstants {
        self.builtin_constants.as_ref().unwrap()
    }

    pub fn string_id<S>(&mut self, s: &S) -> StringId
    where
        S: ToString,
    {
        if let Entry::Occupied(entry) = self.strings_by_name.entry(s.to_string()) {
            return *entry.get();
        }
        let string_id = self.add_string(s.to_string());
        self.strings_by_name.insert(s.to_string(), string_id);
        string_id
    }

    const fn bytes_allocated(&self) -> usize {
        self.values.bytes_allocated()
            + self.strings.bytes_allocated()
            + self.functions.bytes_allocated()
    }

    pub const fn needs_gc(&self) -> bool {
        self.bytes_allocated() > self.next_gc
    }

    pub fn gc_start(&mut self) {
        if self.log_gc {
            eprintln!("-- gc begin");
        }

        self.values
            .mark(&self.builtin_constants().nil.clone(), self.black_value);
        self.values
            .mark(&self.builtin_constants().true_.clone(), self.black_value);
        self.values
            .mark(&self.builtin_constants().false_.clone(), self.black_value);
        self.strings.mark(
            &self.builtin_constants().init_string.clone(),
            self.black_value,
        );
        for number in self.builtin_constants().integers.clone() {
            self.values.mark(&number, self.black_value);
        }
        for number in self.builtin_constants().floats.clone() {
            self.values.mark(&number, self.black_value);
        }
    }

    pub fn trace(&mut self) {
        if self.log_gc {
            eprintln!("-- trace start");
        }
        while !self.functions.gray.is_empty()
            || !self.strings.gray.is_empty()
            || !self.values.gray.is_empty()
        {
            for index in self.values.flush_gray() {
                self.blacken_value(index);
            }
            for index in self.strings.flush_gray() {
                self.blacken_string(index);
            }
            for index in self.functions.flush_gray() {
                self.blacken_function(index);
            }
        }
    }

    pub fn mark_value(&mut self, id: &ValueId) {
        self.blacken_value(id.id);
    }

    pub fn mark_string(&mut self, id: &StringId) {
        self.blacken_string(id.id);
    }

    pub fn mark_function(&mut self, id: &FunctionId) {
        self.blacken_function(id.id);
    }

    #[allow(clippy::too_many_lines)]
    fn blacken_value(&mut self, index: ValueKey) {
        let item = &mut self.values.data[index];
        if item.marked == self.black_value {
            return;
        }

        if self.log_gc {
            eprintln!("Value/{:?} blacken {} start", index, item.item);
        }
        item.marked = self.black_value;
        self.values.gray.push(index);
        match &item.item {
            Value::Bool(_) | Value::Nil | Value::Number(_) | Value::Upvalue(Upvalue::Open(_)) => {}
            Value::String(string_id) => {
                if self.log_gc {
                    eprintln!("String/{:?} gray {}", string_id.id, **string_id);
                }
                self.strings.gray.push(string_id.id);
            }
            Value::Function(function_id) => {
                if self.log_gc {
                    eprintln!("Function/{:?} gray {}", function_id, **function_id);
                }
                self.functions.gray.push(function_id.id);
            }

            Value::NativeFunction(native_function_id) => {
                if self.log_gc {
                    eprintln!(
                        "String/{:?} gray {}",
                        native_function_id.name.id, *native_function_id.name
                    );
                }
                self.strings.gray.push(native_function_id.name.id);
            }
            Value::NativeMethod(native_methods_id) => {
                if self.log_gc {
                    eprintln!(
                        "String/{:?} gray {}",
                        native_methods_id.name.id, *native_methods_id.name
                    );
                    eprintln!(
                        "String/{:?} gray {}",
                        native_methods_id.class.id, *native_methods_id.class
                    );
                }
                self.strings.gray.push(native_methods_id.name.id);
                let class_id = native_methods_id.class.id;
                self.strings.gray.push(class_id);
            }
            Value::Closure(closure) => {
                if self.log_gc {
                    eprintln!(
                        "Function/{:?} gray {}",
                        closure.function.id, *closure.function
                    );
                }
                self.functions.gray.push(closure.function.id);
                self.values.gray.append(
                    &mut closure
                        .upvalues
                        .iter()
                        .map(|uv| {
                            if self.log_gc {
                                eprintln!("Value/{:?} gray {}", uv.id, **uv);
                            }
                            uv.id
                        })
                        .collect(),
                );
            }
            Value::Upvalue(Upvalue::Closed(value_id)) => {
                if self.log_gc {
                    eprintln!("Value/{:?} gray {}", value_id.id, **value_id);
                }
                self.values.gray.push(value_id.id);
            }
            Value::Class(c) => {
                if self.log_gc {
                    eprintln!("String/{:?} gray {}", c.name.id, *c.name);
                }
                self.strings.gray.push(c.name.id);
                let method_ids = c
                    .methods
                    .iter()
                    .map(|(n, c)| {
                        if self.log_gc {
                            eprintln!("String/{:?} gray {}", n.id, **n);
                            eprintln!("Value/{:?} gray {}", c.id, **c);
                        }
                        (n.id, c.id)
                    })
                    .collect::<Vec<_>>();
                for (method_name, closure) in method_ids {
                    self.strings.gray.push(method_name);
                    self.values.gray.push(closure);
                }
            }
            Value::Instance(instance) => {
                let mut fields = instance
                    .fields
                    .values()
                    .map(|value| {
                        if self.log_gc {
                            eprintln!("Value/{:?} gray {}", value.id, **value);
                        }
                        value.id
                    })
                    .collect();
                let class_id = instance.class.id;
                self.values.gray.append(&mut fields);
                if self.log_gc {
                    eprintln!("Value/{:?} gray {}", class_id, *instance.class);
                }
                self.values.gray.push(class_id);
            }
            Value::BoundMethod(bound_method) => {
                let receiver_id = bound_method.receiver.id;
                let method_id = bound_method.method.id;
                if self.log_gc {
                    eprintln!("Value/{:?} gray {}", receiver_id, *bound_method.receiver);
                    eprintln!("Value/{:?} gray {}", method_id, *bound_method.method);
                }
                self.values.gray.push(receiver_id);
                self.values.gray.push(method_id);
            }
            Value::List(list) => {
                let class_id = list.class.id;
                if self.log_gc {
                    eprintln!("Value/{:?} gray {}", class_id, *list.class);
                }
                self.values.gray.push(class_id);
                for item in &list.items {
                    if self.log_gc {
                        eprintln!("Value/{:?} gray {}", item.id, **item);
                    }
                    self.values.gray.push(item.id);
                }
            }
        }
        if self.log_gc {
            eprintln!("Value/{:?} blacken {} end", index, item.item);
        }
    }

    pub fn blacken_string(&mut self, index: StringKey) {
        if self.log_gc {
            eprintln!("String/{:?} blacken {} start", index, self.strings[index]);
        }
        self.strings.mark_raw(index, self.black_value);
        if self.log_gc {
            eprintln!("String/{:?} blacken {} end", index, self.strings[index]);
        }
    }

    fn blacken_function(&mut self, index: FunctionKey) {
        let item = &mut self.functions.data[index];
        if item.marked == self.black_value {
            return;
        }
        if self.log_gc {
            eprintln!("Function/{:?} blacken {} start", index, item.item);
        }

        if self.log_gc {
            eprintln!("Function/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;
        self.functions.gray.push(index);
        let function = &item.item;
        self.strings.gray.push(function.name.id);
        for constant in function.chunk.constants() {
            self.values.gray.push(constant.id);
        }
        self.functions.mark_raw(index, self.black_value);
        if self.log_gc {
            eprintln!("Function/{:?} blacken {} end", index, self.functions[index]);
        }
    }

    pub fn sweep(&mut self) {
        if self.log_gc {
            eprintln!("-- sweep start");
        }

        let before = self.bytes_allocated();
        self.values.sweep(self.black_value);
        self.functions.sweep(self.black_value);
        self.strings.sweep(self.black_value);
        self.black_value = !self.black_value;

        self.next_gc = self.bytes_allocated() * crate::config::GC_HEAP_GROW_FACTOR;
        if self.log_gc {
            eprintln!("-- gc end");
            eprintln!(
                "   collected {} (from {} to {}) next at {}",
                humansize::format_size(before - self.bytes_allocated(), humansize::BINARY),
                humansize::format_size(before, humansize::BINARY),
                humansize::format_size(self.bytes_allocated(), humansize::BINARY),
                humansize::format_size(self.next_gc, humansize::BINARY),
            );
        }
    }

    pub fn add_value(&mut self, value: Value) -> ValueId {
        self.values.add(value, self.black_value)
    }

    pub fn add_string(&mut self, value: String) -> StringId {
        self.strings.add(value, self.black_value)
    }

    pub fn add_function(&mut self, value: Function) -> FunctionId {
        self.functions.add(value, self.black_value)
    }
}
