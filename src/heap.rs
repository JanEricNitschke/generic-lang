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

use crate::value::{
    BoundMethod, Class, Closure, Function, Instance, List, NativeFunction, NativeMethod, Upvalue,
    Value,
};

pub trait ArenaValue: Debug + Display + PartialEq {}
impl<T> ArenaValue for T where T: Debug + Display + PartialEq {}

new_key_type! {
    pub struct ValueKey;
    pub struct FunctionKey;
    pub struct StringKey;
    pub struct UpvalueKey;
    pub struct ClosureKey;
    pub struct NativeFunctionKey;
    pub struct NativeMethodKey;
    pub struct ClassKey;
    pub struct InstanceKey;
    pub struct ListKey;
    pub struct BoundMethodKey;
}

#[derive(Clone, Debug, PartialOrd, Derivative)]
#[derivative(Hash)]
pub struct ArenaId<K: Key, T: ArenaValue> {
    id: K,
    #[derivative(Hash = "ignore")]
    pub arena: NonNull<Arena<K, T>>, // Yes this is terrible, yes I'm OK with it for this projec
}

impl<K: Key, T: ArenaValue> PartialEq for ArenaId<K, T> {
    fn eq(&self, other: &Self) -> bool {
        // Two different bound methods are always considered different
        self.id == other.id || **self == **other
    }
}

impl<K: Key, T: ArenaValue> Eq for ArenaId<K, T> {}

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

pub type StringId = ArenaId<StringKey, String>;
pub type UpvalueId = ArenaId<UpvalueKey, Upvalue>;
pub type FunctionId = ArenaId<FunctionKey, Function>;
pub type ClosureId = ArenaId<ClosureKey, Closure>;
pub type NativeFunctionId = ArenaId<NativeFunctionKey, NativeFunction>;
pub type NativeMethodId = ArenaId<NativeMethodKey, NativeMethod>;
pub type ClassId = ArenaId<ClassKey, Class>;
pub type InstanceId = ArenaId<InstanceKey, Instance>;
pub type ListId = ArenaId<ListKey, List>;
pub type BoundMethodId = ArenaId<BoundMethodKey, BoundMethod>;

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuiltinConstants {
    pub init_string: StringId,
    pub script_name: StringId,
}

impl BuiltinConstants {
    #[must_use]
    pub fn new(heap: &mut Heap) -> Self {
        Self {
            init_string: heap.string_id(&"__init__".to_string()),
            script_name: heap.string_id(&"__name__".to_string()),
        }
    }
}

// Has to be a macro because of mutable borrow of self
macro_rules! gray_value {
    ($self:expr, $value:expr) => {
        match $value {
            Value::Bool(_) | Value::Nil | Value::Number(_) => {}
            Value::Upvalue(id) => {
                if $self.log_gc {
                    eprintln!("Upvalue/{:?} gray {}", id.id, **id);
                }
                $self.upvalues.gray.push(id.id);
            }
            Value::String(id) => {
                if $self.log_gc {
                    eprintln!("String/{:?} gray {}", id.id, **id);
                }
                $self.strings.gray.push(id.id);
            }
            Value::Function(id) => {
                if $self.log_gc {
                    eprintln!("Function/{:?} gray {}", id.id, **id);
                }
                $self.functions.gray.push(id.id);
            }
            Value::NativeFunction(id) => {
                if $self.log_gc {
                    eprintln!("NativeFunction/{:?} gray {}", id.id, **id);
                }
                $self.native_functions.gray.push(id.id);
            }
            Value::NativeMethod(id) => {
                if $self.log_gc {
                    eprintln!("NativeMethod/{:?} gray {}", id.id, **id);
                }
                $self.native_methods.gray.push(id.id);
            }
            Value::Closure(id) => {
                if $self.log_gc {
                    eprintln!("Closure/{:?} gray {}", id.id, **id);
                }
                $self.closures.gray.push(id.id);
            }
            Value::Class(id) => {
                if $self.log_gc {
                    eprintln!("Class/{:?} gray {}", id.id, **id);
                }
                $self.classes.gray.push(id.id);
            }
            Value::Instance(id) => {
                if $self.log_gc {
                    eprintln!("Instance/{:?} gray {}", id.id, **id);
                }
                $self.instances.gray.push(id.id);
            }
            Value::List(id) => {
                if $self.log_gc {
                    eprintln!("List/{:?} gray {}", id.id, **id);
                }
                $self.lists.gray.push(id.id);
            }
            Value::BoundMethod(id) => {
                if $self.log_gc {
                    eprintln!("BoundMethod/{:?} gray {}", id.id, **id);
                }
                $self.bound_methods.gray.push(id.id);
            }
        }
    };
}

#[derive(Clone, Debug)]
pub struct Heap {
    builtin_constants: Option<BuiltinConstants>,
    pub strings_by_name: HashMap<String, StringId>,
    pub native_classes: HashMap<String, Value>,
    pub strings: Arena<StringKey, String>,

    pub values: Arena<ValueKey, Value>,
    pub functions: Arena<FunctionKey, Function>,
    pub bound_methods: Arena<BoundMethodKey, BoundMethod>,
    pub closures: Arena<ClosureKey, Closure>,
    pub native_functions: Arena<NativeFunctionKey, NativeFunction>,
    pub native_methods: Arena<NativeMethodKey, NativeMethod>,
    pub classes: Arena<ClassKey, Class>,
    pub instances: Arena<InstanceKey, Instance>,
    pub lists: Arena<ListKey, List>,
    pub upvalues: Arena<UpvalueKey, Upvalue>,

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
            bound_methods: Arena::new("BoundMethod", log_gc),
            closures: Arena::new("Closure", log_gc),
            native_functions: Arena::new("NativeFunction", log_gc),
            native_methods: Arena::new("NativeMethod", log_gc),
            classes: Arena::new("Class", log_gc),
            instances: Arena::new("Instance", log_gc),
            lists: Arena::new("List", log_gc),
            upvalues: Arena::new("Upvalue", log_gc),

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
        let string_val = self.add_string(s.to_string());
        let string_id = string_val.as_string();
        self.strings_by_name.insert(s.to_string(), *string_id);
        *string_id
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
    }

    pub fn trace(&mut self) {
        if self.log_gc {
            eprintln!("-- trace start");
        }
        while !self.functions.gray.is_empty()
            || !self.strings.gray.is_empty()
            || !self.values.gray.is_empty()
            || !self.upvalues.gray.is_empty()
            || !self.native_functions.gray.is_empty()
            || !self.native_methods.gray.is_empty()
            || !self.closures.gray.is_empty()
            || !self.classes.gray.is_empty()
            || !self.instances.gray.is_empty()
            || !self.lists.gray.is_empty()
            || !self.bound_methods.gray.is_empty()
        {
            for index in self.strings.flush_gray() {
                self.blacken_string(index);
            }
            for index in self.functions.flush_gray() {
                self.blacken_function(index);
            }
            for index in self.upvalues.flush_gray() {
                self.blacken_upvalue(index);
            }
            for index in self.native_functions.flush_gray() {
                self.blacken_native_function(index);
            }
            for index in self.native_methods.flush_gray() {
                self.blacken_native_method(index);
            }
            for index in self.closures.flush_gray() {
                self.blacken_closure(index);
            }
            for index in self.classes.flush_gray() {
                self.blacken_class(index);
            }
            for index in self.instances.flush_gray() {
                self.blacken_instance(index);
            }
            for index in self.lists.flush_gray() {
                self.blacken_list(index);
            }
            for index in self.bound_methods.flush_gray() {
                self.blacken_bound_method(index);
            }
        }
    }

    pub fn mark_value(&mut self, id: &Value) {
        self.blacken_value(id);
    }

    pub fn mark_string(&mut self, id: &StringId) {
        self.blacken_string(id.id);
    }

    pub fn mark_function(&mut self, id: &FunctionId) {
        self.blacken_function(id.id);
    }

    pub fn mark_upvalue(&mut self, id: &UpvalueId) {
        self.blacken_upvalue(id.id);
    }

    #[allow(clippy::too_many_lines)]
    fn blacken_value(&mut self, index: &Value) {
        match index {
            Value::Bool(_) | Value::Nil | Value::Number(_) => {}
            Value::Upvalue(id) => self.blacken_upvalue(id.id),
            Value::String(id) => self.blacken_string(id.id),
            Value::Function(id) => self.blacken_function(id.id),
            Value::NativeFunction(id) => self.blacken_native_function(id.id),
            Value::NativeMethod(id) => self.blacken_native_method(id.id),
            Value::Closure(id) => self.blacken_closure(id.id),
            Value::Class(id) => self.blacken_class(id.id),
            Value::Instance(id) => self.blacken_instance(id.id),
            Value::List(id) => self.blacken_list(id.id),
            Value::BoundMethod(id) => self.blacken_bound_method(id.id),
        }
    }

    fn blacken_upvalue(&mut self, index: UpvalueKey) {
        let item = &mut self.upvalues.data[index];
        if item.marked == self.black_value {
            return;
        }
        if self.log_gc {
            eprintln!("Upvalue/{:?} blacken {} start", index, item.item);
        }
        if self.log_gc {
            eprintln!("Upvalue{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;
        match &item.item {
            Upvalue::Open(_) => {}
            Upvalue::Closed(value) => {
                gray_value!(self, value);
            }
        }
        if self.log_gc {
            eprintln!("Upvalue/{:?} blacken {} end", index, item.item);
        }
    }

    fn blacken_native_function(&mut self, index: NativeFunctionKey) {
        let item = &mut self.native_functions.data[index];
        if item.marked == self.black_value {
            return;
        }
        if self.log_gc {
            eprintln!("NativeFunction/{:?} blacken {} start", index, item.item);
        }
        if self.log_gc {
            eprintln!("NativeFunction/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;

        let function = &item.item;
        self.strings.gray.push(function.name.id);
        if self.log_gc {
            eprintln!("NativeFunction/{:?} blacken {} end", index, item.item);
        }
    }

    fn blacken_native_method(&mut self, index: NativeMethodKey) {
        let item = &mut self.native_methods.data[index];
        if item.marked == self.black_value {
            return;
        }
        if self.log_gc {
            eprintln!("NativeMethod/{:?} blacken {} start", index, item.item);
        }
        if self.log_gc {
            eprintln!("NativeMethod/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;

        let function = &item.item;
        self.strings.gray.push(function.name.id);
        self.strings.gray.push(function.class.id);
        if self.log_gc {
            eprintln!("NativeMethod/{:?} blacken {} end", index, item.item);
        }
    }

    fn blacken_closure(&mut self, index: ClosureKey) {
        let item = &mut self.closures.data[index];
        if item.marked == self.black_value {
            return;
        }
        if self.log_gc {
            eprintln!("Closure/{:?} blacken {} start", index, item.item);
        }

        if self.log_gc {
            eprintln!("Closure/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;

        let closure = &item.item;
        self.functions.gray.push(closure.function.id);
        for upvalue in &closure.upvalues {
            self.upvalues.gray.push(upvalue.id);
        }
        if self.log_gc {
            eprintln!("Closure/{:?} blacken {} end", index, item.item);
        }
    }

    fn blacken_class(&mut self, index: ClassKey) {
        let item = &mut self.classes.data[index];
        if item.marked == self.black_value {
            return;
        }
        if self.log_gc {
            eprintln!("Class/{:?} blacken {} start", index, item.item);
        }

        if self.log_gc {
            eprintln!("Class/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;

        let class = &item.item;
        self.strings.gray.push(class.name.id);
        for (method_name, method) in &class.methods {
            self.strings.gray.push(method_name.id);
            gray_value!(self, method);
        }
        if self.log_gc {
            eprintln!("Class/{:?} blacken {} end", index, item.item);
        }
    }

    fn blacken_instance(&mut self, index: InstanceKey) {
        let item = &mut self.instances.data[index];
        if item.marked == self.black_value {
            return;
        }
        if self.log_gc {
            eprintln!("Instance/{:?} blacken {} start", index, item.item);
        }

        if self.log_gc {
            eprintln!("Instance/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;

        let instance = &item.item;
        self.strings.gray.push(instance.class.name.id);
        for field in instance.fields.values() {
            gray_value!(self, field);
        }
        if self.log_gc {
            eprintln!("Instance/{:?} blacken {} end", index, item.item);
        }
    }

    fn blacken_bound_method(&mut self, index: BoundMethodKey) {
        let item = &mut self.bound_methods.data[index];
        if item.marked == self.black_value {
            return;
        }
        if self.log_gc {
            eprintln!("BoundMethod/{:?} blacken {} start", index, item.item);
        }

        if self.log_gc {
            eprintln!("BoundMethod/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;

        let bound_method = &item.item;
        gray_value!(self, &bound_method.receiver);
        gray_value!(self, &bound_method.method);
        if self.log_gc {
            eprintln!("BoundMethod/{:?} blacken {} end", index, item.item);
        }
    }

    fn blacken_list(&mut self, index: ListKey) {
        let item = &mut self.lists.data[index];
        if item.marked == self.black_value {
            return;
        }
        if self.log_gc {
            eprintln!("List/{:?} blacken {} start", index, item.item);
        }

        if self.log_gc {
            eprintln!("List/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;

        let list = &item.item;
        self.strings.gray.push(list.class.name.id);
        for item in &list.items {
            gray_value!(self, item);
        }
        if self.log_gc {
            eprintln!("List/{:?} blacken {} end", index, item.item);
        }
    }

    fn blacken_string(&mut self, index: StringKey) {
        let item = &mut self.strings.data[index];
        if item.marked == self.black_value {
            return;
        }
        if self.log_gc {
            eprintln!("String/{:?} blacken {} start", index, item.item);
        }
        if self.log_gc {
            eprintln!("String/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;
        if self.log_gc {
            eprintln!("String/{:?} blacken {} end", index, item.item);
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
            gray_value!(self, constant);
        }
        if self.log_gc {
            eprintln!("Function/{:?} blacken {} end", index, item.item);
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

    pub fn add_string(&mut self, value: String) -> Value {
        self.strings.add(value, self.black_value).into()
    }

    pub fn add_function(&mut self, value: Function) -> Value {
        self.functions.add(value, self.black_value).into()
    }

    pub fn add_bound_method(&mut self, value: BoundMethod) -> Value {
        self.bound_methods.add(value, self.black_value).into()
    }

    pub fn add_closure(&mut self, value: Closure) -> Value {
        self.closures.add(value, self.black_value).into()
    }

    pub fn add_native_function(&mut self, value: NativeFunction) -> Value {
        self.native_functions.add(value, self.black_value).into()
    }

    pub fn add_native_method(&mut self, value: NativeMethod) -> Value {
        self.native_methods.add(value, self.black_value).into()
    }

    pub fn add_class(&mut self, value: Class) -> Value {
        self.classes.add(value, self.black_value).into()
    }

    pub fn add_instance(&mut self, value: Instance) -> Value {
        self.instances.add(value, self.black_value).into()
    }

    pub fn add_list(&mut self, value: List) -> Value {
        self.lists.add(value, self.black_value).into()
    }

    pub fn add_upvalue(&mut self, value: Upvalue) -> Value {
        self.upvalues.add(value, self.black_value).into()
    }
}
