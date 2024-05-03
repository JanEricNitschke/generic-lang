//! Handles the allocation of most complex types.
//!
//! While booleans, `nil` and `StopIteration` are stored directly on the stack,
//! all other objects only have references stored there.
//! The actual objects leave in the heap.
//!
//! The heap is managed via arenas for each type of `Value`.
//!
//! Currently, for ease of use, each value holds an `Id` that wraps
//! the key of the object in its respective arena as well as a pointer to the arena.
//! This makes for short syntax via `deref` and allows the retrieval of the objects
//! without direct access to the heap instance.
//!
//! However it requires the use of unsafe and `Pin` and also has a memory
//! overhead as each `Id` has to store the key AND the pointer.
//!
//! Garbage collection occurs via `mark and sweep`.

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
    BoundMethod, Class, Closure, Function, Instance, Module, NativeClass, NativeFunction,
    NativeMethod, Upvalue, Value,
};

pub trait ArenaValue: Debug + Display + PartialEq {}
impl<T> ArenaValue for T where T: Debug + Display + PartialEq {}

// Define separate key types for each `Value` variant to ensure
// that no misuse occurs.
new_key_type! {
    pub struct FunctionKey;
    pub struct StringKey;
    pub struct UpvalueKey;
    pub struct ClosureKey;
    pub struct NativeFunctionKey;
    pub struct NativeMethodKey;
    pub struct ClassKey;
    pub struct InstanceKey;
    pub struct BoundMethodKey;
    pub struct ModuleKey;
}

/// Wrapper for the key of the object as well as the arena that contains it.
#[derive(Clone, PartialOrd, Debug, Derivative)]
#[derivative(Hash)]
pub struct ArenaId<K: Key, T: ArenaValue> {
    id: K,
    // Yes this is terrible, yes I'm OK with it for this project
    // These could be but then i would have to put `clones` everywhere.
    // use std::cell::RefCell;
    // use std::rc::Rc;
    // arena: Rc<RefCell<Arena<K, T>>>,
    arena: NonNull<Arena<K, T>>,
}

impl<K: Key, T: ArenaValue> PartialEq for ArenaId<K, T> {
    fn eq(&self, other: &Self) -> bool {
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
    pub(super) fn marked(&self, black_value: bool) -> bool {
        unsafe { self.arena.as_ref().is_marked(self.id, black_value) }
    }
}

/// Wrapper attaching a flag indicating whether an object
/// has been marked during the mark phase of mark and sweep
/// garbage collection.
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

// Separate Id types for each `Value` variant.
pub type StringId = ArenaId<StringKey, String>;
pub type UpvalueId = ArenaId<UpvalueKey, Upvalue>;
pub type FunctionId = ArenaId<FunctionKey, Function>;
pub type ClosureId = ArenaId<ClosureKey, Closure>;
pub type NativeFunctionId = ArenaId<NativeFunctionKey, NativeFunction>;
pub type NativeMethodId = ArenaId<NativeMethodKey, NativeMethod>;
pub type ClassId = ArenaId<ClassKey, Class>;
pub type InstanceId = ArenaId<InstanceKey, Instance>;
pub type BoundMethodId = ArenaId<BoundMethodKey, BoundMethod>;
pub type ModuleId = ArenaId<ModuleKey, Module>;

/// Arenas storing each `Value` variant.
///
/// Each arena has a name when logging the garbage collector.
/// The main core of the arenas is a `HopSlotMap` storing the actual values.
/// Additionally, they store their overall number of allocated bytes as well
/// as a vector of items to process for `mark and sweep`.
#[derive(Clone, Debug)]
pub struct Arena<K: Key, V: ArenaValue> {
    #[cfg(feature = "log_gc")]
    name: &'static str,

    data: SlotMap<K, Item<V>>,
    bytes_allocated: usize,

    gray: Vec<K>,
}

impl<K: Key, V: ArenaValue> Arena<K, V> {
    #[must_use]
    fn new(#[cfg(feature = "log_gc")] name: &'static str) -> Self {
        Self {
            #[cfg(feature = "log_gc")]
            name,
            data: SlotMap::with_key(),
            bytes_allocated: 0,
            gray: Vec::new(),
        }
    }

    /// Add a value to the arena and return a corresponding Id.
    ///
    /// Also update the total number of bytes allocated in this arena.
    fn add(&mut self, value: V, black_value: bool) -> ArenaId<K, V> {
        let id = self.data.insert(Item::new(value, !black_value));
        self.bytes_allocated += std::mem::size_of::<V>();

        #[cfg(feature = "log_gc")]
        {
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

    /// Clear the gray values and return them for processing.
    fn flush_gray(&mut self) -> Vec<K> {
        let capacity = self.gray.capacity();
        std::mem::replace(&mut self.gray, Vec::with_capacity(capacity))
    }

    /// Remove all non-marked values from the arena.
    ///
    /// Also update the total number of allocated bytes.
    fn sweep(&mut self, black_value: bool) {
        self.data.retain(|_key, value| {
            #[cfg(feature = "log_gc")]
            if !(value.marked == black_value) {
                eprintln!("{}/{:?} free {}", self.name, _key, value.item);
            }
            value.marked == black_value
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

/// Collection of all builtin constants that are needed in different parts
/// of the heap or VM.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuiltinConstants {
    /// Name of the initializer, currently `__init__`
    pub(super) init_string: StringId,
    /// Identifier that contains the name of the current module.
    /// Currently `__name__`.
    pub(super) script_name: StringId,
}

impl BuiltinConstants {
    #[must_use]
    fn new(heap: &mut Heap) -> Self {
        Self {
            init_string: heap.string_id(&"__init__".to_string()),
            script_name: heap.string_id(&"__name__".to_string()),
        }
    }
}

/// Switch to add a `Value` to the gray vector of the correct arena.
///
/// Needs to be a macro because it is used in multiple places and a function
/// runs into issues with the borrow checker.
macro_rules! gray_value {
    ($self:expr, $value:expr) => {
        match $value {
            Value::Bool(_) | Value::Nil | Value::StopIteration | Value::Number(_) => {}
            Value::Upvalue(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("Upvalue/{:?} gray {}", id.id, **id);
                }
                $self.upvalues.gray.push(id.id);
            }
            Value::String(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("String/{:?} gray {}", id.id, **id);
                }
                $self.strings.gray.push(id.id);
            }
            Value::Function(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("Function/{:?} gray {}", id.id, **id);
                }
                $self.functions.gray.push(id.id);
            }
            Value::NativeFunction(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("NativeFunction/{:?} gray {}", id.id, **id);
                }
                $self.native_functions.gray.push(id.id);
            }
            Value::NativeMethod(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("NativeMethod/{:?} gray {}", id.id, **id);
                }
                $self.native_methods.gray.push(id.id);
            }
            Value::Closure(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("Closure/{:?} gray {}", id.id, **id);
                }
                $self.closures.gray.push(id.id);
            }
            Value::Class(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("Class/{:?} gray {}", id.id, **id);
                }
                $self.classes.gray.push(id.id);
            }
            Value::Instance(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("Instance/{:?} gray {}", id.id, **id);
                }
                $self.instances.gray.push(id.id);
            }
            Value::BoundMethod(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("BoundMethod/{:?} gray {}", id.id, **id);
                }
                $self.bound_methods.gray.push(id.id);
            }
            Value::Module(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("Module/{:?} gray {}", id.id, **id);
                }
                $self.modules.gray.push(id.id);
            }
        }
    };
}

/// Main representation of the heap in generic.
///
/// Heart is multiple arenas, one for each variant of `Value.`
#[derive(Clone, Debug)]
pub struct Heap {
    builtin_constants: Option<BuiltinConstants>,
    pub(super) strings_by_name: HashMap<String, StringId>,
    pub(super) native_classes: HashMap<String, Value>,

    pub(super) strings: Arena<StringKey, String>,
    functions: Arena<FunctionKey, Function>,
    bound_methods: Arena<BoundMethodKey, BoundMethod>,
    closures: Arena<ClosureKey, Closure>,
    native_functions: Arena<NativeFunctionKey, NativeFunction>,
    native_methods: Arena<NativeMethodKey, NativeMethod>,
    classes: Arena<ClassKey, Class>,
    instances: Arena<InstanceKey, Instance>,
    upvalues: Arena<UpvalueKey, Upvalue>,
    modules: Arena<ModuleKey, Module>,

    next_gc: usize,
    pub(super) black_value: bool,
}

impl Heap {
    pub(super) fn new() -> Pin<Box<Self>> {
        let strings_by_name: HashMap<String, StringId> = HashMap::default();

        let mut heap = Box::pin(Self {
            builtin_constants: None,
            strings_by_name,
            native_classes: HashMap::default(),

            strings: Arena::new(
                #[cfg(feature = "log_gc")]
                "String",
            ),
            functions: Arena::new(
                #[cfg(feature = "log_gc")]
                "Function",
            ),
            bound_methods: Arena::new(
                #[cfg(feature = "log_gc")]
                "BoundMethod",
            ),
            closures: Arena::new(
                #[cfg(feature = "log_gc")]
                "Closure",
            ),
            native_functions: Arena::new(
                #[cfg(feature = "log_gc")]
                "NativeFunction",
            ),
            native_methods: Arena::new(
                #[cfg(feature = "log_gc")]
                "NativeMethod",
            ),
            classes: Arena::new(
                #[cfg(feature = "log_gc")]
                "Class",
            ),
            instances: Arena::new(
                #[cfg(feature = "log_gc")]
                "Instance",
            ),
            upvalues: Arena::new(
                #[cfg(feature = "log_gc")]
                "Upvalue",
            ),
            modules: Arena::new(
                #[cfg(feature = "log_gc")]
                "Module",
            ),

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

    pub(super) fn builtin_constants(&self) -> &BuiltinConstants {
        self.builtin_constants.as_ref().unwrap()
    }

    /// Uniquefy string Ids so that each actual string is mapped to the same Id.
    pub(super) fn string_id<S>(&mut self, s: &S) -> StringId
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
        self.strings.bytes_allocated()
            + self.functions.bytes_allocated()
            + self.bound_methods.bytes_allocated()
            + self.closures.bytes_allocated()
            + self.native_functions.bytes_allocated()
            + self.native_methods.bytes_allocated()
            + self.classes.bytes_allocated()
            + self.instances.bytes_allocated()
            + self.upvalues.bytes_allocated()
            + self.modules.bytes_allocated()
    }

    #[cfg(not(feature = "stress_gc"))]
    pub(super) const fn needs_gc(&self) -> bool {
        self.bytes_allocated() > self.next_gc
    }

    /// Prepare the garbage collection by marking all
    /// values used by the heap itself.
    ///
    /// These include the builtin constant as well
    /// as native classes.
    pub(super) fn gc_start(&mut self) {
        #[cfg(feature = "log_gc")]
        {
            eprintln!("-- gc begin");
        }

        self.strings
            .gray
            .push(self.builtin_constants().init_string.id);
        self.strings
            .gray
            .push(self.builtin_constants().script_name.id);

        for value in self.native_classes.values() {
            gray_value!(self, value);
        }
    }

    /// Trace through all reachable values.
    ///
    /// For that repeatedly iterate over all marked values
    /// and mark everything that can be reached by them.
    pub(super) fn trace(&mut self) {
        #[cfg(feature = "log_gc")]
        {
            eprintln!("-- trace start");
        }
        while !self.functions.gray.is_empty()
            || !self.strings.gray.is_empty()
            || !self.upvalues.gray.is_empty()
            || !self.native_functions.gray.is_empty()
            || !self.native_methods.gray.is_empty()
            || !self.closures.gray.is_empty()
            || !self.classes.gray.is_empty()
            || !self.instances.gray.is_empty()
            || !self.bound_methods.gray.is_empty()
            || !self.modules.gray.is_empty()
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
            for index in self.bound_methods.flush_gray() {
                self.blacken_bound_method(index);
            }
            for index in self.modules.flush_gray() {
                self.blacken_module(index);
            }
        }
    }

    pub(super) fn mark_value(&mut self, id: &Value) {
        self.blacken_value(id);
    }

    pub(super) fn mark_function(&mut self, id: &FunctionId) {
        self.blacken_function(id.id);
    }

    pub(super) fn mark_upvalue(&mut self, id: &UpvalueId) {
        self.blacken_upvalue(id.id);
    }

    pub(super) fn mark_module(&mut self, id: &ModuleId) {
        self.blacken_module(id.id);
    }

    fn blacken_value(&mut self, index: &Value) {
        match index {
            Value::Bool(_) | Value::Nil | Value::StopIteration | Value::Number(_) => {}
            Value::Upvalue(id) => self.blacken_upvalue(id.id),
            Value::String(id) => self.blacken_string(id.id),
            Value::Function(id) => self.blacken_function(id.id),
            Value::NativeFunction(id) => self.blacken_native_function(id.id),
            Value::NativeMethod(id) => self.blacken_native_method(id.id),
            Value::Closure(id) => self.blacken_closure(id.id),
            Value::Class(id) => self.blacken_class(id.id),
            Value::Instance(id) => self.blacken_instance(id.id),
            Value::BoundMethod(id) => self.blacken_bound_method(id.id),
            Value::Module(id) => self.blacken_module(id.id),
        }
    }

    /// Closed upvalues refer to a separate value that has to be marked.
    ///
    /// Open ones do not contain any data that is stored on the heap.
    fn blacken_upvalue(&mut self, index: UpvalueKey) {
        let item = &mut self.upvalues.data[index];
        if item.marked == self.black_value {
            return;
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("Upvalue/{:?} blacken {} start", index, item.item);
            eprintln!("Upvalue{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;
        match &item.item {
            Upvalue::Open(_) => {}
            Upvalue::Closed(value) => {
                gray_value!(self, value);
            }
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("Upvalue/{:?} blacken {} end", index, item.item);
        }
    }

    /// Modules need to mark their heap stored name as well as all
    /// the keys and values stored in their globals.
    fn blacken_module(&mut self, index: ModuleKey) {
        let item = &mut self.modules.data[index];
        if item.marked == self.black_value {
            return;
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("Module/{:?} blacken {} start", index, item.item);
            eprintln!("Module/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;
        let module = &item.item;
        self.strings.gray.push(module.name.id);
        for (key, value) in &module.globals {
            self.strings.gray.push(key.id);
            gray_value!(self, &value.value);
        }
    }

    /// Native functions only have their name on the heap.
    /// The implementation is directly in rust.
    fn blacken_native_function(&mut self, index: NativeFunctionKey) {
        let item = &mut self.native_functions.data[index];
        if item.marked == self.black_value {
            return;
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("NativeFunction/{:?} blacken {} start", index, item.item);
            eprintln!("NativeFunction/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;

        let function = &item.item;
        self.strings.gray.push(function.name.id);
        #[cfg(feature = "log_gc")]
        {
            eprintln!("NativeFunction/{:?} blacken {} end", index, item.item);
        }
    }

    /// Native methods store their name as well as the class they belong to.
    fn blacken_native_method(&mut self, index: NativeMethodKey) {
        let item = &mut self.native_methods.data[index];
        if item.marked == self.black_value {
            return;
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("NativeMethod/{:?} blacken {} start", index, item.item);
            eprintln!("NativeMethod/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;

        let function = &item.item;
        self.strings.gray.push(function.name.id);
        self.strings.gray.push(function.class.id);
        #[cfg(feature = "log_gc")]
        {
            eprintln!("NativeMethod/{:?} blacken {} end", index, item.item);
        }
    }

    /// Closures store their wrapped function ad well as the captured upvalues.
    fn blacken_closure(&mut self, index: ClosureKey) {
        let item = &mut self.closures.data[index];
        if item.marked == self.black_value {
            return;
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("Closure/{:?} blacken {} start", index, item.item);
            eprintln!("Closure/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;

        let closure = &item.item;
        self.functions.gray.push(closure.function.id);
        for upvalue in &closure.upvalues {
            self.upvalues.gray.push(upvalue.id);
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("Closure/{:?} blacken {} end", index, item.item);
        }
    }

    /// Classes store their name as well as their methods with their names.
    fn blacken_class(&mut self, index: ClassKey) {
        let item = &mut self.classes.data[index];
        if item.marked == self.black_value {
            return;
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("Class/{:?} blacken {} start", index, item.item);
            eprintln!("Class/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;

        let class = &item.item;
        self.strings.gray.push(class.name.id);
        for (method_name, method) in &class.methods {
            self.strings.gray.push(method_name.id);
            gray_value!(self, method);
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("Class/{:?} blacken {} end", index, item.item);
        }
    }

    // I feel this is still within reason.
    /// Instances store the name of the class they belong to as well as their fields.
    ///
    /// If they represent an instance of a native class, then the data structure
    /// that handles the native functionality may itself reference more heap allocated data.
    #[allow(clippy::cognitive_complexity)]
    fn blacken_instance(&mut self, index: InstanceKey) {
        let item = &mut self.instances.data[index];
        if item.marked == self.black_value {
            return;
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("Instance/{:?} blacken {} start", index, item.item);
            eprintln!("Instance/{index:?} mark {}", item.item);
        }

        item.marked = self.black_value;

        let instance = &item.item;
        self.strings.gray.push(instance.class.name.id);
        for field in instance.fields.values() {
            gray_value!(self, field);
        }

        if let Some(backing) = instance.backing.as_ref() {
            // This will probably go into a macro again at some point.
            match backing {
                NativeClass::List(list) => {
                    for item in &list.items {
                        gray_value!(self, item);
                    }
                }
                NativeClass::ListIterator(list_iter) => {
                    if let Some(list) = &list_iter.list {
                        self.instances.gray.push(list.id);
                    }
                }
                NativeClass::Set(set) => {
                    for item in &set.items {
                        gray_value!(self, item);
                    }
                }
                NativeClass::Dict(dict) => {
                    for (key, value) in &dict.items {
                        gray_value!(self, key);
                        gray_value!(self, value);
                    }
                }
            }
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("Instance/{:?} blacken {} end", index, item.item);
        }
    }

    /// Bound methods store the instance they are bound to
    /// as well as the method they are binding.
    #[allow(clippy::cognitive_complexity)]
    fn blacken_bound_method(&mut self, index: BoundMethodKey) {
        let item = &mut self.bound_methods.data[index];
        if item.marked == self.black_value {
            return;
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("BoundMethod/{:?} blacken {} start", index, item.item);
            eprintln!("BoundMethod/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;

        let bound_method = &item.item;
        gray_value!(self, &bound_method.receiver);
        gray_value!(self, &bound_method.method);
        #[cfg(feature = "log_gc")]
        {
            eprintln!("BoundMethod/{:?} blacken {} end", index, item.item);
        }
    }

    /// Strings dont actually contain anything else.
    fn blacken_string(&mut self, index: StringKey) {
        let item = &mut self.strings.data[index];
        if item.marked == self.black_value {
            return;
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("String/{:?} blacken {} start", index, item.item);
            eprintln!("String/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;
        #[cfg(feature = "log_gc")]
        {
            eprintln!("String/{:?} blacken {} end", index, item.item);
        }
    }

    /// Function contain their own name as well as a list of constants,
    /// although none of these should currently be heap allocated.
    fn blacken_function(&mut self, index: FunctionKey) {
        let item = &mut self.functions.data[index];
        if item.marked == self.black_value {
            return;
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("Function/{:?} blacken {} start", index, item.item);
            eprintln!("Function/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;
        self.functions.gray.push(index);
        let function = &item.item;
        self.strings.gray.push(function.name.id);
        for constant in function.chunk.constants() {
            gray_value!(self, constant);
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("Function/{:?} blacken {} end", index, item.item);
        }
    }

    pub(super) fn sweep(&mut self) {
        #[cfg(feature = "log_gc")]
        eprintln!("-- sweep start");
        #[cfg(feature = "log_gc")]
        let before = self.bytes_allocated();

        self.functions.sweep(self.black_value);
        self.bound_methods.sweep(self.black_value);
        self.closures.sweep(self.black_value);
        self.strings.sweep(self.black_value);
        self.upvalues.sweep(self.black_value);
        self.native_functions.sweep(self.black_value);
        self.native_methods.sweep(self.black_value);
        self.classes.sweep(self.black_value);
        self.instances.sweep(self.black_value);
        self.modules.sweep(self.black_value);

        self.black_value = !self.black_value;

        self.next_gc = self.bytes_allocated() * crate::config::GC_HEAP_GROW_FACTOR;
        #[cfg(feature = "log_gc")]
        {
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

    fn add_string(&mut self, value: String) -> Value {
        self.strings.add(value, self.black_value).into()
    }

    pub(super) fn add_function(&mut self, value: Function) -> Value {
        self.functions.add(value, self.black_value).into()
    }

    pub(super) fn add_bound_method(&mut self, value: BoundMethod) -> Value {
        self.bound_methods.add(value, self.black_value).into()
    }

    pub(super) fn add_closure(&mut self, value: Closure) -> Value {
        self.closures.add(value, self.black_value).into()
    }

    pub(super) fn add_native_function(&mut self, value: NativeFunction) -> Value {
        self.native_functions.add(value, self.black_value).into()
    }

    pub(super) fn add_native_method(&mut self, value: NativeMethod) -> Value {
        self.native_methods.add(value, self.black_value).into()
    }

    pub(super) fn add_class(&mut self, value: Class) -> Value {
        self.classes.add(value, self.black_value).into()
    }

    pub(super) fn add_instance(&mut self, value: Instance) -> Value {
        self.instances.add(value, self.black_value).into()
    }

    pub(super) fn add_upvalue(&mut self, value: Upvalue) -> Value {
        self.upvalues.add(value, self.black_value).into()
    }

    pub(super) fn add_module(&mut self, value: Module) -> Value {
        self.modules.add(value, self.black_value).into()
    }
}
