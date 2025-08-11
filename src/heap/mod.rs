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

mod arenas;

use num_bigint::BigInt;
use paste::paste;
use rustc_hash::FxHashMap as HashMap;
use std::collections::hash_map::Entry;
use std::fmt::Debug;

use crate::heap::arenas::Arena;
pub use crate::heap::arenas::{
    BigIntId, BoundMethodId, ClassId, ClosureId, FunctionId, InstanceId, ModuleId,
    NativeFunctionId, NativeMethodId, StringId, UpvalueId,
};
use crate::value::{
    BoundMethod, Class, Closure, Function, GenericInt, Instance, Module, NativeClass,
    NativeFunction, NativeMethod, Number, Upvalue, Value,
};

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
            Value::Upvalue(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("Upvalue/{:?} gray {}", id, $self.upvalues[*id]);
                }
                $self.upvalues.gray.push(*id);
            }
            Value::String(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("String/{:?} gray {}", id, $self.strings[*id]);
                }
                $self.strings.gray.push(*id);
            }
            Value::Number(Number::Integer(GenericInt::Big(id))) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("BigInt/{:?} gray {}", id, $self.big_ints[*id]);
                }
                $self.big_ints.gray.push(*id);
            }
            Value::Number(Number::Rational(rational)) => {
                let numerator = rational.numerator();
                let denominator = rational.denominator();
                #[cfg(feature = "log_gc")]
                {
                    eprintln!(
                        "Rational/{:?} gray {}",
                        numerator, $self.rationals[*numerator]
                    );
                }
                if let GenericInt::Big(num) = numerator {
                    #[cfg(feature = "log_gc")]
                    {
                        eprintln!("BigInt/{:?} gray {}", num, $self.big_ints[*num]);
                    }
                    $self.big_ints.gray.push(num);
                }
                if let GenericInt::Big(denom) = denominator {
                    #[cfg(feature = "log_gc")]
                    {
                        eprintln!("BigInt/{:?} gray {}", denom, $self.big_ints[*denom]);
                    }
                    $self.big_ints.gray.push(denom);
                }
            }
            Value::Function(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("Function/{:?} gray {}", id, $self.functions[*id]);
                }
                $self.functions.gray.push(*id);
            }
            Value::NativeFunction(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!(
                        "NativeFunction/{:?} gray {}",
                        id, $self.native_functions[*id]
                    );
                }
                $self.native_functions.gray.push(*id);
            }
            Value::NativeMethod(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("NativeMethod/{:?} gray {}", id, $self.native_methods[*id]);
                }
                $self.native_methods.gray.push(*id);
            }
            Value::Closure(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("Closure/{:?} gray {}", id, $self.closures[*id]);
                }
                $self.closures.gray.push(*id);
            }
            Value::Class(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("Class/{:?} gray {}", id, $self.classes[*id]);
                }
                $self.classes.gray.push(*id);
            }
            Value::Instance(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("Instance/{:?} gray {}", *id, $self.instances[*id]);
                }
                $self.instances.gray.push(*id);
            }
            Value::BoundMethod(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("BoundMethod/{:?} gray {}", id, $self.bound_methods[*id]);
                }
                $self.bound_methods.gray.push(*id);
            }
            Value::Module(id) => {
                #[cfg(feature = "log_gc")]
                {
                    eprintln!("Module/{:?} gray {}", id, $self.modules[*id]);
                }
                $self.modules.gray.push(*id);
            }
            Value::Bool(_)
            | Value::Nil
            | Value::StopIteration
            | Value::Number(Number::Integer(GenericInt::Small(_)) | Number::Float(_)) => {}
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

    pub(super) strings: Arena<StringId, String>,
    big_ints: Arena<BigIntId, BigInt>,
    functions: Arena<FunctionId, Function>,
    bound_methods: Arena<BoundMethodId, BoundMethod>,
    closures: Arena<ClosureId, Closure>,
    native_functions: Arena<NativeFunctionId, NativeFunction>,
    native_methods: Arena<NativeMethodId, NativeMethod>,
    classes: Arena<ClassId, Class>,
    instances: Arena<InstanceId, Instance>,
    upvalues: Arena<UpvalueId, Upvalue>,
    modules: Arena<ModuleId, Module>,

    next_gc: usize,
    pub(super) black_value: bool,
}

impl Heap {
    pub(super) fn new() -> Self {
        let strings_by_name: HashMap<String, StringId> = HashMap::default();

        let mut heap = Self {
            builtin_constants: None,
            strings_by_name,
            native_classes: HashMap::default(),

            strings: Arena::new("String"),
            big_ints: Arena::new("BigInt"),
            functions: Arena::new("Function"),
            bound_methods: Arena::new("BoundMethod"),
            closures: Arena::new("Closure"),
            native_functions: Arena::new("NativeFunction"),
            native_methods: Arena::new("NativeMethod"),
            classes: Arena::new("Class"),
            instances: Arena::new("Instance"),
            upvalues: Arena::new("Upvalue"),
            modules: Arena::new("Module"),

            next_gc: 1024 * 1024,
            black_value: true,
        };
        // Very important: first pin, *then* initialize the constants, as the `ArenaId`s generated
        // here will carry a raw pointer that needs to remain valid
        heap.builtin_constants = Some(BuiltinConstants::new(&mut heap));
        let init_string_id = heap.builtin_constants().init_string;
        let init_string = heap.strings[init_string_id].clone();
        heap.strings_by_name.insert(init_string, init_string_id);

        heap
    }

    pub(super) const fn builtin_constants(&self) -> &BuiltinConstants {
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
            + self.big_ints.bytes_allocated()
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

        self.strings.gray.push(self.builtin_constants().init_string);
        self.strings.gray.push(self.builtin_constants().script_name);

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
            || !self.big_ints.gray.is_empty()
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
            for index in self.big_ints.flush_gray() {
                self.blacken_big_int(index);
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

    pub(super) fn mark_function(&mut self, id: FunctionId) {
        self.blacken_function(id);
    }

    pub(super) fn mark_upvalue(&mut self, id: UpvalueId) {
        self.blacken_upvalue(id);
    }

    pub(super) fn mark_module(&mut self, id: ModuleId) {
        self.blacken_module(id);
    }

    fn blacken_value(&mut self, index: &Value) {
        match index {
            Value::Upvalue(id) => self.blacken_upvalue(*id),
            Value::String(id) => self.blacken_string(*id),
            Value::Number(Number::Integer(GenericInt::Big(id))) => self.blacken_big_int(*id),
            Value::Function(id) => self.blacken_function(*id),
            Value::NativeFunction(id) => self.blacken_native_function(*id),
            Value::NativeMethod(id) => self.blacken_native_method(*id),
            Value::Closure(id) => self.blacken_closure(*id),
            Value::Class(id) => self.blacken_class(*id),
            Value::Instance(id) => self.blacken_instance(*id),
            Value::BoundMethod(id) => self.blacken_bound_method(*id),
            Value::Module(id) => self.blacken_module(*id),
            Value::Bool(_) | Value::Nil | Value::StopIteration | Value::Number(_) => {}
        }
    }

    /// Closed upvalues refer to a separate value that has to be marked.
    ///
    /// Open ones do not contain any data that is stored on the heap.
    fn blacken_upvalue(&mut self, index: UpvalueId) {
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
        #[cfg(feature = "log_gc")]
        let item = item.clone();
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
    fn blacken_module(&mut self, index: ModuleId) {
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
        #[cfg(feature = "log_gc")]
        let item = item.clone();
        let module = &item.item;
        self.strings.gray.push(module.name);
        for value in module.globals.values() {
            gray_value!(self, &value.value);
        }
    }

    /// Native functions only have their name on the heap.
    /// The implementation is directly in rust.
    fn blacken_native_function(&mut self, index: NativeFunctionId) {
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
        self.strings.gray.push(function.name);
        #[cfg(feature = "log_gc")]
        {
            eprintln!("NativeFunction/{:?} blacken {} end", index, item.item);
        }
    }

    /// Native methods store their name as well as the class they belong to.
    fn blacken_native_method(&mut self, index: NativeMethodId) {
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
        self.strings.gray.push(function.name);
        self.strings.gray.push(function.class);
        #[cfg(feature = "log_gc")]
        {
            eprintln!("NativeMethod/{:?} blacken {} end", index, item.item);
        }
    }

    /// Closures store their wrapped function ad well as the captured upvalues.
    fn blacken_closure(&mut self, index: ClosureId) {
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
        self.functions.gray.push(closure.function);
        for upvalue in &closure.upvalues {
            self.upvalues.gray.push(*upvalue);
        }
        if let Some(module) = closure.containing_module {
            self.modules.gray.push(module);
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("Closure/{:?} blacken {} end", index, item.item);
        }
    }

    /// Classes store their name as well as their methods with their names.
    fn blacken_class(&mut self, index: ClassId) {
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
        #[cfg(feature = "log_gc")]
        let item = item.clone();
        let class = &item.item;
        self.strings.gray.push(class.name);
        for (method_name, method) in &class.methods {
            self.strings.gray.push(*method_name);
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
    fn blacken_instance(&mut self, index: InstanceId) {
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
        #[cfg(feature = "log_gc")]
        let item = item.clone();
        let instance = &item.item;
        let class = &self.classes[instance.class];
        self.strings.gray.push(class.name);
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
                    self.instances.gray.push(list_iter.list);
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
                NativeClass::Range(range) => {
                    gray_value!(self, &range.start().into());
                    gray_value!(self, &range.end().into());
                }
                NativeClass::RangeIterator(range_iter) => {
                    self.instances.gray.push(range_iter.range);
                    if let GenericInt::Big(offset) = range_iter.offset {
                        #[cfg(feature = "log_gc")]
                        {
                            eprintln!("BigInt/{:?} gray {}", offset, self.big_ints[offset]);
                        }
                        self.big_ints.gray.push(offset);
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
    fn blacken_bound_method(&mut self, index: BoundMethodId) {
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
        #[cfg(feature = "log_gc")]
        let item = item.clone();
        let bound_method = &item.item;
        gray_value!(self, &bound_method.receiver);
        gray_value!(self, &bound_method.method);
        #[cfg(feature = "log_gc")]
        {
            eprintln!("BoundMethod/{:?} blacken {} end", index, item.item);
        }
    }

    /// Strings dont actually contain anything else.
    fn blacken_string(&mut self, index: StringId) {
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

    /// `BigInts` only contain the actual number.
    fn blacken_big_int(&mut self, index: BigIntId) {
        let item = &mut self.big_ints.data[index];
        if item.marked == self.black_value {
            return;
        }
        #[cfg(feature = "log_gc")]
        {
            eprintln!("BigInt/{:?} blacken {} start", index, item.item);
            eprintln!("BigInt/{index:?} mark {}", item.item);
        }
        item.marked = self.black_value;
        #[cfg(feature = "log_gc")]
        {
            eprintln!("BigInt/{:?} blacken {} end", index, item.item);
        }
    }

    /// Function contain their own name as well as a list of constants,
    /// although none of these should currently be heap allocated.
    fn blacken_function(&mut self, index: FunctionId) {
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
        #[cfg(feature = "log_gc")]
        let item = item.clone();
        self.functions.gray.push(index);
        let function = &item.item;
        self.strings.gray.push(function.name);
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

        // Need to sweep closures before functions as
        // the former prints the latter on `log_gc`.
        // Also have to sweep strings last as modules and
        // functions use their name on their debug display.
        self.modules.sweep(self.black_value);
        self.closures.sweep(self.black_value);
        self.functions.sweep(self.black_value);
        self.bound_methods.sweep(self.black_value);
        self.upvalues.sweep(self.black_value);
        self.native_functions.sweep(self.black_value);
        self.native_methods.sweep(self.black_value);
        self.classes.sweep(self.black_value);
        self.instances.sweep(self.black_value);
        self.big_ints.sweep(self.black_value);
        self.strings.sweep(self.black_value);

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
}

macro_rules! define_value_methods {
    ($(
        $slot_name:ident => {
            field: $field_name:ident,
            ty: $ty:ty,
            id_ty: $id_ty:ty
        }
    ),* $(,)?) => {
        paste! {
        $(
            // e.g. pub(super) fn add_string(&mut self, value: String) -> Value
            pub(super) fn [<add_$slot_name>](&mut self, value: $ty) -> Value {
                self.$field_name.add(value, self.black_value).into()
            }

            // e.g. pub(super) get_string(&self, index: StringId) -> &String
            pub(super) fn [<get_$slot_name>](&self, index: $id_ty) -> &$ty {
                self.$field_name.get(index)
            }

            // e.g. pub(super) get_mut_string(&mut self, index: StringId) -> &mut String
            pub(super) fn [<get_mut_$slot_name>](&mut self, index: $id_ty) -> &mut $ty {
                self.$field_name.get_mut(index)
            }

            // e.g. pub(super) string_marked(&self, index: StringId) -> bool
            pub(super) fn [< $slot_name _marked>](&self, index: $id_ty) -> bool {
                self.$field_name.is_marked(index, self.black_value)
            }
        )*
    }
}}

impl Heap {
    define_value_methods!(
        string => {
            field: strings,
            ty: String,
            id_ty: StringId
        },
        big_int => {
            field: big_ints,
            ty: BigInt,
            id_ty: BigIntId
        },
        function => {
            field: functions,
            ty: Function,
            id_ty: FunctionId
        },
        bound_method => {
            field: bound_methods,
            ty: BoundMethod,
            id_ty: BoundMethodId
        },
        closure => {
            field: closures,
            ty: Closure,
            id_ty: ClosureId
        },
        native_function => {
            field: native_functions,
            ty: NativeFunction,
            id_ty: NativeFunctionId
        },
        native_method => {
            field: native_methods,
            ty: NativeMethod,
            id_ty: NativeMethodId
        },
        class => {
            field: classes,
            ty: Class,
            id_ty: ClassId
        },
        instance => {
            field: instances,
            ty: Instance,
            id_ty: InstanceId
        },
        upvalue => {
            field: upvalues,
            ty: Upvalue,
            id_ty: UpvalueId
        },
        module => {
            field: modules,
            ty: Module,
            id_ty: ModuleId
        },
    );
}
