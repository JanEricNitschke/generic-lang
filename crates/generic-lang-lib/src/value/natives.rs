use crate::vm::ExceptionKind::TypeError;
use crate::{
    heap::{Heap, StringId},
    value::{ClosureId, GenericInt, Instance, InstanceId, is_exception_subclass},
    vm::{
        SuspendedExceptionHandler, VM,
        callstack::CallFrame,
        errors::{VmErrorKind, VmResult},
    },
};

use hashbrown::HashTable;
use hashbrown::hash_table::Entry;

use super::Value;

use derivative::Derivative;

// Values related to natives
#[derive(Derivative)]
#[derivative(Debug, PartialEq, PartialOrd, Clone)]
pub struct NativeFunction {
    pub(crate) name: StringId,
    pub(crate) arity: &'static [u8],

    #[derivative(
            Debug = "ignore",
            // Treat the implementation as always equal; we discriminate built-in functions by name
            PartialEq(compare_with = "always_equals"),
            PartialOrd = "ignore"
        )]
    pub(crate) fun: NativeFunctionImpl,
}

impl NativeFunction {
    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        format!("<native fn {}>", *self.name.to_value(heap))
    }
}

impl std::fmt::Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<native fn Value>")
    }
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, PartialOrd, Clone)]
pub struct NativeMethod {
    pub(crate) class: StringId,
    pub(crate) name: StringId,
    pub(crate) arity: &'static [u8],

    #[derivative(
            Debug = "ignore",
            // Treat the implementation as always equal; we discriminate built-in functions by name
            PartialEq(compare_with = "always_equals"),
            PartialOrd = "ignore"
        )]
    pub(crate) fun: NativeMethodImpl,
}

const fn always_equals<T>(_: &T, _: &T) -> bool {
    true
}

impl NativeMethod {
    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        format!(
            "<native method {} of class {}>",
            *self.name.to_value(heap),
            *self.class.to_value(heap)
        )
    }
}

impl std::fmt::Display for NativeMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<native method Value>")
    }
}

pub type NativeFunctionImpl = fn(&mut VM, &[Value]) -> VmResult<Value>;
pub type NativeMethodImpl = fn(&mut VM, &Value, &[Value]) -> VmResult<Value>;
pub type ModuleContents = Vec<(&'static str, &'static [u8], NativeFunctionImpl)>;

// Actual Natives
#[derive(Debug, Clone, PartialEq)]
pub enum NativeClass {
    List(List),
    ListIterator(ListIterator),
    Set(Set),
    Dict(Dict),
    Range(Range),
    RangeIterator(RangeIterator),
    Tuple(Tuple),
    TupleIterator(TupleIterator),
    Exception(Exception),
    Generator(Generator),
    Template(Template),
    TemplateIterator(TemplateIterator),
    Interpolation(Interpolation),
    // Proxy classes for value type constructors
    BoolProxy,
    StringProxy,
    IntegerProxy,
    FloatProxy,
    RationalProxy,
}

impl NativeClass {
    pub(crate) fn new(kind: &str) -> Self {
        match kind {
            "List" => Self::List(List::default()),
            "Set" => Self::Set(Set::default()),
            "Dict" => Self::Dict(Dict::default()),
            "Range" => Self::Range(Range::default()),
            "Tuple" => Self::Tuple(Tuple::default()),
            "Exception" => Self::Exception(Exception::default()),
            // Proxy classes for value type constructors
            "Bool" => Self::BoolProxy,
            "String" => Self::StringProxy,
            "Integer" => Self::IntegerProxy,
            "Float" => Self::FloatProxy,
            "Rational" => Self::RationalProxy,
            _ => unreachable!("Unknown native class `{}`.", kind),
        }
    }

    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        match self {
            Self::List(list) => list.to_string(heap),
            Self::ListIterator(list_iter) => list_iter.to_string(heap),
            Self::Set(set) => set.to_string(heap),
            Self::Dict(dict) => dict.to_string(heap),
            Self::Range(range) => range.to_string(heap),
            Self::RangeIterator(range_iter) => range_iter.to_string(heap),
            Self::Tuple(tuple) => tuple.to_string(heap),
            Self::TupleIterator(tuple_iter) => tuple_iter.to_string(heap),
            // Needs the instance's class name; handled in `Instance::to_string`.
            Self::Exception(_) => unreachable!("Exceptions are stringified via their instance"),
            Self::Generator(generator) => generator.to_string(heap),
            Self::Template(template) => template.to_string(heap),
            Self::TemplateIterator(template_iter) => template_iter.to_string(heap),
            Self::Interpolation(interpolation) => interpolation.to_string(heap),
            // Proxy classes should never be accessed for string conversion
            Self::BoolProxy => unreachable!("BoolProxy should never be converted to string"),
            Self::StringProxy => unreachable!("StringProxy should never be converted to string"),
            Self::IntegerProxy => unreachable!("IntegerProxy should never be converted to string"),
            Self::FloatProxy => unreachable!("FloatProxy should never be converted to string"),
            Self::RationalProxy => {
                unreachable!("RationalProxy should never be converted to string")
            }
        }
    }
}

/// Implement `From<T> for NativeClass` for every backing type
/// (the variant carries the type of the same name).
macro_rules! impl_from_for_native_class {
    ($($ty:ident),* $(,)?) => {
        $(
            impl From<$ty> for NativeClass {
                fn from(value: $ty) -> Self {
                    Self::$ty(value)
                }
            }
        )*
    };
}

impl_from_for_native_class!(
    List,
    ListIterator,
    Tuple,
    TupleIterator,
    Set,
    Dict,
    Range,
    RangeIterator,
    Exception,
    Generator,
    Template,
    TemplateIterator,
    Interpolation,
);

#[derive(Debug, Clone, PartialEq, Default)]
pub struct List {
    pub(crate) items: Vec<Value>,
}

impl List {
    #[must_use]
    pub(crate) const fn new(items: Vec<Value>) -> Self {
        Self { items }
    }

    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "[{}]",
            self.items
                .iter()
                .map(|item| item.to_string(heap))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<List Value>")
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Default)]
pub struct ListIterator {
    pub(crate) list: InstanceId,
    pub(crate) index: usize,
}

impl ListIterator {
    pub(crate) const fn new(list: InstanceId) -> Self {
        Self { list, index: 0 }
    }

    pub(crate) fn get_list<'a>(&self, heap: &'a Heap) -> &'a List {
        match &self.list.to_value(heap).backing {
            Some(NativeClass::List(list)) => list,
            _ => unreachable!("Expected a List instance, got {:?}", self.list),
        }
    }

    #[allow(clippy::option_if_let_else)]
    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "<list iterator of {}>",
            self.list.to_value(heap).to_string(heap)
        )
    }
}

impl std::fmt::Display for ListIterator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<list iterator of Value>")
    }
}

#[derive(Debug, Clone, Default)]
pub struct Set {
    pub(crate) items: HashTable<(Value, u64)>,
}

impl Set {
    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "{{{}}}",
            self.items
                .iter()
                .map(|(item, _hash)| item.to_string(heap))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    /// Add an item to a set (`receiver` must back a `Set` and be rooted).
    ///
    /// GC-safety: see [`Dict::add`] — the set is never taken out of the
    /// heap, and the table is never borrowed while the interpreter runs.
    pub(crate) fn add(vm: &mut VM, receiver: &Value, item: Value) -> VmResult {
        let hash = vm.compute_hash(item)?;
        let matched = Self::probe(vm, receiver, item, hash)?;
        let items = &mut receiver.as_set_mut(&mut vm.heap).items;
        if let Entry::Vacant(entry) = items.entry(hash, |(v, _h)| Some(*v) == matched, |(_v, h)| *h)
        {
            entry.insert((item, hash));
        }
        Ok(None)
    }

    /// Remove an item from a set. Returns whether it was found.
    pub(crate) fn remove(vm: &mut VM, receiver: &Value, item: Value) -> VmResult<bool> {
        let hash = vm.compute_hash(item)?;
        let Some(matched) = Self::probe(vm, receiver, item, hash)? else {
            return Ok(false);
        };
        let items = &mut receiver.as_set_mut(&mut vm.heap).items;
        Ok(items
            .find_entry(hash, |(v, _h)| *v == matched)
            .is_ok_and(|entry| {
                entry.remove();
                true
            }))
    }

    /// Check whether a set contains an item.
    pub(crate) fn contains(vm: &mut VM, receiver: &Value, item: Value) -> VmResult<bool> {
        let hash = vm.compute_hash(item)?;
        Ok(Self::probe(vm, receiver, item, hash)?.is_some())
    }

    /// Find the stored item equal to `item` among the same-hash entries.
    /// May run `__eq__`; the table is only borrowed for the pure candidate
    /// snapshot.
    fn probe(vm: &mut VM, receiver: &Value, item: Value, hash: u64) -> VmResult<Option<Value>> {
        let candidates: Vec<Value> = receiver
            .as_set(&vm.heap)
            .items
            .iter_hash(hash)
            .map(|(v, _h)| *v)
            .collect();
        for candidate in candidates {
            if vm.compare_values(candidate, item)? {
                return Ok(Some(candidate));
            }
        }
        Ok(None)
    }
}

impl std::fmt::Display for Set {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<Set Value>")
    }
}

impl PartialEq for Set {
    fn eq(&self, other: &Self) -> bool {
        hash_table_equal(&self.items, &other.items)
    }
}

#[derive(Debug, Clone, Default)]
pub struct Dict {
    pub(crate) items: HashTable<(Value, Value, u64)>,
}

impl Dict {
    #[allow(clippy::literal_string_with_formatting_args)]
    fn to_string(&self, heap: &Heap) -> String {
        if self.items.is_empty() {
            return "{:}".to_string();
        }
        format!(
            "{{{}}}",
            self.items
                .iter()
                .map(|(key, value, _hash)| format!(
                    "{}: {}",
                    key.to_string(heap),
                    value.to_string(heap)
                ))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    /// Add an entry to a dict (`receiver` must back a `Dict` and be rooted).
    ///
    /// GC-safety: the dict is never taken out of the heap. `__hash__`/`__eq__`
    /// re-enter the interpreter, which can trigger a GC (which must be able
    /// to see the dict's contents — they may not be rooted anywhere else) and
    /// can even mutate this very dict. So all interpreter re-entry happens
    /// while the dict rests in the heap, and the table is only borrowed in
    /// between for phases that cannot re-enter: hash the key, snapshot the
    /// same-hash candidates, probe them with `__eq__`, then mutate with pure
    /// closures only. The matched entry is re-located by identity (`Value`
    /// equality on the exact stored key, not `__eq__` — a reentrant dunder
    /// may have mutated the dict in the meantime), and rebucketing re-derives
    /// hashes from the hash stored in each entry — `__hash__`/`__eq__` never
    /// run while the table is borrowed.
    pub(crate) fn add(vm: &mut VM, receiver: &Value, key: Value, value: Value) -> VmResult {
        let hash = vm.compute_hash(key)?;
        let matched = Self::probe(vm, receiver, key, hash)?;
        let items = &mut receiver.as_dict_mut(&mut vm.heap).items;
        match items.entry(hash, |(k, _v, _h)| Some(*k) == matched, |(_k, _v, h)| *h) {
            Entry::Occupied(mut entry) => entry.get_mut().1 = value,
            Entry::Vacant(entry) => {
                entry.insert((key, value, hash));
            }
        }
        Ok(None)
    }

    /// Look up the value for a key in a dict.
    pub(crate) fn get(vm: &mut VM, receiver: &Value, key: Value) -> VmResult<Option<Value>> {
        let hash = vm.compute_hash(key)?;
        let Some(matched) = Self::probe(vm, receiver, key, hash)? else {
            return Ok(None);
        };
        Ok(receiver
            .as_dict(&vm.heap)
            .items
            .find(hash, |(k, _v, _h)| *k == matched)
            .map(|(_k, v, _h)| *v))
    }

    /// Check whether a dict contains a key.
    pub(crate) fn contains(vm: &mut VM, receiver: &Value, key: Value) -> VmResult<bool> {
        let hash = vm.compute_hash(key)?;
        Ok(Self::probe(vm, receiver, key, hash)?.is_some())
    }

    /// Remove an entry from a dict. Returns the removed value, if any.
    pub(crate) fn remove(vm: &mut VM, receiver: &Value, key: Value) -> VmResult<Option<Value>> {
        let hash = vm.compute_hash(key)?;
        let Some(matched) = Self::probe(vm, receiver, key, hash)? else {
            return Ok(None);
        };
        let items = &mut receiver.as_dict_mut(&mut vm.heap).items;
        Ok(items
            .find_entry(hash, |(k, _v, _h)| *k == matched)
            .ok()
            .map(|entry| entry.remove().0.1))
    }

    /// Find the stored key equal to `key` among the same-hash entries.
    /// May run `__eq__`; the table is only borrowed for the pure candidate
    /// snapshot. The `?` bails on the first exception, so no further
    /// candidate is compared while an exception is in flight.
    fn probe(vm: &mut VM, receiver: &Value, key: Value, hash: u64) -> VmResult<Option<Value>> {
        let candidates: Vec<Value> = receiver
            .as_dict(&vm.heap)
            .items
            .iter_hash(hash)
            .map(|(k, _v, _h)| *k)
            .collect();
        for candidate in candidates {
            if vm.compare_values(candidate, key)? {
                return Ok(Some(candidate));
            }
        }
        Ok(None)
    }
}

impl std::fmt::Display for Dict {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<Dict Value>")
    }
}

impl PartialEq for Dict {
    fn eq(&self, other: &Self) -> bool {
        hash_table_equal(&self.items, &other.items)
    }
}

fn hash_table_equal<T: PartialEq>(table1: &HashTable<T>, table2: &HashTable<T>) -> bool {
    if table1.len() != table2.len() {
        return false;
    }

    table1
        .iter()
        .all(|item1| table2.iter().any(|item2| item1 == item2))
}

#[derive(Debug, Clone, Copy)]
pub struct Range {
    start: GenericInt,
    end: GenericInt,
}

impl Range {
    pub fn new(start: GenericInt, end: GenericInt) -> Self {
        Self { start, end }
    }

    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "{}..<{}",
            self.start.to_string(heap),
            self.end.to_string(heap)
        )
    }

    pub(crate) fn start(&self) -> GenericInt {
        self.start
    }

    pub fn end(&self) -> GenericInt {
        self.end
    }

    pub(crate) fn contains(&self, value: &GenericInt, heap: &Heap) -> bool {
        if self.start.le(&self.end, heap) {
            // Normal range: start <= end
            self.start.le(value, heap) && value.lt(&self.end, heap)
        } else {
            // Reverse range: start > end
            self.end.lt(value, heap) && value.le(&self.start, heap)
        }
    }

    pub(crate) fn len(&self, heap: &mut Heap) -> GenericInt {
        self.end().sub(self.start(), heap).abs(heap)
    }

    pub(crate) fn is_empty(&self, heap: &Heap) -> bool {
        self.start().eq(&self.end(), heap)
    }

    pub(crate) fn is_forward(&self, heap: &Heap) -> bool {
        self.start.le(&self.end, heap)
    }
}

impl Default for Range {
    fn default() -> Self {
        Self::new(GenericInt::Small(0), GenericInt::Small(0))
    }
}

impl PartialEq for Range {
    fn eq(&self, _other: &Self) -> bool {
        // Two different ranges are never equal (for now).
        false
    }
}

#[derive(Debug, Clone, Default)]
pub struct RangeIterator {
    pub(crate) range: InstanceId,
    pub(crate) offset: GenericInt,
}

impl RangeIterator {
    pub(crate) fn new(range: InstanceId) -> Self {
        Self {
            range,
            offset: GenericInt::Small(0),
        }
    }

    pub(crate) fn get_range<'a>(&self, heap: &'a Heap) -> &'a Range {
        match &self.range.to_value(heap).backing {
            Some(NativeClass::Range(range)) => range,
            _ => unreachable!("Expected a Range instance, got {:?}", self.range),
        }
    }

    #[allow(clippy::option_if_let_else)]
    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "<range iterator of {}>",
            self.range.to_value(heap).to_string(heap)
        )
    }
}

impl std::fmt::Display for RangeIterator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<range iterator of Value>")
    }
}

impl PartialEq for RangeIterator {
    fn eq(&self, _other: &Self) -> bool {
        // Two different ranges are never equal (for now).
        false
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Tuple {
    items: Vec<Value>,
}

impl Tuple {
    #[must_use]
    pub(crate) const fn new(items: Vec<Value>) -> Self {
        Self { items }
    }

    pub fn items(&self) -> &[Value] {
        &self.items
    }

    fn to_string(&self, heap: &Heap) -> String {
        if self.items.len() == 1 {
            return format!("({},)", self.items[0].to_string(heap));
        }
        format!(
            "({})",
            self.items
                .iter()
                .map(|item| item.to_string(heap))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl std::fmt::Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<tuple Value>")
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Default)]
pub struct TupleIterator {
    tuple: InstanceId,
    pub index: usize,
}

impl TupleIterator {
    pub(crate) const fn new(tuple: InstanceId) -> Self {
        Self { tuple, index: 0 }
    }

    pub(crate) fn tuple(&self) -> InstanceId {
        self.tuple
    }

    pub(crate) fn get_tuple<'a>(&self, heap: &'a Heap) -> &'a Tuple {
        match &self.tuple.to_value(heap).backing {
            Some(NativeClass::Tuple(tuple)) => tuple,
            _ => unreachable!("Expected a tuple instance, got {:?}", self.tuple),
        }
    }

    #[allow(clippy::option_if_let_else)]
    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "<tuple iterator of {}>",
            self.tuple.to_value(heap).to_string(heap)
        )
    }
}

impl std::fmt::Display for TupleIterator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<tuple iterator of Value>")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Exception {
    message: Option<StringId>,
    pub stack_trace: Option<StringId>,
}

impl Exception {
    #[must_use]
    pub(crate) fn new(message: Option<StringId>) -> Self {
        Self {
            message,
            stack_trace: None,
        }
    }

    pub(crate) fn message(&self) -> Option<StringId> {
        self.message
    }

    pub(crate) fn stack_trace(&self) -> Option<StringId> {
        self.stack_trace
    }
}

impl std::fmt::Display for Exception {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<Exception Value>")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum GeneratorState {
    /// Created but never resumed. The saved stack holds the closure and its
    /// arguments; the callframe points at the first instruction.
    #[default]
    Suspended,
    /// Started: currently executing, or parked at a `yield` between resumes.
    /// Note that a generator waiting at a `yield` is `Running`, not
    /// `Suspended` — `Suspended` strictly means "never started".
    Running,
    /// Finished: returned, threw out, or was closed. Resuming a completed
    /// generator returns `StopIteration` without running any bytecode.
    Completed,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Generator {
    pub(crate) callframe: CallFrame,
    /// Handlers registered inside the generator, in suspended
    /// (generator-relative) form; rebased on every resume/suspension.
    pub(crate) exception_handlers: Vec<SuspendedExceptionHandler>,
    pub(crate) stack: Vec<Value>,
    pub(crate) state: GeneratorState,
}

impl Generator {
    #[must_use]
    pub(crate) fn new(
        callframe: CallFrame,
        exception_handlers: Vec<SuspendedExceptionHandler>,
        stack: Vec<Value>,
    ) -> Self {
        Self {
            callframe,
            exception_handlers,
            stack,
            state: GeneratorState::default(),
        }
    }

    pub(crate) fn from_closure_id(closure: ClosureId) -> Self {
        Self {
            callframe: CallFrame::from_closure_id(closure),
            exception_handlers: Vec::new(),
            stack: Vec::new(),
            state: GeneratorState::default(),
        }
    }

    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        format!(
            "<generator of {}>",
            self.callframe
                .closure
                .to_value(heap)
                .function
                .to_value(heap)
                .name
                .to_value(heap)
        )
    }

    pub(crate) fn closure(&self) -> ClosureId {
        self.callframe.closure
    }

    /// Re-plant this suspended generator into the live VM and drive it with `f`.
    ///
    /// Restoring works by moving the generator's saved state onto the VM's
    /// own structures: the saved callframe is rebased to the current stack
    /// top and pushed onto the callstack, the saved value slice is appended
    /// to the VM stack, and the saved exception handlers are spliced onto
    /// the VM handler stack. `f` then executes bytecode — `VM::run_function`
    /// for `send`/`next`, or a closure that first throws for [`Self::raise`].
    ///
    /// `f`'s result is interpreted as:
    /// - `Ok(Some(frame))`: the generator suspended at a `yield`, or returned
    ///   (a generator `return` pushes `StopIteration` as its value). `frame`
    ///   is the generator's own popped frame and the yielded value is on the
    ///   stack top; `StopIteration` marks the generator `Completed`.
    /// - `Err(..)`: a runtime error, or an exception escaping the generator
    ///   (including one raised into it and caught outside — see
    ///   [`Self::raise`]). The generator is marked `Completed` and the error
    ///   propagates.
    /// - `Ok(None)` is impossible: the run loops only exit a region through
    ///   a yield/return frame or an error (see the precondition on
    ///   `VM::run_function_from_depth`).
    ///
    /// On suspension the inverse move runs: the returned frame, the stack
    /// slice above its base, and all handlers registered above the remaining
    /// callstack are drained back into the generator for the next resume —
    /// the handlers rebased to their suspended (generator-relative) form so
    /// they survive being resumed at a different depth.
    ///
    /// # GC safety
    ///
    /// While a generator runs, the native methods have taken it out of the
    /// heap, so the GC cannot see it. Moving the saved value stack and
    /// callframe onto `vm.stack`/`vm.callstack` (GC roots) below BEFORE `f`
    /// executes any bytecode is what keeps its contents alive. Any new state
    /// added to `Generator` that holds heap values must likewise be re-rooted
    /// here before `f` runs.
    fn resume_with<F>(&mut self, vm: &mut VM, f: F) -> VmResult<Value>
    where
        F: FnOnce(&mut VM) -> VmResult<Option<CallFrame>>,
    {
        if self.state == GeneratorState::Completed {
            return Ok(Value::StopIteration);
        }
        let closure_id = self.closure();
        let mut callframe =
            std::mem::replace(&mut self.callframe, CallFrame::from_closure_id(closure_id));
        callframe.stack_base = vm.stack.len();
        let frame_base = vm.callstack.len();
        let stack_base = callframe.stack_base;
        vm.callstack.push_callframe(callframe, &vm.heap);
        vm.resume_suspended_handlers(
            std::mem::take(&mut self.exception_handlers),
            frame_base,
            stack_base,
        );
        vm.stack.extend(std::mem::take(&mut self.stack));

        self.state = GeneratorState::Running;
        let callframe = match f(vm) {
            Ok(Some(callframe)) => callframe,
            Ok(None) => {
                unreachable!("generator resume must end in a yield/return frame or an error")
            }
            Err(err) => {
                self.state = GeneratorState::Completed;
                return Err(err);
            }
        };

        let yielded_value = vm.stack.pop().expect("Stack underflow in generator");
        if yielded_value == Value::StopIteration {
            self.state = GeneratorState::Completed;
        }

        self.callframe = callframe;
        self.stack = vm.stack.drain(self.callframe.stack_base..).collect();
        self.exception_handlers = vm.suspend_handlers_above(self.callframe.stack_base);

        Ok(yielded_value)
    }

    /// Resume the generator, delivering `value` as the result of the `yield`
    /// expression it is parked on.
    ///
    /// A never-started generator (`Suspended`) has no pending `yield` to
    /// receive the value, so sending anything but nil throws a `TypeError`.
    /// For a started generator the value is pushed onto its saved stack,
    /// which is exactly where the resumed frame expects the yield result.
    pub(crate) fn send(&mut self, value: Value, vm: &mut VM) -> VmResult<Value> {
        if self.state == GeneratorState::Suspended && value != Value::Nil {
            vm.throw(
                TypeError,
                "Can't send non-nil value to a just-started generator",
            )?;
        }

        if self.state == GeneratorState::Running {
            self.stack.push(value);
        }

        self.resume_with(vm, VM::run_function)
    }

    /// Throw an exception of class `exception_class` into the generator at
    /// its suspension point (like Python's `generator.throw`).
    ///
    /// After validating that `exception_class` is a class deriving from
    /// `Exception`, the generator is resumed with a closure that — inside the
    /// restored generator context — instantiates the class (running its
    /// `__init__`), unwinds from the suspended position, and then continues
    /// execution wherever the unwind landed:
    /// - Caught by a `try` inside the generator: the generator keeps running
    ///   to its next `yield`/`return`, indistinguishable from a normal resume.
    /// - Caught outside: the generator is marked `Completed` and the
    ///   already-positioned unwind propagates through the `Err` branch of
    ///   [`Self::resume_with`].
    /// - Caught nowhere: the fatal runtime error propagates; the generator is
    ///   not resumed.
    pub(crate) fn raise(&mut self, exception_class: Value, vm: &mut VM) -> VmResult<Value> {
        let exception_class_id = if let Value::Class(exception_class_id) = exception_class {
            // Check that the class to catch is a subclass of Exception
            if !is_exception_subclass(&vm.heap, exception_class_id) {
                return Err(vm
                    .throw(
                        TypeError,
                        &format!(
                            "Can only throw Exception or its subclasses, got: {}",
                            exception_class_id
                                .to_value(&vm.heap)
                                .name
                                .to_value(&vm.heap)
                        ),
                    )
                    .unwrap_err());
            }
            exception_class_id
        } else {
            return Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "Exception to throw must be a class, got: {}",
                        exception_class.to_string(&vm.heap)
                    ),
                )
                .unwrap_err());
        };

        self.resume_with(vm, |vm| {
            let class_data = exception_class_id.to_value(&vm.heap);
            let init_method_id = vm.heap.builtin_constants().init_string;
            let init_method = *class_data.methods.get(&init_method_id).unwrap();

            let backing = NativeClass::new("Exception");

            let instance = vm
                .heap
                .add_instance(Instance::new(exception_class_id, Some(backing)));
            vm.stack.push(instance);
            vm.invoke_and_run_function(
                init_method_id,
                0,
                matches!(init_method, Value::NativeMethod(_)),
            )?;
            let exception = vm.stack.pop().expect("Stack underflow in next_native");

            let call_depth = vm.callstack.len();
            match vm.unwind(exception) {
                // No handler anywhere: the fatal traceback is already
                // printed. Abort instead of resuming the generator.
                err @ Err(VmErrorKind::Runtime(_)) => err,
                Err(err @ VmErrorKind::Exception(_)) => {
                    if vm.callstack.len() < call_depth {
                        // Caught outside the generator: the unwind already
                        // popped the generator frame and positioned the VM at
                        // the outer handler. Propagate the marker untouched;
                        // `resume_with` marks the generator `Completed`.
                        Err(err)
                    } else {
                        // Caught inside the generator: continue running it
                        // from the catch block like a normal resume.
                        vm.run_function_from_depth(call_depth)
                    }
                }
                Ok(_) => unreachable!("unwind never returns Ok"),
            }
        })
    }

    /// Resume the generator without delivering a value: `send(nil)`.
    pub(crate) fn next(&mut self, vm: &mut VM) -> VmResult<Value> {
        self.send(Value::Nil, vm)
    }
}

impl std::fmt::Display for Generator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<Generator Value>")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Template {
    interpolations: Vec<InstanceId>,
    strings: Vec<StringId>,
}

impl Template {
    #[must_use]
    pub(crate) fn new(interpolations: Vec<InstanceId>, strings: Vec<StringId>) -> Self {
        Self {
            interpolations,
            strings,
        }
    }

    pub(crate) fn strings(&self) -> &Vec<StringId> {
        &self.strings
    }

    pub(crate) fn interpolations(&self) -> &Vec<InstanceId> {
        &self.interpolations
    }

    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        let strings_str = if self.strings.len() == 1 {
            format!("(\"{}\",)", self.strings[0].to_value(heap))
        } else {
            format!(
                "({})",
                self.strings
                    .iter()
                    .map(|s| format!("\"{}\"", s.to_value(heap)))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        let interpolations_str = if self.interpolations.len() == 1 {
            format!(
                "({},)",
                self.interpolations[0].to_value(heap).to_string(heap)
            )
        } else {
            format!(
                "({})",
                self.interpolations
                    .iter()
                    .map(|i| i.to_value(heap).to_string(heap))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        format!("Template(strings={strings_str}, interpolations={interpolations_str})")
    }
}

impl std::fmt::Display for Template {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<Template Value>")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TemplateIterator {
    pub(crate) template: InstanceId,
    pub(crate) index: usize,
}

impl TemplateIterator {
    pub(crate) fn new(template: InstanceId) -> Self {
        Self { template, index: 0 }
    }

    pub(crate) fn get_template<'a>(&self, heap: &'a Heap) -> &'a Template {
        match &self.template.to_value(heap).backing {
            Some(NativeClass::Template(template)) => template,
            _ => unreachable!("Expected a Template instance, got {:?}", self.template),
        }
    }

    #[allow(clippy::option_if_let_else)]
    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "<template iterator of {}>",
            self.template.to_value(heap).to_string(heap)
        )
    }
}

impl std::fmt::Display for TemplateIterator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<template iterator of Value>")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Interpolation {
    value: Value,
    expression: StringId,
}

impl Interpolation {
    #[must_use]
    pub(crate) fn new(value: Value, expression: StringId) -> Self {
        Self { value, expression }
    }

    pub(crate) fn value(&self) -> Value {
        self.value
    }

    pub(crate) fn expression(&self) -> StringId {
        self.expression
    }

    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        format!(
            "Interpolation(value={}, expression={})",
            self.value.to_string(heap),
            self.expression.to_value(heap)
        )
    }
}

impl Default for Interpolation {
    fn default() -> Self {
        Self {
            value: Value::Nil,
            expression: StringId::default(),
        }
    }
}

impl std::fmt::Display for Interpolation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<Interpolation Value>")
    }
}
