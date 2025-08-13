use crate::{
    heap::{Heap, StringId},
    value::GenericInt,
    vm::{
        VM,
        errors::{ExceptionRaisedKind, RuntimeErrorKind, VmErrorKind},
    },
};

use hashbrown::HashTable;
use hashbrown::hash_table::Entry;

use super::Value;
use crate::heap::InstanceId;
use crate::vm::errors::VmError;
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

pub type NativeFunctionImpl = fn(&mut VM, &mut [&mut Value]) -> VmError<Value>;
pub type NativeMethodImpl = fn(&mut VM, &mut Value, &mut [&mut Value]) -> VmError<Value>;
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
            Self::Exception(exception) => exception.to_string(heap),
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

impl From<List> for NativeClass {
    fn from(list: List) -> Self {
        Self::List(list)
    }
}

impl From<ListIterator> for NativeClass {
    fn from(list_iterator: ListIterator) -> Self {
        Self::ListIterator(list_iterator)
    }
}

impl From<Tuple> for NativeClass {
    fn from(tuple: Tuple) -> Self {
        Self::Tuple(tuple)
    }
}

impl From<TupleIterator> for NativeClass {
    fn from(tuple_iterator: TupleIterator) -> Self {
        Self::TupleIterator(tuple_iterator)
    }
}

impl From<Set> for NativeClass {
    fn from(set: Set) -> Self {
        Self::Set(set)
    }
}

impl From<Dict> for NativeClass {
    fn from(dict: Dict) -> Self {
        Self::Dict(dict)
    }
}

impl From<Range> for NativeClass {
    fn from(range: Range) -> Self {
        Self::Range(range)
    }
}

impl From<RangeIterator> for NativeClass {
    fn from(range_iterator: RangeIterator) -> Self {
        Self::RangeIterator(range_iterator)
    }
}

impl From<Exception> for NativeClass {
    fn from(exception: Exception) -> Self {
        Self::Exception(exception)
    }
}

#[derive(Debug, Clone, PartialEq)]
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

impl Default for List {
    fn default() -> Self {
        Self::new(Vec::new())
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

#[derive(Debug, Clone)]
pub struct Set {
    pub(crate) items: HashTable<(Value, u64)>,
}

impl Set {
    #[must_use]
    pub(crate) fn new(items: HashTable<(Value, u64)>) -> Self {
        Self { items }
    }

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

    pub(crate) fn add(&mut self, item: Value, vm: &mut VM) -> VmError {
        let hash = vm.compute_hash(item)?;
        let entry = self.items.entry(
            hash,
            |(val, _stored_hash)| vm.compare_values_for_collections(*val, item),
            |(_val, stored_hash)| *stored_hash,
        );

        if vm.encountered_hard_exception {
            return Err(VmErrorKind::Runtime(RuntimeErrorKind));
        }
        if vm.handling_exception {
            return Err(VmErrorKind::Exception(ExceptionRaisedKind));
        }

        if let Entry::Vacant(entry) = entry {
            entry.insert((item, hash));
        }
        Ok(())
    }

    pub(crate) fn remove(&mut self, item: Value, vm: &mut VM) -> VmError<bool> {
        let hash = vm.compute_hash(item)?;
        let found = self
            .items
            .find_entry(hash, |(val, _stored_hash)| {
                vm.compare_values_for_collections(*val, item)
            })
            .is_ok_and(|entry| {
                entry.remove();
                true
            });

        if vm.encountered_hard_exception {
            return Err(VmErrorKind::Runtime(RuntimeErrorKind));
        }
        if vm.handling_exception {
            return Err(VmErrorKind::Exception(ExceptionRaisedKind));
        }

        Ok(found)
    }

    pub(crate) fn contains(&self, item: Value, vm: &mut VM) -> VmError<bool> {
        let hash = vm.compute_hash(item)?;
        let found = self
            .items
            .find(hash, |(val, _stored_hash)| {
                vm.compare_values_for_collections(*val, item)
            })
            .is_some();

        if vm.encountered_hard_exception {
            return Err(VmErrorKind::Runtime(RuntimeErrorKind));
        }
        if vm.handling_exception {
            return Err(VmErrorKind::Exception(ExceptionRaisedKind));
        }

        Ok(found)
    }
}

impl Default for Set {
    fn default() -> Self {
        Self::new(HashTable::default())
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

#[derive(Debug, Clone)]
pub struct Dict {
    pub(crate) items: HashTable<(Value, Value, u64)>,
}

impl Dict {
    #[must_use]
    pub(crate) fn new(items: HashTable<(Value, Value, u64)>) -> Self {
        Self { items }
    }

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

    pub(crate) fn add(&mut self, key: Value, value: Value, vm: &mut VM) -> VmError {
        let hash = vm.compute_hash(key)?;
        let entry = self.items.entry(
            hash,
            |(k, _v, _stored_hash)| vm.compare_values_for_collections(*k, key),
            |(_k, _v, stored_hash)| *stored_hash,
        );

        if vm.encountered_hard_exception {
            return Err(VmErrorKind::Runtime(RuntimeErrorKind));
        }
        if vm.handling_exception {
            return Err(VmErrorKind::Exception(ExceptionRaisedKind));
        }

        match entry {
            Entry::Vacant(entry) => {
                entry.insert((key, value, hash));
            }
            Entry::Occupied(mut entry) => {
                entry.get_mut().1 = value;
            }
        }
        Ok(())
    }

    pub(crate) fn get(&self, key: Value, vm: &mut VM) -> VmError<Option<&Value>> {
        let hash = vm.compute_hash(key)?;

        let result = self
            .items
            .find(hash, |(k, _v, _stored_hash)| {
                vm.compare_values_for_collections(*k, key)
            })
            .map(|(_k, v, _stored_hash)| v);

        if vm.encountered_hard_exception {
            return Err(VmErrorKind::Runtime(RuntimeErrorKind));
        }
        if vm.handling_exception {
            return Err(VmErrorKind::Exception(ExceptionRaisedKind));
        }

        Ok(result)
    }

    pub(crate) fn contains(&self, key: Value, vm: &mut VM) -> VmError<bool> {
        let hash = vm.compute_hash(key)?;
        let result = self
            .items
            .find(hash, |(k, _v, _stored_hash)| {
                vm.compare_values_for_collections(*k, key)
            })
            .is_some();
        if vm.encountered_hard_exception {
            return Err(VmErrorKind::Runtime(RuntimeErrorKind));
        }
        if vm.handling_exception {
            return Err(VmErrorKind::Exception(ExceptionRaisedKind));
        }
        Ok(result)
    }
}

impl Default for Dict {
    fn default() -> Self {
        Self::new(HashTable::default())
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

#[derive(Debug, Clone, PartialEq)]
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

impl Default for Tuple {
    fn default() -> Self {
        Self::new(Vec::new())
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
    stack_trace: StringId,
}

impl Exception {
    #[must_use]
    pub(crate) fn new(message: Option<StringId>, stack_trace: StringId) -> Self {
        Self {
            message,
            stack_trace,
        }
    }

    pub(crate) fn message(&self) -> Option<StringId> {
        self.message
    }

    pub(crate) fn stack_trace(&self) -> StringId {
        self.stack_trace
    }

    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        let mut result = match self.message {
            Some(message) => format!("Exception: {}\n", message.to_value(heap)),
            None => "Exception\n".to_string(),
        };
        result.push_str(self.stack_trace.to_value(heap));
        result
    }
}

impl std::fmt::Display for Exception {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<Exception Value>")
    }
}
