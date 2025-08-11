use crate::{
    heap::{Heap, StringId},
    value::GenericInt,
    vm::VM,
};

use hashbrown::HashTable;
use hashbrown::hash_table::Entry;

use super::Value;
use crate::heap::InstanceId;
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

pub type NativeFunctionImpl = fn(&mut VM, &mut [&mut Value]) -> Result<Value, String>;
pub type NativeMethodImpl = fn(&mut VM, &mut Value, &mut [&mut Value]) -> Result<Value, String>;
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
}

impl NativeClass {
    pub(crate) fn new(kind: &str) -> Self {
        match kind {
            "List" => Self::List(List::new()),
            "Set" => Self::Set(Set::new()),
            "Dict" => Self::Dict(Dict::new()),
            "Range" => Self::Range(Range::new(GenericInt::Small(0), GenericInt::Small(0))),
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

#[derive(Debug, Clone, PartialEq)]
pub struct List {
    pub(crate) items: Vec<Value>,
}

impl List {
    #[must_use]
    pub(crate) const fn new() -> Self {
        Self { items: Vec::new() }
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
    pub(crate) items: HashTable<Value>,
}

impl Set {
    #[must_use]
    pub(crate) fn new() -> Self {
        Self {
            items: HashTable::default(),
        }
    }

    fn to_string(&self, heap: &Heap) -> String {
        format!(
            "{{{}}}",
            self.items
                .iter()
                .map(|item| item.to_string(heap))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    pub(crate) fn add(&mut self, item: Value, heap: &Heap) {
        if let Entry::Vacant(entry) = self.items.entry(
            item.to_hash(heap),
            |val| val.eq(&item, heap),
            |val| val.to_hash(heap),
        ) {
            entry.insert(item);
        }
    }

    pub(crate) fn remove(&mut self, item: &Value, heap: &Heap) -> bool {
        self.items
            .find_entry(item.to_hash(heap), |val| val.eq(item, heap))
            .is_ok_and(|entry| {
                entry.remove();
                true
            })
    }

    pub(crate) fn contains(&self, item: &Value, heap: &Heap) -> bool {
        self.items
            .find(item.to_hash(heap), |val| val.eq(item, heap))
            .is_some()
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
    pub(crate) items: HashTable<(Value, Value)>,
}

impl Dict {
    #[must_use]
    pub(crate) fn new() -> Self {
        Self {
            items: HashTable::default(),
        }
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
                .map(|(key, value)| format!("{}: {}", key.to_string(heap), value.to_string(heap)))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    pub(crate) fn add(&mut self, key: Value, value: Value, heap: &Heap) {
        match self.items.entry(
            key.to_hash(heap),
            |(k, _v)| k.eq(&key, heap),
            |(k, _v)| k.to_hash(heap),
        ) {
            Entry::Vacant(entry) => {
                entry.insert((key, value));
            }
            Entry::Occupied(mut entry) => {
                entry.get_mut().1 = value;
            }
        }
    }

    pub(crate) fn get(&self, key: &Value, heap: &Heap) -> Option<&Value> {
        self.items
            .find(key.to_hash(heap), |(k, _v)| k.eq(key, heap))
            .map(|(_k, v)| v)
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
