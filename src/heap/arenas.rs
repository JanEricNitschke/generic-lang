use num_bigint::BigInt;
use paste::paste;
use slotmap::{HopSlotMap as SlotMap, Key, new_key_type};
use std::fmt::{Debug, Display};

use super::Heap;
use crate::value::{
    BoundMethod, Class, Closure, Function, Instance, Module, NativeFunction, NativeMethod, Upvalue,
};

/// Wrapper attaching a flag indicating whether an object
/// has been marked during the mark phase of mark and sweep
/// garbage collection.
#[derive(Clone, Debug, PartialEq)]
pub(super) struct Item<T> {
    pub(super) marked: bool,
    pub(super) item: T,
}

impl<T> Item<T> {
    const fn new(item: T, marked: bool) -> Self {
        Self { marked, item }
    }
}

pub trait ArenaValue: Debug + Display + PartialEq {}
impl<T> ArenaValue for T where T: Debug + Display + PartialEq {}

// Define separate key types for each `Value` variant to ensure
// that no misuse occurs.
new_key_type! {
    pub struct FunctionId;
    pub struct StringId;
    pub struct BigIntId;
    pub struct UpvalueId;
    pub struct ClosureId;
    pub struct NativeFunctionId;
    pub struct NativeMethodId;
    pub struct ClassId;
    pub struct InstanceId;
    pub struct BoundMethodId;
    pub struct ModuleId;
}

macro_rules! impl_to_value {
    ($($id:ident => {$slot_name:ident, $value_ty:ty}),*) => {
        paste! {
            $(
                impl $id {
                    pub fn to_value<'a>(&self, heap: &'a Heap) -> &'a $value_ty {
                        heap.[<get_$slot_name>](*self)
                    }

                    #[allow(dead_code)]
                    pub fn to_value_mut<'a>(&self, heap: &'a mut Heap) -> &'a mut $value_ty {
                        heap.[<get_mut_$slot_name>](*self)
                    }

                    #[allow(dead_code)]
                    pub fn marked(&self, heap: &Heap) -> bool {
                        heap.[< $slot_name _marked>](*self)
                    }
                }
            )*
        }
    };
}

// Implement `to_value` and `to_value_mut` for all the given structs
impl_to_value!(
    FunctionId => {function, Function},
    StringId => {string, String},
    BigIntId => {big_int, BigInt},
    UpvalueId => {upvalue, Upvalue},
    ClosureId => {closure, Closure},
    NativeFunctionId => {native_function, NativeFunction},
    NativeMethodId => {native_method, NativeMethod},
    ClassId => {class, Class},
    InstanceId => {instance, Instance},
    BoundMethodId => {bound_method, BoundMethod},
    ModuleId => {module, Module}
);

/// Arenas storing each `Value` variant.
///
/// Each arena has a name when logging the garbage collector.
/// The main core of the arenas is a `HopSlotMap` storing the actual values.
/// Additionally, they store their overall number of allocated bytes as well
/// as a vector of items to process for `mark and sweep`.
#[derive(Clone, Debug)]
pub struct Arena<K: Key, V: ArenaValue> {
    #[allow(dead_code)]
    name: &'static str,

    pub(super) data: SlotMap<K, Item<V>>,
    bytes_allocated: usize,

    pub(super) gray: Vec<K>,
}

impl<K: Key, V: ArenaValue> Arena<K, V> {
    #[must_use]
    pub(super) fn new(name: &'static str) -> Self {
        Self {
            name,
            data: SlotMap::with_key(),
            bytes_allocated: 0,
            gray: Vec::new(),
        }
    }

    /// Add a value to the arena and return a corresponding Id.
    ///
    /// Also update the total number of bytes allocated in this arena.
    pub(super) fn add(&mut self, value: V, black_value: bool) -> K {
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

        id
    }

    pub(super) fn get(&self, index: K) -> &V {
        &self.data[index].item
    }

    pub(super) fn get_mut(&mut self, index: K) -> &mut V {
        &mut self.data[index].item
    }

    pub(super) fn is_marked(&self, index: K, black_value: bool) -> bool {
        self.data[index].marked == black_value
    }

    /// Clear the gray values and return them for processing.
    pub(super) fn flush_gray(&mut self) -> Vec<K> {
        let capacity = self.gray.capacity();
        std::mem::replace(&mut self.gray, Vec::with_capacity(capacity))
    }

    /// Remove all non-marked values from the arena.
    ///
    /// Also update the total number of allocated bytes.
    pub(super) fn sweep(&mut self, black_value: bool) {
        #[cfg_attr(feature = "log_gc", allow(clippy::used_underscore_binding))]
        self.data.retain(|_key, value| {
            #[cfg(feature = "log_gc")]
            if value.marked != black_value {
                eprintln!("{}/{:?} free {}", self.name, _key, value.item);
            }
            value.marked == black_value
        });
        self.bytes_allocated = std::mem::size_of::<V>() * self.data.len();
    }

    pub(super) const fn bytes_allocated(&self) -> usize {
        self.bytes_allocated
    }
}

impl<K: Key, V: ArenaValue> std::ops::Index<K> for Arena<K, V> {
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        &self.data[index].item
    }
}

impl<K: Key, V: ArenaValue> std::ops::IndexMut<K> for Arena<K, V> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.data[index].item
    }
}
