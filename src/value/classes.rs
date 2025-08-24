use crate::heap::{ClassId, Heap, StringId};

use derivative::Derivative;
use rustc_hash::FxHashMap as HashMap;

use super::{NativeClass, Value};

#[derive(Debug, Clone, Derivative)]
#[derivative(PartialOrd)]
pub struct Class {
    pub(crate) name: StringId,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    // Have to be general Value because it can be a nativemethod(how?) or a closure
    // Should probably also be String and not StringId?
    pub(crate) methods: HashMap<StringId, Value>,
    pub(crate) is_native: bool,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    pub(crate) superclass: Option<ClassId>,
}

impl Class {
    #[must_use]
    pub(crate) fn new(name: StringId, is_native: bool) -> Self {
        Self {
            name,
            methods: HashMap::default(),
            is_native,
            superclass: None,
        }
    }

    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        format!("<class {}>", *self.name.to_value(heap))
    }

    pub(crate) fn get_native_superclass(&self, heap: &Heap, class_id: ClassId) -> Option<ClassId> {
        if self.is_native {
            return Some(class_id); // We are the native class
        }

        if let Some(superclass_id) = self.superclass {
            let superclass = superclass_id.to_value(heap);
            if superclass.is_native {
                return Some(superclass_id);
            }
            return superclass.get_native_superclass(heap, superclass_id);
        }

        None
    }
}

/// Check if the first class is the same as or a subclass of the second class.
/// This is a standalone function that works with `ClassIds` directly.
pub fn is_subclass_of(heap: &Heap, current_class_id: ClassId, superclass_id: ClassId) -> bool {
    // Check if they are the same class
    if current_class_id == superclass_id {
        return true;
    }

    // Walk up the inheritance chain
    if let Some(parent_id) = current_class_id.to_value(heap).superclass {
        return is_subclass_of(heap, parent_id, superclass_id);
    }

    false
}

pub fn get_native_class_id(heap: &Heap, native_class: &str) -> ClassId {
    *heap
        .native_classes
        .get(native_class)
        .expect("Internal error: Exception class should be defined in native_classes")
}

/// Check if a class is a subclass of Exception
pub fn is_exception_subclass(heap: &Heap, class_id: ClassId) -> bool {
    is_subclass_of(heap, class_id, get_native_class_id(heap, "Exception"))
}

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<class Value>")
    }
}

impl PartialEq for Class {
    fn eq(&self, _other: &Self) -> bool {
        // Two different classes are always considered different
        false
    }
}

impl Eq for Class {}

#[derive(Derivative)]
#[derivative(Debug, PartialOrd, Clone)]
pub struct Instance {
    pub(crate) class: ClassId,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    pub(crate) fields: HashMap<String, Value>,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    pub(crate) backing: Option<NativeClass>,
}

impl Instance {
    #[must_use]
    pub(crate) fn new(class: ClassId, backing: Option<NativeClass>) -> Self {
        Self {
            class,
            fields: HashMap::default(),
            backing,
        }
    }

    #[allow(clippy::option_if_let_else)]
    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        match &self.backing {
            Some(native_class) => native_class.to_string(heap),
            None => format!(
                "<{} instance>",
                self.class.to_value(heap).name.to_value(heap)
            ),
        }
    }

    pub(crate) fn has_field_or_method(&self, method_name: StringId, heap: &Heap) -> bool {
        self.fields.contains_key(method_name.to_value(heap))
            || self.class.to_value(heap).methods.contains_key(&method_name)
    }

    pub(crate) fn get_field_or_method(&self, method_name: StringId, heap: &Heap) -> Option<Value> {
        self.fields
            .get(method_name.to_value(heap))
            .copied()
            .or_else(|| self.class.to_value(heap).methods.get(&method_name).copied())
    }
}

impl std::fmt::Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.backing {
            Some(_) => f.pad("<native instance Value>"),
            None => f.pad("<instance Value>"),
        }
    }
}

impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        self.backing.is_some() && self.backing == other.backing
    }
}

#[derive(Debug, Clone)]
pub struct BoundMethod {
    // Probably could be an InstanceId now
    pub(crate) receiver: Value,
    // Has to be a general Value because it can be a NativeMethod or Closure
    pub(crate) method: Value,
}

impl BoundMethod {
    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        format!(
            "<bound method {}.{} of {}>",
            *self.receiver_class_name(heap).to_value(heap),
            *self.method_name(heap).to_value(heap),
            self.receiver.to_string(heap)
        )
    }

    fn method_name(&self, heap: &Heap) -> StringId {
        match self.method {
            Value::NativeMethod(native) => native.to_value(heap).name,
            Value::Closure(closure) => closure.to_value(heap).function.to_value(heap).name,
            x => unreachable!(
                "Bound method only binds over closures or native methods, got `{}` instead.",
                x.to_string(heap)
            ),
        }
    }

    fn receiver_class_name(&self, heap: &Heap) -> StringId {
        match self.receiver {
            Value::Instance(instance) => instance.to_value(heap).class.to_value(heap).name,
            x => unreachable!(
                "Bound methods can only have instances as receivers, got `{}` instead.",
                x.to_string(heap)
            ),
        }
    }
}

impl PartialEq for BoundMethod {
    fn eq(&self, _other: &Self) -> bool {
        // Two different bound methods are always considered different
        false
    }
}

impl std::fmt::Display for BoundMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<bound method Value>")
    }
}
