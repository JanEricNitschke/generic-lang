use crate::heap::{ClassId, Heap, StringId};

use derivative::Derivative;
use rustc_hash::FxHashMap as HashMap;

use super::{NativeClass, Value};

#[derive(Debug, Clone, Derivative)]
#[derivative(PartialOrd, PartialEq)]
pub struct Class {
    pub(crate) name: StringId,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    // Have to be general Value because it can be a nativemethod(how?) or a closure
    // Should probably also be String and not StringId?
    pub(crate) methods: HashMap<StringId, Value>,
    pub(crate) is_native: bool,
}
impl Eq for Class {}

impl Class {
    #[must_use]
    pub(crate) fn new(name: StringId, is_native: bool) -> Self {
        Self {
            name,
            methods: HashMap::default(),
            is_native,
        }
    }

    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        format!("<class {}>", *self.name.to_value(heap))
    }
}

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<class Value>")
    }
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, PartialOrd, Clone)]
pub struct Instance {
    pub(crate) class: ClassId,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    pub(crate) fields: HashMap<String, Value>,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    pub(crate) backing: Option<NativeClass>,
}

impl Instance {
    #[must_use]
    pub(crate) fn new(class: Value, backing: Option<NativeClass>) -> Self {
        let id = *class.as_class();
        Self {
            class: id,
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
}

impl std::fmt::Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.backing {
            Some(_) => f.pad("<native instance Value>"),
            None => f.pad("<instance Value>"),
        }
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
