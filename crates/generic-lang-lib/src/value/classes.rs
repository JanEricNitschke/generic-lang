use crate::heap::{ClassId, Heap, StringId};

use derivative::Derivative;
use indexmap::IndexMap;
use rustc_hash::FxHashMap as HashMap;

use super::{NativeClass, Number, Value};

/// Plugin-specific class metadata, declared on the C ABI `ClassDesc` and stored
/// on the class at load time. Reached through an instance's `class` during the
/// GC mark and sweep phases.
#[cfg(feature = "plugins")]
#[derive(Debug, Clone, Copy)]
pub struct PluginClassInfo {
    pub(crate) drop: Option<extern "C" fn(*mut core::ffi::c_void)>,
    pub(crate) traverse: Option<generic_lang_api::PluginTraverseFn>,
}

/// What a class is backed by. `User` is an ordinary generic class; `Native` is
/// an interpreter builtin (`List`, `Exception`, the value-type proxies, …);
/// `Plugin` is defined by a native extension and carries its drop/traverse.
#[derive(Debug, Clone, Copy)]
pub enum ClassKind {
    User,
    Native,
    #[cfg(feature = "plugins")]
    Plugin(PluginClassInfo),
}

/// A class variable declared in a class body (`var x: annotation = default;`).
/// The annotation is `Nil` when omitted; `default` is `None` for a bare
/// declaration, which reserves the name (and its position, for
/// dataclasses) without providing a readable value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ClassVariable {
    pub(crate) annotation: Value,
    pub(crate) default: Option<Value>,
}

#[derive(Debug, Clone, Derivative)]
#[derivative(PartialOrd)]
pub struct Class {
    pub(crate) name: StringId,
    /// The class's methods: `Closure`s for methods written in generic,
    /// `NativeMethod`s for native, plugin, and dataclass-generated ones -
    /// hence the general `Value`. Keyed by interned `StringId` (equal
    /// content means equal id, and `blacken_class` marks the keys).
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    pub(crate) methods: HashMap<StringId, Value>,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    pub(crate) kind: ClassKind,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    pub(crate) superclass: Option<ClassId>,
    /// Class variables in declaration order (inherited ones first, copied
    /// down at `OP_INHERIT` like methods). The order is observable: it is
    /// the field order of dataclasses. An `IndexMap` so name lookups are
    /// hashed while `get_index` keeps the ordered walks.
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    pub(crate) variables: IndexMap<StringId, ClassVariable>,
}

impl Class {
    #[must_use]
    pub(crate) fn new(name: StringId, kind: ClassKind) -> Self {
        Self {
            name,
            methods: HashMap::default(),
            kind,
            superclass: None,
            variables: IndexMap::new(),
        }
    }

    /// Declare (or redeclare, keeping the original position) a class
    /// variable.
    pub(crate) fn add_class_variable(
        &mut self,
        name: StringId,
        annotation: Value,
        default: Option<Value>,
    ) {
        self.variables.insert(
            name,
            ClassVariable {
                annotation,
                default,
            },
        );
    }

    /// Set the value of a class variable, declaring it (without an
    /// annotation) when it does not exist yet. An existing declaration
    /// keeps its annotation and position.
    pub(crate) fn set_class_variable_value(&mut self, name: StringId, value: Value) {
        if let Some(variable) = self.variables.get_mut(&name) {
            variable.default = Some(value);
        } else {
            self.variables.insert(
                name,
                ClassVariable {
                    annotation: Value::Nil,
                    default: Some(value),
                },
            );
        }
    }

    /// The current value of a class variable, if it is declared and has a
    /// default.
    /// Remove a class variable; the remaining declaration order is
    /// preserved (`shift_remove`), which dataclass field order relies on.
    pub(crate) fn remove_class_variable(&mut self, name: StringId) -> Option<ClassVariable> {
        self.variables.shift_remove(&name)
    }

    pub(crate) fn class_variable_value(&self, name: StringId) -> Option<Value> {
        self.variables
            .get(&name)
            .and_then(|variable| variable.default)
    }

    /// Whether this class stops the native-superclass walk. Both `Native` and
    /// `Plugin` do; only `User` continues up the chain.
    pub(crate) fn is_native(&self) -> bool {
        !matches!(self.kind, ClassKind::User)
    }

    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        format!("<class {}>", *self.name.to_value(heap))
    }

    pub(crate) fn get_native_superclass(&self, heap: &Heap, class_id: ClassId) -> Option<ClassId> {
        if self.is_native() {
            return Some(class_id); // We are the native class
        }

        if let Some(superclass_id) = self.superclass {
            let superclass = superclass_id.to_value(heap);
            if superclass.is_native() {
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

/// The class of a value, if it has one: instances carry their class, and the
/// value types map to their proxy classes (`Bool`, `String`, `Integer`,
/// `Float`, `Rational`). Everything else (nil, functions, classes, modules, …)
/// has no class and yields `None`.
pub fn class_of_value(heap: &Heap, value: Value) -> Option<ClassId> {
    match value {
        Value::Instance(instance) => Some(instance.to_value(heap).class),
        Value::Bool(_) => Some(get_native_class_id(heap, "Bool")),
        Value::String(_) => Some(get_native_class_id(heap, "String")),
        Value::Number(Number::Integer(_)) => Some(get_native_class_id(heap, "Integer")),
        Value::Number(Number::Float(_)) => Some(get_native_class_id(heap, "Float")),
        Value::Number(Number::Rational(_)) => Some(get_native_class_id(heap, "Rational")),
        Value::Nil => Some(get_native_class_id(heap, "NilType")),
        Value::StopIteration => Some(get_native_class_id(heap, "StopIterationType")),
        Value::Module(_) => Some(get_native_class_id(heap, "Module")),
        _ => None,
    }
}

/// Whether `value` is an instance of `class_id` or of a subclass of it -
/// the semantics of the `isinstance` builtin. Value types match their
/// proxy classes exactly (`Bool`, `String`, `Integer`, `Float`,
/// `Rational`, `Module`); everything else that is not an instance is
/// `false`.
pub fn value_isinstance(heap: &Heap, value: Value, class_id: ClassId) -> bool {
    match value {
        Value::Instance(instance) => is_subclass_of(heap, instance.to_value(heap).class, class_id),
        Value::Bool(_) => class_id == get_native_class_id(heap, "Bool"),
        Value::String(_) => class_id == get_native_class_id(heap, "String"),
        Value::Number(Number::Integer(_)) => class_id == get_native_class_id(heap, "Integer"),
        Value::Number(Number::Float(_)) => class_id == get_native_class_id(heap, "Float"),
        Value::Number(Number::Rational(_)) => class_id == get_native_class_id(heap, "Rational"),
        Value::Nil => class_id == get_native_class_id(heap, "NilType"),
        Value::StopIteration => class_id == get_native_class_id(heap, "StopIterationType"),
        Value::Module(_) => class_id == get_native_class_id(heap, "Module"),
        _ => false,
    }
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
        self.to_string_capped(heap, 0)
    }

    /// Depth-bounded variant used by the recursive `Value::to_string_capped`;
    /// `depth` is threaded into the backing container so a nested or cyclic
    /// value cannot overflow the host stack while being formatted.
    pub(crate) fn to_string_capped(&self, heap: &Heap, depth: usize) -> String {
        match &self.backing {
            // Exceptions render repr-style as `ClassName('message')`. The class
            // name lives on the instance, not the backing, so this is handled
            // here rather than in `NativeClass::to_string`.
            Some(NativeClass::Exception(exception)) => {
                let class_name = self.class.to_value(heap).name.to_value(heap);
                match exception.message() {
                    Some(message) => format!("{class_name}('{}')", message.to_value(heap)),
                    None => format!("{class_name}()"),
                }
            }
            // Plugin instances render like user instances: by class name. The
            // class name lives on the instance, so this is handled here rather
            // than in `NativeClass::to_string`.
            #[cfg(feature = "plugins")]
            Some(NativeClass::Plugin(_)) => format!(
                "<{} instance>",
                self.class.to_value(heap).name.to_value(heap)
            ),
            Some(native_class) => native_class.to_string(heap, depth),
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
