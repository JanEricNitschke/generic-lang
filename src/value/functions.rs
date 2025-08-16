use crate::{
    chunk::Chunk,
    enums::ImportType,
    heap::{FunctionId, Heap, ModuleId, StringId, UpvalueId},
    vm::Global,
};

use super::Value;

use derivative::Derivative;
use rustc_hash::FxHashMap as HashMap;
use std::path::PathBuf;

/// Closure are wrappers around runable code.
///
/// They contain a reference to the actual function they wrap,
/// the captured upvalues and their count.
///
/// They also additionally store whether the wrapped code
/// is a general function or a module.
///
/// For the correct resolution of global variables referenced from function
/// in imported modules, it is also necessary that the module that contains
/// that function is also available for that resolution.
#[derive(Debug, PartialOrd, Clone)]
pub struct Closure {
    pub(crate) function: FunctionId,
    pub(crate) upvalues: Vec<UpvalueId>,
    pub(crate) upvalue_count: usize,
    pub(crate) is_module: bool,
    pub(crate) containing_module: Option<ModuleId>,
}

impl Closure {
    pub(crate) fn new(
        function: FunctionId,
        is_module: bool,
        containing_module: Option<ModuleId>,
        heap: &Heap,
    ) -> Self {
        let upvalue_count = function.to_value(heap).upvalue_count;
        Self {
            function,
            upvalues: Vec::with_capacity(upvalue_count),
            upvalue_count,
            is_module,
            containing_module,
        }
    }

    pub(super) fn to_string(&self, heap: &Heap) -> String {
        self.function.to_value(heap).to_string(heap)
    }
}

impl PartialEq for Closure {
    fn eq(&self, _other: &Self) -> bool {
        // Two different closures are always considered different, even if they close over exactly the same things
        false
    }
}

impl std::fmt::Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<Closure Value>")
    }
}

/// Object for actual function implementations
///
/// Contains the name, number of expected arguments and number of
/// captured upvalues.
///
/// Additionally hold the chunk of compiled bytecode.
#[derive(Debug, Eq, Clone)]
pub struct Function {
    pub(crate) arity: usize,
    pub(crate) chunk: Chunk,
    pub(crate) name: StringId,
    pub(crate) upvalue_count: usize,
}

impl Function {
    #[must_use]
    pub(crate) fn new(arity: usize, name: StringId) -> Self {
        Self {
            arity,
            name,
            chunk: Chunk::new(name),
            upvalue_count: 0,
        }
    }

    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        format!("<fn {}>", *self.name.to_value(heap))
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<fn Value>")
    }
}

impl PartialEq for Function {
    fn eq(&self, _other: &Self) -> bool {
        // Two different functions are always considered different
        false
    }
}

#[derive(Debug, Clone, Derivative)]
#[derivative(PartialOrd)]
pub struct Module {
    pub(crate) name: StringId,
    pub(crate) path: PathBuf,
    #[derivative(PartialOrd = "ignore", PartialEq = "ignore")]
    pub(crate) globals: HashMap<StringId, Global>,
    pub(crate) names_to_import: Option<Vec<StringId>>,
    pub(crate) alias: StringId,
    pub(crate) local_import: ImportType,
}

impl Module {
    pub(crate) fn new(
        name: StringId,
        path: PathBuf,
        names_to_import: Option<Vec<StringId>>,
        alias: StringId,
        local_import: ImportType,
    ) -> Self {
        Self {
            name,
            path,
            globals: HashMap::default(),
            names_to_import,
            alias,
            local_import,
        }
    }

    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        format!("<module {}>", *self.name.to_value(heap))
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("<module Value>")
    }
}

impl Eq for Module {}

impl PartialEq for Module {
    fn eq(&self, _other: &Self) -> bool {
        // Two different modules are always considered different
        false
    }
}

/// Uncaptured (open) upvalues point to the stack index of the value,
/// while captured upvalues point to the value in the heap.
#[derive(Debug, Clone, PartialEq)]
pub enum Upvalue {
    Open(usize),
    Closed(Value),
}

impl Upvalue {
    pub(crate) fn as_open(&self) -> usize {
        match self {
            Self::Open(n) => *n,
            Self::Closed(_) => unreachable!("Only call as_open on a known open upvalue!"),
        }
    }

    pub(crate) fn eq(&self, other: &Self, heap: &Heap) -> bool {
        match (self, other) {
            (Self::Open(a), Self::Open(b)) => a == b,
            (Self::Closed(a), Self::Closed(b)) => a.eq(b, heap),
            _ => false,
        }
    }
}

impl std::fmt::Display for Upvalue {
    /// Upvalues are implementation details and should never be seen by the user.
    /// So this is only used for debugging.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("upvalue")
    }
}
