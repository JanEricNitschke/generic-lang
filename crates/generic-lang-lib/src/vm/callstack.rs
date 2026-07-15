use crate::heap::{ClosureId, FunctionId, Heap};
use crate::value::Closure;

/// A call frame for the call stack.
///
/// Contains the closure corresponding to the relevant function,
/// the instruction pointer within the function, as well as the `stack_base`
/// of the function in the global stack.
///
/// Additionally, it contains a boolean indicating whether the closure is a module,
/// in order to handle transferring globals on module end.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct CallFrame {
    pub closure: ClosureId,
    pub ip: usize,
    pub stack_base: usize,
    pub is_module: bool,
}

impl CallFrame {
    pub fn closure<'a>(&self, heap: &'a Heap) -> &'a Closure {
        self.closure.to_value(heap)
    }

    pub fn from_closure_id(closure: ClosureId) -> Self {
        Self {
            closure,
            ip: 0,
            stack_base: 0,
            is_module: false,
        }
    }
}

/// Actual call stack of the VM.
///
/// Contains stored references for the current closure and function,
/// to not have to grab them from the vector every time.
#[derive(Debug)]
pub struct CallStack {
    frames: Vec<CallFrame>,
    // Maybe this could either be a straight pointer or at least not an Option.
    current_closure: Option<ClosureId>,
    current_function: Option<FunctionId>,
}

impl CallStack {
    #[must_use]
    pub(super) fn new() -> Self {
        Self {
            frames: Vec::with_capacity(crate::config::FRAMES_MAX),
            current_closure: None,
            current_function: None,
        }
    }

    pub(super) fn iter(&self) -> std::slice::Iter<'_, CallFrame> {
        self.frames.iter()
    }

    pub(super) fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    pub fn set_currents(&mut self, heap: &Heap) {
        self.current_closure = self.frames.last().map(|f| f.closure);
        self.current_function = self.current_closure.map(|c| c.to_value(heap).function);
    }

    pub fn pop(&mut self, heap: &Heap) -> Option<CallFrame> {
        let retval = self.frames.pop();
        self.set_currents(heap);
        retval
    }

    pub(super) fn truncate(&mut self, n: usize, heap: &Heap) {
        self.frames.truncate(n);
        self.set_currents(heap);
    }

    pub(super) fn push(&mut self, closure: ClosureId, stack_base: usize, heap: &Heap) {
        self.frames.push(CallFrame {
            closure,
            ip: 0,
            stack_base,
            is_module: closure.to_value(heap).is_module,
        });
        self.current_closure = Some(closure);
        self.current_function = Some(closure.to_value(heap).function);
    }

    pub(crate) fn push_callframe(&mut self, callframe: CallFrame, heap: &Heap) {
        self.current_closure = Some(callframe.closure);
        self.current_function = Some(callframe.closure.to_value(heap).function);
        self.frames.push(callframe);
    }

    pub(super) fn current_mut(&mut self) -> &mut CallFrame {
        let i = self.frames.len() - 1;
        &mut self.frames[i]
    }

    pub(super) fn current(&self) -> &CallFrame {
        let i = self.frames.len() - 1;
        &self.frames[i]
    }

    pub(super) fn code_byte(&self, index: usize, heap: &Heap) -> u8 {
        self.current_function.unwrap().to_value(heap).chunk.code()[index]
    }

    pub(super) const fn closure(&self) -> ClosureId {
        self.current_closure.unwrap()
    }

    pub(super) const fn function(&self) -> FunctionId {
        self.current_function.unwrap()
    }

    pub(crate) fn len(&self) -> usize {
        self.frames.len()
    }
}
