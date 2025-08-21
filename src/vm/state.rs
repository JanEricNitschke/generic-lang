use rustc_hash::FxHashMap as HashMap;

use crate::heap::{ModuleId, StringId};

use super::{Global, VM};

impl VM {
    pub(super) fn globals(&self) -> &HashMap<StringId, Global> {
        &self.modules.last().unwrap().to_value(&self.heap).globals
    }

    pub(super) fn globals_mut(&mut self) -> &mut HashMap<StringId, Global> {
        &mut self
            .modules
            .last_mut()
            .unwrap()
            .to_value_mut(&mut self.heap)
            .globals
    }

    pub(super) fn current_module(&self) -> ModuleId {
        *self.modules.last().unwrap()
    }

    /// Get the module present when the current closure was defined.
    ///
    /// Needed to properly handle captured globals in modules.
    /// Module closures have to be handles specially as while they are being defined
    /// they are not yet in the module list.
    ///
    /// Normally a module is compiled to bytecode. Then it is scheduled to be
    /// executed in the main loop. Its `containing_module` is the one that imports it.
    /// However, when it is actually executed it is on top of the module list
    /// and this is where the globals have to come from. Not from its `containing_module`.
    ///
    /// This is different for normal function closures. They are created once the `fun`
    /// statement is executed at runtime. At this point the module is already on the
    /// module list and correctly becomes their `containing_module`. Then, whenever they
    /// are actually called, they refer to the correct globals.
    pub(super) fn defining_module(&self) -> ModuleId {
        let current_closure = self.callstack.current().closure.to_value(&self.heap);
        match current_closure.containing_module {
            Some(module) if !current_closure.is_module => module,
            _ => self.current_module(),
        }
    }
}
