use super::VM;

impl VM {
    /// Call the heap garbage collector.
    ///
    /// Return early if not gc is needed because the heap is still small.
    /// Mark all the roots that can be reached from the VM.
    /// - The stack
    /// - The callstack
    /// - The open upvalues
    /// - The modules
    /// - The builtins (names and values)
    /// - The stdlib registry's interned module names
    /// - The plugin export cache's interned names (`plugins` feature)
    ///
    /// Trace all the references from the roots.
    /// Remove all the unmarked strings from the globals and heap strings.
    /// Lastly, delete all the unmarked values from the heap.
    pub(super) fn collect_garbage(&mut self) {
        #[cfg(not(feature = "stress_gc"))]
        if !self.heap.needs_gc() {
            return;
        }

        self.heap.gc_start();

        // Mark roots
        #[cfg(feature = "log_gc")]
        eprintln!("Marking stack values.");
        for value in &self.stack {
            self.heap.mark_value(value);
        }
        #[cfg(feature = "log_gc")]
        eprintln!("Callstack functions.");
        for frame in self.callstack.iter() {
            self.heap.mark_function(frame.closure(&self.heap).function);
        }
        #[cfg(feature = "log_gc")]
        eprintln!("Marking open upvalues.");
        for upvalue in &self.open_upvalues {
            self.heap.mark_upvalue(*upvalue);
        }
        #[cfg(feature = "log_gc")]
        eprintln!("Marking modules.");
        for module in &self.modules {
            self.heap.mark_module(*module);
        }
        // Builtin names must not rely on their values carrying the same
        // interned string (native functions, classes, and closures happen
        // to; a plain constant or an aliased binding would not).
        #[cfg(feature = "log_gc")]
        eprintln!("Marking builtins.");
        for (name_id, builtin) in &self.builtins {
            self.heap.mark_value(&(*name_id).into());
            self.heap.mark_value(&builtin.value);
        }
        // The stdlib registry is keyed by interned module names. A module
        // that no compiled chunk mentions yet (e.g. the native half of a
        // mixed module, imported only from its generic half once that
        // compiles) has no other root, and losing the key loses the module.
        #[cfg(feature = "log_gc")]
        eprintln!("Marking stdlib module names.");
        for name_id in self.stdlib.keys() {
            self.heap.mark_value(&(*name_id).into());
        }
        // The plugin loader's per-path export cache holds interned name
        // `StringId`s that a cache-hit re-import feeds into module setup;
        // nothing else keeps them reachable, so they are roots.
        #[cfg(feature = "plugins")]
        {
            #[cfg(feature = "log_gc")]
            eprintln!("Marking plugin export names.");
            for exports in self.plugins.loaded.values() {
                for (name_id, _, _) in &exports.functions {
                    self.heap.mark_value(&(*name_id).into());
                }
                for (class_name_id, _drop, _traverse, methods) in &exports.classes {
                    self.heap.mark_value(&(*class_name_id).into());
                    for (method_name_id, _arities, _fun) in methods {
                        self.heap.mark_value(&(*method_name_id).into());
                    }
                }
            }
        }

        // Trace references
        self.heap.trace();

        // Remove references to unmarked strings in `heap.strings_by_name`.
        let mut strings_by_name = std::mem::take(&mut self.heap.strings_by_name);
        strings_by_name.retain(|_, string_id| {
            #[cfg(feature = "log_gc")]
            if !string_id.marked(&self.heap) {
                eprintln!(
                    "String/{:?} free from strings by name {}",
                    string_id,
                    string_id.to_value(&self.heap)
                );
            }
            string_id.marked(&self.heap)
        });
        self.heap.strings_by_name = strings_by_name;

        // Finally, sweep
        self.heap.sweep();
    }
}
