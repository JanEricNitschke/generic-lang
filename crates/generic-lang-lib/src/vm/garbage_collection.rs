use super::VM;

macro_rules! clear_garbage {
    ($collection:expr, $heap:expr, $collection_name:expr $(,)?) => {
        $collection.retain(|string_id, _| {
            #[cfg(feature = "log_gc")]
            if !string_id.marked($heap) {
                eprintln!(
                    "String/{:?} free from {} {}",
                    string_id,
                    $collection_name,
                    string_id.to_value($heap)
                );
            }
            string_id.marked($heap)
        });
    };
}

impl VM {
    /// Call the heap garbage collector.
    ///
    /// Return early if not gc is needed because the heap is still small.
    /// Mark all the roots that can be reached from the VM.
    /// - The stack
    /// - The callstack
    /// - The open upvalues
    /// - The modules
    /// - The builtins
    ///
    /// Trace all the references from the roots.
    /// Remove all the unmarked strings from the globals, builtins and heap strings.
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
        #[cfg(feature = "log_gc")]
        eprintln!("Marking builtins.");
        for builtin in self.builtins.values() {
            self.heap.mark_value(&builtin.value);
        }

        // Trace references
        self.heap.trace();

        // Remove references to unmarked strings in `globals` and `heap.strings_by_name`
        // and `builtins`
        for module_id in &mut self.modules.iter().copied() {
            let module = module_id.to_value_mut(&mut self.heap);
            let mut globals = std::mem::take(&mut module.globals);
            clear_garbage!(&mut globals, &self.heap, "module globals");
            module_id.to_value_mut(&mut self.heap).globals = globals;
        }

        clear_garbage!(&mut self.builtins, &self.heap, "builtins");

        clear_garbage!(&mut self.stdlib, &self.heap, "stdlib");

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
