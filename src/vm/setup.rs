use std::path::PathBuf;

use crate::{
    compiler::Compiler,
    heap::StringId,
    scanner::Scanner,
    value::{
        Class, Closure, Module, NativeFunction, NativeFunctionImpl, NativeMethod, NativeMethodImpl,
    },
};

use super::{Global, VM};

impl VM {
    /// Add the given closure as a module and set the currently active scriptname.
    ///
    /// The module is added to the top of the module stack.
    pub(super) fn add_closure_to_modules(
        &mut self,
        closure: &Closure,
        file_path: PathBuf,
        names_to_import: Option<Vec<StringId>>,
        alias: Option<StringId>,
        local_import: bool,
    ) {
        if closure.is_module {
            let value_id = closure.function.to_value(&self.heap).name;
            let script_name = self.heap.builtin_constants().script_name;
            let alias = alias.map_or(value_id, |alias| alias);
            let module_id = self.heap.add_module(Module::new(
                value_id,
                file_path,
                names_to_import,
                alias,
                local_import,
            ));
            self.modules.push(*module_id.as_module());
            // Scriptname has to be set in the globals of the new module.
            self.globals().insert(
                script_name,
                Global {
                    value: value_id.into(),
                    mutable: true,
                },
            );
        }
    }

    /// Define a native function by adding it to the heap and the VM builtins.
    pub(crate) fn define_native_function<T: ToString>(
        &mut self,
        name: &T,
        arity: &'static [u8],
        fun: NativeFunctionImpl,
    ) {
        let name_id = self.heap.string_id(name);
        self.heap.strings_by_name.insert(name.to_string(), name_id);
        let value = self.heap.add_native_function(NativeFunction {
            name: name_id,
            arity,
            fun,
        });
        self.builtins.insert(
            name_id,
            Global {
                value,
                mutable: true,
            },
        );
    }

    /// Define a native class by adding it to the heap and optionally the VM builtins.
    pub(crate) fn define_native_class<T: ToString>(&mut self, name: &T, add_to_builtins: bool) {
        let name_id = self.heap.string_id(name);
        self.heap.strings_by_name.insert(name.to_string(), name_id);
        let value = self.heap.add_class(Class::new(name_id, true));
        if add_to_builtins {
            self.builtins.insert(
                name_id,
                Global {
                    value,
                    mutable: true,
                },
            );
        }
        self.heap
            .native_classes
            .insert(name_id.to_value(&self.heap).clone(), value);
    }

    /// Define a native method by adding it to the heap and the class.
    pub(crate) fn define_native_method<C: ToString, N: ToString>(
        &mut self,
        class: &C,
        name: &N,
        arity: &'static [u8],
        fun: NativeMethodImpl,
    ) {
        let class_id = self.heap.string_id(class);
        self.heap
            .strings_by_name
            .insert(class.to_string(), class_id);
        let name_id = self.heap.string_id(name);
        self.heap.strings_by_name.insert(name.to_string(), name_id);
        let value_id = self.heap.add_native_method(NativeMethod {
            class: class_id,
            name: name_id,
            arity,
            fun,
        });
        let target_class = self
            .heap
            .native_classes
            .get_mut(&class_id.to_value(&self.heap).clone())
            .unwrap()
            .as_class_mut()
            .clone()
            .to_value_mut(&mut self.heap);
        target_class.methods.insert(name_id, value_id);
    }

    /// Register a rust native stdlib module by its name and exported functions.
    ///
    /// Add the name of the module to the heap and add the exported functions
    /// to the stdlib map so that they can be loaded into the globals when the module is imported.
    pub(crate) fn register_stdlib_module<T: ToString>(
        &mut self,
        name: &T,
        functions: Vec<(&'static str, &'static [u8], NativeFunctionImpl)>,
    ) {
        let name_id = self.heap.string_id(name);
        self.heap.strings_by_name.insert(name.to_string(), name_id);
        self.stdlib.insert(name_id, functions);
    }

    /// Get the combined source code of all builtin files.
    fn get_builtins_source() -> Vec<u8> {
        let mut combined_source = Vec::new();

        // Get path to builtins directory
        let mut builtins_path = std::path::PathBuf::from(file!());
        builtins_path.pop(); // Remove mod.rs
        builtins_path.pop(); // Remove vm
        builtins_path.push("builtins");

        // Load and combine all builtin files
        if let Ok(entries) = std::fs::read_dir(&builtins_path) {
            for entry in entries.flatten() {
                if let Some(file_name) = entry.file_name().to_str()
                    && std::path::Path::new(file_name)
                        .extension()
                        .is_some_and(|ext| ext.eq_ignore_ascii_case("gen"))
                {
                    let file_path = entry.path();
                    if let Ok(contents) = std::fs::read(&file_path) {
                        combined_source.extend_from_slice(&contents);
                        combined_source.push(b'\n');
                    }
                }
            }
        }

        combined_source
    }

    /// Execute builtins source code and capture their globals into the builtins `HashMap`.
    ///
    /// This is called at startup before user code execution to populate the builtins
    /// that will be available globally in user programs.
    pub(super) fn execute_builtins(&mut self) {
        let builtins_source = Self::get_builtins_source();

        if builtins_source.is_empty() {
            return;
        }

        // Compile the builtins source
        let scanner = Scanner::new(&builtins_source);
        let compiler = Compiler::new(scanner, &mut self.heap, "<builtins>");
        if let Some(function) = compiler.compile() {
            let function_id = self.heap.add_function(function);
            let closure = Closure::new(*function_id.as_function(), true, None, &self.heap);

            // Create a temporary module for builtins execution
            let builtin_module_name = self.heap.string_id(&"<builtins>");
            let module_id = self.heap.add_module(Module::new(
                builtin_module_name,
                PathBuf::from("<builtins>"),
                None,
                builtin_module_name,
                false,
            ));

            // Save current modules state
            let original_modules = self.modules.clone();
            let original_stack_len = self.stack.len();

            // Set up for builtins execution
            self.modules.clear();
            self.modules.push(*module_id.as_module());

            let value_id = self.heap.add_closure(closure);
            self.stack_push_value(value_id);
            self.execute_call(value_id, 0);

            // Execute the builtins
            if self.run_function() == crate::vm::InterpretResult::Ok {
                // Capture globals from the builtins module
                let builtin_globals = self
                    .modules
                    .last()
                    .unwrap()
                    .to_value(&self.heap)
                    .globals
                    .clone();

                // Move all globals (except __name__) to the builtins HashMap
                let script_name = self.heap.builtin_constants().script_name;
                for (name, global) in builtin_globals {
                    if name != script_name {
                        self.builtins.insert(name, global);
                    }
                }
            }

            // Restore original state
            self.modules = original_modules;
            self.stack.truncate(original_stack_len);
        }
    }
}
