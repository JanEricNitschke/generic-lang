use std::path::PathBuf;

use crate::{
    enums::ImportType,
    heap::StringId,
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
        local_import: ImportType,
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
}
