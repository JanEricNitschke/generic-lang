use super::{Global, VM};

use path_slash::PathBufExt;
use std::path::PathBuf;

use crate::{
    config::GENERIC_STDLIB_DIR,
    heap::StringId,
    value::{Closure, Module, ModuleContents, NativeFunction},
    vm::errors::VmResult,
};

impl VM {
    /// Functionality for importing a file as a module.
    ///
    /// Optionally a set of names can be given to import instead of the whole module
    /// or an alias can be given to import the module under a different name.
    ///
    /// Can either import a user defined module by relative filepath or a stdlib module by name.
    #[allow(clippy::option_if_let_else)]
    pub(super) fn import_file(
        &mut self,
        file_path_string_id: StringId,
        names_to_import: Option<Vec<StringId>>,
        alias: Option<StringId>,
        local_import: bool,
    ) -> VmResult {
        let file_path = self.clean_filepath(file_path_string_id);

        let name = if let Some(stem) = file_path.file_stem() {
            stem.to_str().unwrap().to_string()
        } else {
            return self.throw_import_error("Import path should have a filestem.");
        };
        let name_id = self.heap.string_id(&name);

        // User defined generic module
        if let Ok(contents) = std::fs::read_to_string(&file_path) {
            // Check for circular imports only for user-defined modules
            // Skip stdlib modules since they're under our control and can't have circular imports
            if self.modules.iter().any(|module| {
                let module_path = &module.to_value(&self.heap).path;

                // Check if this is NOT a stdlib module
                let is_not_stdlib = module_path
                    .file_name()
                    .and_then(|n| n.to_str())
                    .is_none_or(|filename| GENERIC_STDLIB_DIR.get_file(filename).is_none());

                is_not_stdlib && module_path.canonicalize().unwrap() == file_path
            }) {
                let message = format!(
                    "Circular import of module `{}` detected.",
                    name_id.to_value(&self.heap)
                );
                return self.throw_import_error(&message);
            }

            self.import_generic_module(
                &contents,
                &name,
                file_path,
                names_to_import,
                alias,
                local_import,
            )?;
        } else if let Some(stdlib_file) = GENERIC_STDLIB_DIR.get_file(format!("{name}.gen")) {
            // stdlib generic module from embedded directory - no circular import check needed
            // since we have full control of stdlib modules
            self.import_generic_module(
                std::str::from_utf8(stdlib_file.contents())
                    .unwrap_or_else(|_| panic!("Invalid UTF-8 in generic stdlib module: {name}")),
                &name,
                file_path,
                names_to_import,
                alias,
                local_import,
            )?;
        } else if let Some(stdlib_functions) = self.stdlib.get(&file_path_string_id).cloned() {
            // These clones are only necessary because this is extracted into a function.
            // If they cause performance issues this can be inlined or turned into a macro.
            self.import_rust_stdlib(
                file_path_string_id,
                file_path,
                alias,
                &stdlib_functions,
                names_to_import,
                local_import,
            )?;
        } else {
            let message = format!(
                "Could not find the file to be imported. Attempted path `{:?}` and stdlib.",
                file_path.to_slash_lossy()
            );
            return self.throw_import_error(&message);
        }
        Ok(None)
    }

    /// Import a rust native stdlib module.
    fn import_rust_stdlib(
        &mut self,
        string_id: StringId,
        file_path: PathBuf,
        alias: Option<StringId>,
        stdlib_functions: &ModuleContents,
        names_to_import: Option<Vec<StringId>>,
        local_import: bool,
    ) -> VmResult {
        let mut module = Module::new(
            string_id,
            file_path,
            None,
            alias.map_or(string_id, |alias| alias),
            local_import,
        );
        for (name, arity, fun) in stdlib_functions {
            let name_id = self.heap.string_id(name);
            let value = self.heap.add_native_function(NativeFunction {
                name: name_id,
                arity,
                fun: *fun,
            });
            module.globals.insert(
                name_id,
                Global {
                    value,
                    mutable: false,
                },
            );
        }
        // Stdlib rust module
        // Add all the functions to the modules globals
        // If we only want to import some functions then we just move them
        // from the new module to the current globals, the module then gets dropped.
        if let Some(names_to_import) = names_to_import {
            for name in names_to_import {
                if let Some(global) = module.globals.remove(&name) {
                    if local_import {
                        self.stack_push(global.value);
                    } else {
                        self.globals_mut().insert(name, global);
                    }
                } else {
                    let message = format!(
                        "Could not find name to import `{}`.",
                        name.to_value(&self.heap)
                    );
                    return self.throw_import_error(&message);
                }
            }
        } else {
            // Otherwise we add the whole module to the current globals.
            let module_id = self.heap.add_module(module);
            if local_import {
                self.stack_push(module_id);
            } else {
                self.globals_mut().insert(
                    string_id,
                    Global {
                        value: module_id,
                        mutable: true,
                    },
                );
            }
        }
        Ok(None)
    }

    /// Import a generic module.
    ///
    /// This can either be a user defined module or a stdlib module.
    /// Creates the module, adds it to the module list, and schedules the closure to be run.
    fn import_generic_module(
        &mut self,
        contents: &str,
        name: &str,
        file_path: PathBuf,
        names_to_import: Option<Vec<StringId>>,
        alias: Option<StringId>,
        local_import: bool,
    ) -> VmResult {
        if let Some(function) = self.compile(
            contents,
            name,
            #[cfg(any(
                feature = "print_code",
                feature = "debug_scanner",
                feature = "debug_parser"
            ))]
            false,
        ) {
            let function = self.heap.add_function(function);
            let function_id = function.as_function();
            let closure =
                Closure::new(*function_id, true, self.modules.last().copied(), &self.heap);

            self.add_closure_to_modules(&closure, file_path, names_to_import, alias, local_import);

            let value_id = self.heap.add_closure(closure);
            self.stack_push(value_id);
            self.execute_call(value_id, 0)
        } else {
            self.throw_import_error(&format!("Could not compile module to import `{name}`."))
        }
    }

    #[allow(clippy::option_if_let_else)]
    fn clean_filepath(&self, string_id: StringId) -> PathBuf {
        let file_path = self.modules.last().map_or_else(
            || PathBuf::from(string_id.to_value(&self.heap)),
            |module| {
                let mut path = module.to_value(&self.heap).path.clone();
                path.pop();
                path.push(string_id.to_value(&self.heap));
                path
            },
        );

        let file_path = match file_path.strip_prefix("./") {
            Ok(file_path) => file_path.to_owned(),
            Err(_) => file_path,
        };
        match file_path.canonicalize() {
            Ok(file_path) => file_path,
            Err(_) => file_path,
        }
    }
}
