use super::{Global, VM};
use crate::vm::ExceptionKind::ImportError;

use path_slash::PathBufExt;
#[cfg(not(feature = "plugins"))]
use std::path::Path;
use std::path::PathBuf;

use crate::{
    config::GENERIC_STDLIB_DIR,
    heap::{ModuleId, StringId},
    value::{Closure, CreatorContext, Module, ModuleContents, ModuleExport, NativeFunction, Value},
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
            return self.throw(ImportError, "Import path should have a filestem.");
        };
        let name_id = self.heap.string_id(&name);

        // A file-backed module that was already imported binds its cached
        // object without reading the file from disk again.
        if let Some(cached) =
            self.bind_if_cached(&file_path, names_to_import.as_deref(), alias, local_import)
        {
            return cached;
        }

        // User defined generic module
        if let Ok(contents) = std::fs::read_to_string(&file_path) {
            // Check for circular imports only for user-defined modules
            // Skip stdlib modules since they're under our control and can't have circular imports
            if self.modules.iter().any(|module| {
                let module_path = &module.to_value(&self.heap).path;

                // Check if this is NOT a stdlib module
                let is_not_stdlib =
                    module_path
                        .file_name()
                        .and_then(|n| n.to_str())
                        .is_none_or(|filename| {
                            GENERIC_STDLIB_DIR
                                .get_file(format!("{filename}.gen"))
                                .is_none()
                        });

                is_not_stdlib
                    && module_path.canonicalize().ok().as_deref() == Some(file_path.as_path())
            }) {
                let message = format!(
                    "Circular import of module `{}` detected.",
                    name_id.to_value(&self.heap)
                );
                return self.throw(ImportError, &message);
            }

            self.import_generic_module(
                &contents,
                &name,
                file_path,
                names_to_import,
                alias,
                local_import,
            )?;
        } else if let Some(plugin_result) = self.try_import_plugin(
            &file_path,
            name_id,
            names_to_import.as_deref(),
            alias,
            local_import,
        ) {
            // Native plugin next to the resolved import path - deliberately
            // shadows stdlib modules. The feature gate lives on
            // `try_import_plugin` itself (a `#[cfg]` cannot sit on an
            // `else if` arm): with the `plugins` feature off this calls the
            // inlined always-`None` stub at the bottom of this file, so the
            // arm folds away entirely.
            plugin_result?;
        } else if let Some(cached) = self.bind_if_cached(
            // Both stdlib kinds are keyed by name: they resolve the same
            // from everywhere, unlike file-backed modules.
            &PathBuf::from(&name),
            names_to_import.as_deref(),
            alias,
            local_import,
        ) {
            return cached;
        } else if let Some(stdlib_file) = GENERIC_STDLIB_DIR.get_file(format!("{name}.gen")) {
            // stdlib generic module from embedded directory - no circular import check needed
            // since we have full control of stdlib modules
            self.import_generic_module(
                std::str::from_utf8(stdlib_file.contents())
                    .unwrap_or_else(|_| panic!("Invalid UTF-8 in generic stdlib module: {name}")),
                &name,
                PathBuf::from(&name),
                names_to_import,
                alias,
                local_import,
            )?;
        } else if let Some(stdlib_functions) = self.stdlib.get(&file_path_string_id).cloned() {
            // These clones are only necessary because this is extracted into a function.
            // If they cause performance issues this can be inlined or turned into a macro.
            self.import_rust_stdlib(
                file_path_string_id,
                PathBuf::from(&name),
                alias,
                &stdlib_functions,
                names_to_import.as_deref(),
                local_import,
            )?;
        } else {
            let message = format!(
                "Could not find the file to be imported. Attempted path `{:?}` and stdlib.",
                file_path.to_slash_lossy()
            );
            return self.throw(ImportError, &message);
        }
        Ok(None)
    }

    /// Build a native module from its `(name, value)` exports, cache it,
    /// and install it into the current scope (honoring `from`-imports,
    /// aliases, and local imports). Shared by the rust-stdlib and plugin
    /// importers, which differ only in how each export's `NativeFunction`
    /// is constructed.
    pub(super) fn install_native_module(
        &mut self,
        name_id: StringId,
        file_path: PathBuf,
        alias: Option<StringId>,
        exports: Vec<(StringId, Value)>,
        names_to_import: Option<&[StringId]>,
        local_import: bool,
    ) -> VmResult {
        let alias = alias.unwrap_or(name_id);
        let mut module = Module::new(name_id, file_path.clone(), None, alias, local_import);
        for (name, value) in exports {
            module.globals.insert(
                name,
                Global {
                    value,
                    mutable: false,
                },
            );
        }
        let module_value = self.heap.add_module(module);
        let module_id = *module_value.as_module();
        self.module_cache.insert(file_path, module_id);
        self.bind_cached_module(module_id, names_to_import, Some(alias), local_import)
    }

    /// Look up `cache_key` and bind the cached module if there is one.
    pub(in crate::vm) fn bind_if_cached(
        &mut self,
        cache_key: &PathBuf,
        names_to_import: Option<&[StringId]>,
        alias: Option<StringId>,
        local_import: bool,
    ) -> Option<VmResult> {
        let module_id = *self.module_cache.get(cache_key)?;
        Some(self.bind_cached_module(module_id, names_to_import, alias, local_import))
    }

    /// Bind an already-built module: a `from` import reads the named
    /// globals off the module, a plain import binds the module itself
    /// under the alias. Shared by the native-module installer and the
    /// cache-hit path of every module kind.
    pub(in crate::vm) fn bind_cached_module(
        &mut self,
        module_id: ModuleId,
        names_to_import: Option<&[StringId]>,
        alias: Option<StringId>,
        local_import: bool,
    ) -> VmResult {
        if let Some(names_to_import) = names_to_import {
            // A `from` import binds every name into the one defining module.
            let defining_module = (!local_import).then(|| self.defining_module());
            for name in names_to_import {
                let Some(global) = module_id.to_value(&self.heap).globals.get(name).copied() else {
                    let message = format!(
                        "Could not find name to import `{}`.",
                        name.to_value(&self.heap)
                    );
                    return self.throw(ImportError, &message);
                };
                if let Some(defining_module) = defining_module {
                    defining_module
                        .to_value_mut(&mut self.heap)
                        .globals
                        .insert(*name, global);
                } else {
                    self.stack_push(global.value);
                }
            }
        } else {
            let alias = alias.unwrap_or_else(|| module_id.to_value(&self.heap).name);
            if local_import {
                self.stack_push(module_id.into());
            } else {
                self.defining_globals_mut().insert(
                    alias,
                    Global {
                        value: module_id.into(),
                        mutable: true,
                    },
                );
            }
        }
        Ok(None)
    }

    /// Import a rust native stdlib module.
    fn import_rust_stdlib(
        &mut self,
        string_id: StringId,
        file_path: PathBuf,
        alias: Option<StringId>,
        stdlib_exports: &ModuleContents,
        names_to_import: Option<&[StringId]>,
        local_import: bool,
    ) -> VmResult {
        // A value creator may re-enter the VM and collect. Root both the name
        // and the value of every export on the VM stack until the module takes
        // ownership: the name is rooted the moment it is interned (before the
        // creator that could collect runs), so a plain value that does not
        // reference its own name string still cannot have that name swept.
        let exports_base = self.stack.len();
        let mut exports: Vec<(StringId, Value)> = Vec::with_capacity(stdlib_exports.len());
        for export in stdlib_exports {
            let (name_id, value) = match export {
                ModuleExport::Function { name, arity, fun } => {
                    let name_id = self.heap.string_id(name);
                    self.stack.push(name_id.into());
                    let value = self.heap.add_native_function(NativeFunction {
                        name: name_id,
                        arity,
                        fun: *fun,
                        #[cfg(feature = "plugins")]
                        plugin_fn: None,
                    });
                    (name_id, value)
                }
                ModuleExport::Class { name } => {
                    let name_id = self.heap.string_id(name);
                    self.stack.push(name_id.into());
                    let class_id = *self.heap.native_classes.get(*name).unwrap_or_else(|| {
                        unreachable!("Stdlib module exports unregistered native class `{name}`.")
                    });
                    (name_id, class_id.into())
                }
                ModuleExport::Value { name, create } => {
                    let name_id = self.heap.string_id(name);
                    self.stack.push(name_id.into());
                    (name_id, create(self, &CreatorContext { name }))
                }
            };
            self.stack.push(value);
            exports.push((name_id, value));
        }
        let result = self.install_native_module(
            string_id,
            file_path,
            alias,
            exports,
            names_to_import,
            local_import,
        );
        // Drop the rooted name/value pairs from under whatever
        // `install_native_module` left on top (locally imported values, or a
        // pending exception).
        self.stack
            .drain(exports_base..exports_base + 2 * stdlib_exports.len());
        result
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
            self.throw(
                ImportError,
                &format!("Could not compile module to import `{name}`."),
            )
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

    /// Import fallback arm 2 when plugins are disabled: never matches, so
    /// the chain in `import_file` falls through to the stdlib arms
    /// untouched. The real implementation lives in `vm/plugins/` (whose
    /// `mod` declaration carries the feature gate).
    #[cfg(not(feature = "plugins"))]
    #[inline]
    #[allow(
        clippy::unnecessary_wraps,
        clippy::unused_self,
        // The signature must match the feature-gated implementation, which
        // needs `&mut self`.
        clippy::needless_pass_by_ref_mut
    )]
    pub(crate) fn try_import_plugin(
        &mut self,
        _file_path: &Path,
        _name_id: StringId,
        _names_to_import: Option<&[StringId]>,
        _alias: Option<StringId>,
        _local_import: bool,
    ) -> Option<VmResult> {
        None
    }
}
