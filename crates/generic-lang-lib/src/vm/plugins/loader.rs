//! Plugin discovery and loading: import fallback arm 2.
//!
//! Import resolution tries `<dir>/{<name>,lib<name>}.<dylib-ext>` next to
//! the resolved import path, directly after the user-file arm - so a plugin
//! deliberately shadows stdlib modules of the same name. A loaded library
//! lives as long as the VM (its heap `NativeFunction`s hold `extern "C"`
//! pointers into it), and re-importing the same path rebuilds the module
//! from the cached exports instead of re-loading the library.

use core::ffi::c_void;
use std::env::consts::{DLL_PREFIX, DLL_SUFFIX};
use std::path::{Path, PathBuf};
use std::{slice, str};

use libloading::Library;
use rustc_hash::FxHashMap as HashMap;

use generic_lang_api::{
    FfiStr, GENERIC_PLUGIN_ABI_VERSION, ModuleDesc, PluginFn, PluginMethodFn, PluginTraverseFn,
    PluginValueFn,
};

use super::trampolines::{call_plugin_value, plugin_trampoline};
use crate::heap::StringId;
use crate::value::{Class, ClassKind, NativeFunction, NativeMethod, PluginClassInfo, Value};
use crate::vm::ExceptionKind::ImportError;
use crate::vm::VM;
use crate::vm::errors::{VmErrorKind, VmResult};

/// One exported function of a loaded plugin: name, arity slice (leaked once at
/// load - bounded, plugins never unload), and the `extern "C"` pointer.
type PluginFunctionExport = (StringId, &'static [u8], PluginFn);

/// A plugin class's drop callback (frees the opaque state). `unsafe` like
/// every plugin-provided function pointer - see `ClassDesc::drop`.
type PluginDropFn = unsafe extern "C" fn(*mut c_void);

/// One method of a loaded plugin class: name, arity slice (excluding the
/// receiver, leaked once at load), and the `extern "C"` pointer.
type PluginMethodExport = (StringId, &'static [u8], PluginMethodFn);

/// One class of a loaded plugin: name, drop fn, traverse fn, and its methods.
type PluginClassExport = (
    StringId,
    Option<PluginDropFn>,
    Option<PluginTraverseFn>,
    Vec<PluginMethodExport>,
);

/// One exported module value of a loaded plugin: name and the creator
/// called at import time.
type PluginValueExport = (StringId, PluginValueFn);

/// A validated-but-not-yet-interned function: borrowed name, arity slice, and
/// pointer - all borrowing the descriptor for the library's lifetime.
type ValidatedFunction<'d> = (&'d str, &'d [u8], PluginFn);

/// A validated-but-not-yet-interned method (like [`ValidatedFunction`], but the
/// pointer is a [`PluginMethodFn`] and the arity excludes the receiver).
type ValidatedMethod<'d> = (&'d str, &'d [u8], PluginMethodFn);

/// A validated-but-not-yet-interned class: borrowed name, callbacks, and method
/// table - all borrowing the descriptor for the library's lifetime.
type ValidatedClass<'d> = (
    &'d str,
    Option<PluginDropFn>,
    Option<PluginTraverseFn>,
    Vec<ValidatedMethod<'d>>,
);

/// A validated-but-not-yet-interned module value: borrowed name and creator.
type ValidatedValue<'d> = (&'d str, PluginValueFn);

/// Everything cached per loaded plugin path: functions, classes, and
/// module values.
#[derive(Default, Clone)]
pub(crate) struct PluginModuleExports {
    pub(in crate::vm) functions: Vec<PluginFunctionExport>,
    pub(in crate::vm) classes: Vec<PluginClassExport>,
    pub(in crate::vm) values: Vec<PluginValueExport>,
}

/// Per-VM plugin state: the loaded libraries (kept alive for the VM's
/// lifetime) and the per-path export cache.
#[derive(Default)]
pub(crate) struct PluginState {
    /// Never dropped while the VM lives - unloading would dangle the
    /// `plugin_fn` pointers held by heap natives and the leaked descriptor
    /// memory.
    libraries: Vec<Library>,
    /// Re-imports of the same dylib rebuild the module from here instead of
    /// re-loading the library. Keyed by the canonicalized plugin path so
    /// every spelling of the same file shares one entry (and one loaded
    /// library). The cached function, class, and method name `StringId`s are
    /// GC roots, marked by `collect_garbage` like every other root category.
    pub(in crate::vm) loaded: HashMap<PathBuf, PluginModuleExports>,
}

impl VM {
    /// Import fallback arm 2: load `name` as a native plugin if a shared
    /// library exists next to the resolved import path.
    ///
    /// Returns `None` if no plugin candidate file exists (the chain falls
    /// through to the stdlib arms); `Some(result)` if one does - where
    /// loading errors (bad init symbol, ABI mismatch, malformed exports)
    /// are thrown `ImportError`s.
    #[allow(unsafe_code)]
    pub(crate) fn try_import_plugin(
        &mut self,
        file_path: &Path,
        name_id: StringId,
        names_to_import: Option<&[StringId]>,
        alias: Option<StringId>,
        local_import: bool,
    ) -> Option<VmResult> {
        let path = find_plugin_candidate(file_path, name_id.to_value(&self.heap))?;
        // Canonicalize the cache key so different spellings of the same
        // file (relative vs. absolute, `./` prefixes, symlinks) share one
        // entry and one loaded library. The candidate was just found on
        // disk, so this only fails on races - fall back to the raw path.
        let path = path.canonicalize().unwrap_or(path);

        if let Some(cached) = self.bind_if_cached(&path, names_to_import, alias, local_import) {
            return Some(cached);
        }

        // Failed loads are deliberately not cached.
        let exports = if let Some(exports) = self.plugins.loaded.get(&path) {
            exports.clone()
        } else {
            match self.load_plugin_library(&path) {
                Ok(exports) => exports,
                Err(result) => return Some(result),
            }
        };

        // SAFETY: `exports` came from `load_plugin_library` or from the cache
        // it fills, so every pointer in it belongs to a loaded, validated
        // library.
        Some(unsafe {
            self.import_plugin_module(
                name_id,
                path,
                alias,
                &exports,
                names_to_import,
                local_import,
            )
        })
    }

    /// Register a plugin class: create a `Class` of `ClassKind::Plugin`, add its
    /// methods as `NativeMethod`s carrying `plugin_fn`, and return the class as
    /// a `Value::Class`.
    ///
    /// # Safety
    ///
    /// `drop`, `traverse`, and the method pointers must come from a validated
    /// descriptor of a loaded library: they are stored on the class and the GC
    /// calls `drop`/`traverse` later, long after this returns.
    #[allow(unsafe_code)]
    unsafe fn add_plugin_class(
        &mut self,
        name: StringId,
        drop: Option<PluginDropFn>,
        traverse: Option<PluginTraverseFn>,
        methods: &[PluginMethodExport],
    ) -> Value {
        let class = Class::new(name, ClassKind::Plugin(PluginClassInfo { drop, traverse }));
        let class_value = self.heap.add_class(class);
        for (method_name, arities, fun) in methods {
            // SAFETY: by this function's contract the methods come from a
            // validated descriptor.
            let method_value = unsafe { self.add_plugin_method(name, *method_name, arities, *fun) };
            class_value
                .as_class()
                .to_value_mut(&mut self.heap)
                .methods
                .insert(*method_name, method_value);
        }
        class_value
    }

    /// Register a plugin method as a `NativeMethod` carrying `plugin_fn`; its
    /// `fun` is the `plugin_method_sentinel` (dispatch branches on `plugin_fn`
    /// before ever calling `fun`).
    ///
    /// # Safety
    ///
    /// `fun` must be a real plugin method from a loaded library, since the
    /// stored pointer is called on every later dispatch.
    #[allow(unsafe_code)]
    unsafe fn add_plugin_method(
        &mut self,
        class_name: StringId,
        name: StringId,
        arity: &'static [u8],
        fun: PluginMethodFn,
    ) -> Value {
        self.heap.add_native_method(NativeMethod {
            class: class_name,
            name,
            arity,
            fun: plugin_method_sentinel,
            plugin_fn: Some(fun),
        })
    }

    /// Load the shared library and validate its descriptor. On success the
    /// library is retained and the exports cached under `path`.
    ///
    /// `Err` carries the already-thrown `ImportError`.
    // The one place in the loader that touches the FFI: dlopen, symbol
    // resolution, and reading the plugin-provided descriptor tables.
    #[allow(unsafe_code, clippy::too_many_lines)]
    fn load_plugin_library(&mut self, path: &Path) -> Result<PluginModuleExports, VmResult> {
        macro_rules! import_error {
            ($($arg:tt)*) => {
                return Err(self.throw(ImportError, &format!($($arg)*)))
            };
        }

        // libloading's `Display` is generic ("dlopen failed"); the actual
        // `dlerror`/OS detail is the error's `source`.
        fn describe(error: &libloading::Error) -> String {
            std::error::Error::source(error)
                .map_or_else(|| error.to_string(), |source| format!("{error}: {source}"))
        }

        // SAFETY: loading a shared library runs its initializers - this is
        // the trust boundary of the plugin system (plugins are trusted
        // native code).
        let library = match unsafe { Library::new(path) } {
            Ok(library) => library,
            Err(error) => import_error!(
                "Failed to load plugin `{}`: {}",
                path.display(),
                describe(&error)
            ),
        };

        // SAFETY: the symbol is declared with exactly this signature in the
        // plugin ABI (`generic_plugin_init` in generic.h). Nothing verifies
        // that the loaded library agrees - asserting it is the point of this
        // `unsafe`, and the reason the resolved pointer is an `unsafe fn`.
        let init = match unsafe {
            library.get::<unsafe extern "C" fn() -> *const ModuleDesc>(b"generic_plugin_init")
        } {
            Ok(symbol) => symbol,
            Err(error) => import_error!(
                "Plugin `{}` does not export `generic_plugin_init`: {}",
                path.display(),
                describe(&error)
            ),
        };

        // SAFETY: calling a function resolved out of a freshly loaded library,
        // under the signature asserted above; the ABI says it takes no
        // arguments and only hands back its static descriptor.
        let desc = unsafe { init() };
        if desc.is_null() {
            import_error!(
                "Plugin `{}` returned no module description.",
                path.display()
            );
        }
        // SAFETY: non-null descriptor returned by the plugin's init; the ABI
        // requires it to stay valid for the lifetime of the library, which
        // the VM keeps alive.
        let desc = unsafe { &*desc };

        if desc.abi_version != GENERIC_PLUGIN_ABI_VERSION {
            import_error!(
                "Plugin `{}` speaks ABI version {}, host expects {}.",
                path.display(),
                desc.abi_version,
                GENERIC_PLUGIN_ABI_VERSION
            );
        }

        // Validate the whole descriptor - functions and classes - before
        // interning or leaking anything, so a plugin rejected partway through
        // never leaks the allocations of the entries validated before it.
        let functions = self.validate_plugin_functions(path, desc)?;
        let classes = self.validate_plugin_classes(path, desc)?;
        let values = self.validate_plugin_values(path, desc)?;

        // Everything validated: intern names and leak the arity slices. Leaked
        // once per entry at load - bounded, since plugins are cached per
        // canonical path and never unloaded.
        let function_exports: Vec<PluginFunctionExport> = functions
            .into_iter()
            .map(|(name, arities, fun)| (self.heap.string_id(&name), leak_arities(arities), fun))
            .collect();
        let class_exports: Vec<PluginClassExport> = classes
            .into_iter()
            .map(|(name, drop, traverse, methods)| {
                let class_name_id = self.heap.string_id(&name);
                let methods: Vec<PluginMethodExport> = methods
                    .into_iter()
                    .map(|(mname, arities, fun)| {
                        (self.heap.string_id(&mname), leak_arities(arities), fun)
                    })
                    .collect();
                (class_name_id, drop, traverse, methods)
            })
            .collect();

        let value_exports: Vec<PluginValueExport> = values
            .into_iter()
            .map(|(name, fun)| (self.heap.string_id(&name), fun))
            .collect();

        let exports = PluginModuleExports {
            functions: function_exports,
            classes: class_exports,
            values: value_exports,
        };

        self.plugins.libraries.push(library);
        self.plugins
            .loaded
            .insert(path.to_path_buf(), exports.clone());
        Ok(exports)
    }

    /// Validate the descriptor's function table without interning: for each
    /// function, the borrowed name, arity slice, and pointer. `Err` is an
    /// already-thrown `ImportError`.
    #[allow(unsafe_code)]
    fn validate_plugin_functions<'d>(
        &mut self,
        path: &Path,
        desc: &'d ModuleDesc,
    ) -> Result<Vec<ValidatedFunction<'d>>, VmResult> {
        macro_rules! import_error {
            ($($arg:tt)*) => { return Err(self.throw(ImportError, &format!($($arg)*))) };
        }

        let functions = if desc.functions_len == 0 {
            &[]
        } else {
            if desc.functions.is_null() {
                import_error!(
                    "Plugin `{}` declares {} functions but a null table.",
                    path.display(),
                    desc.functions_len
                );
            }
            // SAFETY: non-null table of `functions_len` descriptors, valid for
            // the library's lifetime (ABI contract).
            unsafe { slice::from_raw_parts(desc.functions, desc.functions_len) }
        };

        functions
            .iter()
            .map(|function| {
                // SAFETY: a descriptor string, valid for the library's lifetime
                // (ABI contract); `'d` comes from the `&'d ModuleDesc` and the
                // library outlives it.
                let decoded = unsafe { read_ffi_name(function.name) };
                let Some(name) = decoded else {
                    import_error!(
                        "Plugin `{}` exports a function with an invalid name.",
                        path.display()
                    );
                };
                if function.arities.is_null() || function.arities_len == 0 {
                    import_error!(
                        "Plugin function `{name}` in `{}` declares no arities.",
                        path.display()
                    );
                }
                // SAFETY: non-null arity array of `arities_len` bytes (ABI contract).
                let arities =
                    unsafe { slice::from_raw_parts(function.arities, function.arities_len) };
                let Some(fun) = function.fun else {
                    import_error!(
                        "Plugin function `{name}` in `{}` has a null function pointer.",
                        path.display()
                    );
                };
                Ok((name, arities, fun))
            })
            .collect()
    }

    /// Validate the descriptor's value table without interning: for each
    /// module value, the borrowed name and creator. `Err` is an
    /// already-thrown `ImportError`.
    #[allow(unsafe_code)]
    fn validate_plugin_values<'d>(
        &mut self,
        path: &Path,
        desc: &'d ModuleDesc,
    ) -> Result<Vec<ValidatedValue<'d>>, VmResult> {
        macro_rules! import_error {
            ($($arg:tt)*) => { return Err(self.throw(ImportError, &format!($($arg)*))) };
        }

        let values = if desc.values_len == 0 {
            &[]
        } else {
            if desc.values.is_null() {
                import_error!(
                    "Plugin `{}` declares {} values but a null table.",
                    path.display(),
                    desc.values_len
                );
            }
            // SAFETY: non-null table of `values_len` descriptors, valid for
            // the library's lifetime (ABI contract).
            unsafe { slice::from_raw_parts(desc.values, desc.values_len) }
        };

        values
            .iter()
            .map(|value| {
                // SAFETY: a descriptor string, as in `validate_plugin_functions`.
                let decoded = unsafe { read_ffi_name(value.name) };
                let Some(name) = decoded else {
                    import_error!(
                        "Plugin `{}` exports a value with an invalid name.",
                        path.display()
                    );
                };
                let Some(fun) = value.fun else {
                    import_error!(
                        "Plugin value `{name}` in `{}` has a null creator pointer.",
                        path.display()
                    );
                };
                Ok((name, fun))
            })
            .collect()
    }

    /// Validate the descriptor's class table without interning: for each class,
    /// the borrowed name, drop/traverse fns, and borrowed method table (name,
    /// arity slice excluding the receiver, pointer). `Err` is an already-thrown
    /// `ImportError`.
    #[allow(unsafe_code)]
    fn validate_plugin_classes<'d>(
        &mut self,
        path: &Path,
        desc: &'d ModuleDesc,
    ) -> Result<Vec<ValidatedClass<'d>>, VmResult> {
        macro_rules! import_error {
            ($($arg:tt)*) => { return Err(self.throw(ImportError, &format!($($arg)*))) };
        }

        let classes = if desc.classes_len == 0 {
            &[]
        } else {
            if desc.classes.is_null() {
                import_error!(
                    "Plugin `{}` declares {} classes but a null table.",
                    path.display(),
                    desc.classes_len
                );
            }
            // SAFETY: non-null table of `classes_len` descriptors, valid for
            // the library's lifetime (ABI contract).
            unsafe { slice::from_raw_parts(desc.classes, desc.classes_len) }
        };

        classes
            .iter()
            .map(|class| {
                // SAFETY: a descriptor string, as in `validate_plugin_functions`.
                let decoded = unsafe { read_ffi_name(class.name) };
                let Some(name) = decoded else {
                    import_error!("Plugin `{}` exports a class with an invalid name.", path.display());
                };
                if class.methods.is_null() && class.methods_len != 0 {
                    import_error!(
                        "Plugin class `{name}` in `{}` declares {} methods but a null table.",
                        path.display(),
                        class.methods_len
                    );
                }
                let methods = if class.methods_len == 0 {
                    &[]
                } else {
                    // SAFETY: non-null table of `methods_len` descriptors (ABI contract).
                    unsafe { slice::from_raw_parts(class.methods, class.methods_len) }
                };
                let validated_methods = methods
                    .iter()
                    .map(|method| {
                        // SAFETY: a descriptor string, as above.
                        let decoded = unsafe { read_ffi_name(method.name) };
                        let Some(mname) = decoded else {
                            import_error!(
                                "Plugin class `{name}` in `{}` has a method with an invalid name.",
                                path.display()
                            );
                        };
                        if method.arities.is_null() || method.arities_len == 0 {
                            import_error!(
                                "Plugin method `{mname}` of class `{name}` in `{}` declares no arities.",
                                path.display()
                            );
                        }
                        // SAFETY: non-null arity array of `arities_len` bytes.
                        let arities = unsafe {
                            slice::from_raw_parts(method.arities, method.arities_len)
                        };
                        let Some(fun) = method.fun else {
                            import_error!(
                                "Plugin method `{mname}` of class `{name}` in `{}` has a null function pointer.",
                                path.display()
                            );
                        };
                        Ok((mname, arities, fun))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok((name, class.drop, class.traverse, validated_methods))
            })
            .collect()
    }

    /// Build each export's trampoline `NativeFunction` and hand them to
    /// `install_native_module`, which registers the module honoring
    /// `from`-imports, aliases, and local imports.
    ///
    /// # Safety
    ///
    /// Every pointer in `exports` must come from a validated descriptor of a
    /// loaded library - the value creators run here, and the function, class,
    /// and method pointers are stored for later dispatch and collection.
    #[allow(unsafe_code)]
    pub(super) unsafe fn import_plugin_module(
        &mut self,
        name_id: StringId,
        path: PathBuf,
        alias: Option<StringId>,
        exports: &PluginModuleExports,
        names_to_import: Option<&[StringId]>,
        local_import: bool,
    ) -> VmResult {
        let mut natives: Vec<(StringId, Value)> = exports
            .functions
            .iter()
            .map(|(fn_name_id, arity, fun)| {
                (
                    *fn_name_id,
                    // SAFETY: validated exports, per this function's contract.
                    unsafe { self.add_plugin_native(*fn_name_id, arity, *fun) },
                )
            })
            .collect();
        for (class_name_id, drop, traverse, methods) in &exports.classes {
            // SAFETY: validated exports, per this function's contract.
            let class_value =
                unsafe { self.add_plugin_class(*class_name_id, *drop, *traverse, methods) };
            natives.push((*class_name_id, class_value));
        }
        // Module values are built by the plugin at import time. The
        // creators may re-enter and collect, so everything built so far
        // is rooted on the VM stack while they run: the function and
        // class exports above, and each finished value while the
        // remaining creators execute. The whole batch is unrooted after
        // the module took ownership.
        let natives_base = self.stack.len();
        for (_, value) in &natives {
            self.stack.push(*value);
        }
        let values_base = self.stack.len();
        for (value_name_id, fun) in &exports.values {
            // SAFETY: validated exports, per this function's contract.
            match unsafe { call_plugin_value(self, *fun, *value_name_id) } {
                Ok(value) => self.stack.push(value),
                Err(error) => {
                    // Drop the partial batch from under a pending exception.
                    let created = self.stack.len()
                        - values_base
                        - usize::from(matches!(error, VmErrorKind::Exception(_)));
                    self.stack.drain(natives_base..values_base + created);
                    return Err(error);
                }
            }
        }
        for (offset, (value_name_id, _)) in exports.values.iter().enumerate() {
            natives.push((*value_name_id, self.stack[values_base + offset]));
        }
        let result = self.install_native_module(
            name_id,
            path,
            alias,
            natives,
            names_to_import,
            local_import,
        );
        // The module owns the batch now; drop it from under whatever
        // `install_native_module` left on top (locally imported values,
        // or a pending exception from a failed `from`-import).
        self.stack
            .drain(natives_base..values_base + exports.values.len());
        result
    }

    /// Register a plugin function as a heap native: the shared
    /// [`plugin_trampoline`] dispatches every call, and the real `extern "C"`
    /// pointer rides in `plugin_fn`.
    ///
    /// # Safety
    ///
    /// `fun` must be a real plugin function from a loaded library, since the
    /// stored pointer is called on every later dispatch.
    #[allow(unsafe_code)]
    pub(super) unsafe fn add_plugin_native(
        &mut self,
        name: StringId,
        arity: &'static [u8],
        fun: PluginFn,
    ) -> Value {
        self.heap.add_native_function(NativeFunction {
            name,
            arity,
            fun: plugin_trampoline,
            plugin_fn: Some(fun),
        })
    }
}

/// Read an `FfiStr` from a descriptor as a borrowed `&str`, or `None` on a null
/// pointer or invalid UTF-8.
///
/// # Safety
///
/// Unless `s.ptr` is null, it must point to `s.len` initialized bytes that stay
/// valid and unmutated for `'d` - which the caller must not choose freely: the
/// ABI guarantees descriptor strings only for the lifetime of the loaded
/// library. Callers pass a lifetime borrowed from the `&ModuleDesc`, which the
/// library outlives (it is never unloaded). Null is the only part checkable
/// here; a garbage pointer or an over-long length reads out of bounds.
#[allow(unsafe_code)]
unsafe fn read_ffi_name<'d>(s: FfiStr) -> Option<&'d str> {
    if s.ptr.is_null() {
        return None;
    }
    // SAFETY: non-null (just checked), and the caller guarantees `len`
    // readable bytes for `'d`.
    let bytes = unsafe { slice::from_raw_parts(s.ptr, s.len) };
    str::from_utf8(bytes).ok()
}

/// Leak an arity slice as `&'static [u8]`. Bounded: one per exported
/// function/method at load, and plugins never unload.
fn leak_arities(arities: &[u8]) -> &'static [u8] {
    Box::leak(arities.to_vec().into_boxed_slice())
}

/// Sentinel `NativeMethodImpl` for plugin methods: never runs, because
/// `execute_native_method_call` checks `plugin_fn` first and delegates to
/// `call_plugin_method`.
fn plugin_method_sentinel(_vm: &mut VM, _receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    unreachable!(
        "plugin_method_sentinel ran; execute_native_method_call should have delegated to call_plugin_method"
    )
}

/// The first existing plugin candidate for an import path:
/// `<dir>/{<name>,lib<name>}.<platform dylib extension>`.
pub(super) fn find_plugin_candidate(file_path: &Path, name: &str) -> Option<PathBuf> {
    let dir = file_path.parent()?;
    let unprefixed = dir.join(format!("{name}{DLL_SUFFIX}"));
    if unprefixed.is_file() {
        return Some(unprefixed);
    }
    if !DLL_PREFIX.is_empty() {
        let prefixed = dir.join(format!("{DLL_PREFIX}{name}{DLL_SUFFIX}"));
        if prefixed.is_file() {
            return Some(prefixed);
        }
    }
    None
}
