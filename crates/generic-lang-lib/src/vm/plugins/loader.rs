//! Plugin discovery and loading: import fallback arm 2.
//!
//! Import resolution tries `<dir>/{<name>,lib<name>}.<dylib-ext>` next to
//! the resolved import path, directly after the user-file arm - so a plugin
//! deliberately shadows stdlib modules of the same name. A loaded library
//! lives as long as the VM (its heap `NativeFunction`s hold `extern "C"`
//! pointers into it), and re-importing the same path rebuilds the module
//! from the cached exports instead of re-loading the library.

use std::env::consts::{DLL_PREFIX, DLL_SUFFIX};
use std::path::{Path, PathBuf};

use libloading::Library;
use rustc_hash::FxHashMap as HashMap;

use generic_lang_api::{
    GENERIC_PLUGIN_ABI_VERSION, ModuleDesc, PluginFn, PluginMethodFn, PluginTraverseFn,
};

use super::trampolines::plugin_trampoline;
use crate::heap::StringId;
use crate::value::{Class, ClassKind, NativeFunction, NativeMethod, PluginClassInfo, Value};
use crate::vm::ExceptionKind::ImportError;
use crate::vm::VM;
use crate::vm::errors::VmResult;

/// One exported function of a loaded plugin: name, arity slice (leaked once at
/// load - bounded, plugins never unload), and the `extern "C"` pointer.
type PluginFunctionExport = (StringId, &'static [u8], PluginFn);

/// A plugin class's drop callback (frees the opaque state).
type PluginDropFn = extern "C" fn(*mut core::ffi::c_void);

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

/// Everything cached per loaded plugin path: functions and classes.
#[derive(Default, Clone)]
pub struct PluginModuleExports {
    pub(in crate::vm) functions: Vec<PluginFunctionExport>,
    pub(in crate::vm) classes: Vec<PluginClassExport>,
}

/// Per-VM plugin state: the loaded libraries (kept alive for the VM's
/// lifetime) and the per-path export cache.
#[derive(Default)]
pub struct PluginState {
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

        // Failed loads are deliberately not cached.
        let exports = if let Some(exports) = self.plugins.loaded.get(&path) {
            exports.clone()
        } else {
            match self.load_plugin_library(&path) {
                Ok(exports) => exports,
                Err(result) => return Some(result),
            }
        };

        Some(self.import_plugin_module(
            name_id,
            path,
            alias,
            &exports,
            names_to_import,
            local_import,
        ))
    }

    /// Register a plugin class: create a `Class` of `ClassKind::Plugin`, add its
    /// methods as `NativeMethod`s carrying `plugin_fn`, and return the class as
    /// a `Value::Class`.
    fn add_plugin_class(
        &mut self,
        name: StringId,
        drop: Option<PluginDropFn>,
        traverse: Option<PluginTraverseFn>,
        methods: &[PluginMethodExport],
    ) -> Value {
        let class = Class::new(name, ClassKind::Plugin(PluginClassInfo { drop, traverse }));
        let class_value = self.heap.add_class(class);
        for (method_name, arities, fun) in methods {
            let method_value = self.add_plugin_method(name, *method_name, arities, *fun);
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
    fn add_plugin_method(
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
        // plugin ABI (`generic_plugin_init` in generic.h).
        let init = match unsafe {
            library.get::<extern "C" fn() -> *const ModuleDesc>(b"generic_plugin_init")
        } {
            Ok(symbol) => symbol,
            Err(error) => import_error!(
                "Plugin `{}` does not export `generic_plugin_init`: {}",
                path.display(),
                describe(&error)
            ),
        };

        let desc = init();
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

        let exports = PluginModuleExports {
            functions: function_exports,
            classes: class_exports,
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
            unsafe { std::slice::from_raw_parts(desc.functions, desc.functions_len) }
        };

        functions
            .iter()
            .map(|function| {
                let Some(name) = read_ffi_name(function.name) else {
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
                    unsafe { std::slice::from_raw_parts(function.arities, function.arities_len) };
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
            unsafe { std::slice::from_raw_parts(desc.classes, desc.classes_len) }
        };

        classes
            .iter()
            .map(|class| {
                let Some(name) = read_ffi_name(class.name) else {
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
                    unsafe { std::slice::from_raw_parts(class.methods, class.methods_len) }
                };
                let validated_methods = methods
                    .iter()
                    .map(|method| {
                        let Some(mname) = read_ffi_name(method.name) else {
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
                            std::slice::from_raw_parts(method.arities, method.arities_len)
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
    fn import_plugin_module(
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
                    self.add_plugin_native(*fn_name_id, arity, *fun),
                )
            })
            .collect();
        for (class_name_id, drop, traverse, methods) in &exports.classes {
            let class_value = self.add_plugin_class(*class_name_id, *drop, *traverse, methods);
            natives.push((*class_name_id, class_value));
        }
        self.install_native_module(name_id, path, alias, natives, names_to_import, local_import)
    }

    /// Register a plugin function as a heap native: the shared
    /// [`plugin_trampoline`] dispatches every call, and the real `extern "C"`
    /// pointer rides in `plugin_fn`.
    pub(super) fn add_plugin_native(
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
/// pointer or invalid UTF-8. The borrow is valid for the library's lifetime.
///
/// # Safety-ish
///
/// Reads `s.len` bytes at `s.ptr`; sound under the ABI contract (descriptor
/// strings are valid for the library's lifetime).
#[allow(unsafe_code)]
fn read_ffi_name<'d>(s: generic_lang_api::FfiStr) -> Option<&'d str> {
    if s.ptr.is_null() {
        return None;
    }
    // SAFETY: non-null `FfiStr` of `len` bytes, valid for the library's
    // lifetime (ABI contract).
    let bytes = unsafe { std::slice::from_raw_parts(s.ptr, s.len) };
    std::str::from_utf8(bytes).ok()
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
