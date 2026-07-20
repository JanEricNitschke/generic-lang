//! Plugin discovery and loading: import fallback arm 2.
//!
//! Import resolution tries `<dir>/{<name>,lib<name>}.<dylib-ext>` next to
//! the resolved import path, directly after the user-file arm — so a plugin
//! deliberately shadows stdlib modules of the same name. A loaded library
//! lives as long as the VM (its heap `NativeFunction`s hold `extern "C"`
//! pointers into it), and re-importing the same path rebuilds the module
//! from the cached exports instead of re-loading the library.

use std::env::consts::{DLL_PREFIX, DLL_SUFFIX};
use std::path::{Path, PathBuf};

use libloading::Library;
use rustc_hash::FxHashMap as HashMap;

use generic_lang_api::{GENERIC_PLUGIN_ABI_VERSION, ModuleDesc, PluginFn};

use super::trampolines::plugin_trampoline;
use crate::heap::StringId;
use crate::value::{NativeFunction, Value};
use crate::vm::ExceptionKind::ImportError;
use crate::vm::VM;
use crate::vm::errors::VmResult;

/// One export of a loaded plugin: name, arity slice (leaked once at load —
/// bounded, plugins never unload), and the `extern "C"` function pointer.
type PluginExport = (StringId, &'static [u8], PluginFn);

/// Per-VM plugin state: the loaded libraries (kept alive for the VM's
/// lifetime) and the per-path export cache.
#[derive(Default)]
pub struct PluginState {
    /// Never dropped while the VM lives — unloading would dangle the
    /// `plugin_fn` pointers held by heap natives and the leaked descriptor
    /// memory.
    libraries: Vec<Library>,
    /// Re-imports of the same dylib rebuild the module from here instead of
    /// re-loading the library. Keyed by the canonicalized plugin path so
    /// every spelling of the same file shares one entry (and one loaded
    /// library). The cached name `StringId`s are GC roots, marked inline by
    /// `collect_garbage` like every other root category.
    pub(in crate::vm) loaded: HashMap<PathBuf, Vec<PluginExport>>,
}

impl VM {
    /// Import fallback arm 2: load `name` as a native plugin if a shared
    /// library exists next to the resolved import path.
    ///
    /// Returns `None` if no plugin candidate file exists (the chain falls
    /// through to the stdlib arms); `Some(result)` if one does — where
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
        // disk, so this only fails on races — fall back to the raw path.
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

    /// Load the shared library and validate its descriptor. On success the
    /// library is retained and the exports cached under `path`.
    ///
    /// `Err` carries the already-thrown `ImportError`.
    // The one place in the loader that touches the FFI: dlopen, symbol
    // resolution, and reading the plugin-provided descriptor tables.
    #[allow(unsafe_code)]
    fn load_plugin_library(&mut self, path: &Path) -> Result<Vec<PluginExport>, VmResult> {
        macro_rules! import_error {
            ($($arg:tt)*) => {
                return Err(self.throw(ImportError, &format!($($arg)*)))
            };
        }

        // SAFETY: loading a shared library runs its initializers — this is
        // the trust boundary of the plugin system (plugins are trusted
        // native code).
        let library = match unsafe { Library::new(path) } {
            Ok(library) => library,
            Err(error) => import_error!("Failed to load plugin `{}`: {error}", path.display()),
        };

        // SAFETY: the symbol is declared with exactly this signature in the
        // plugin ABI (`generic_plugin_init` in generic.h).
        let init = match unsafe {
            library.get::<extern "C" fn() -> *const ModuleDesc>(b"generic_plugin_init")
        } {
            Ok(symbol) => symbol,
            Err(error) => import_error!(
                "Plugin `{}` does not export `generic_plugin_init`: {error}",
                path.display()
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
            // SAFETY: non-null table of `functions_len` descriptors, valid
            // for the library's lifetime (ABI contract).
            unsafe { std::slice::from_raw_parts(desc.functions, desc.functions_len) }
        };

        let mut exports = Vec::with_capacity(functions.len());
        for function in functions {
            let name = if function.name.ptr.is_null() {
                None
            } else {
                // SAFETY: non-null function name of `len` bytes, valid for
                // the library's lifetime (ABI contract).
                let bytes =
                    unsafe { std::slice::from_raw_parts(function.name.ptr, function.name.len) };
                std::str::from_utf8(bytes).ok()
            };
            let Some(name) = name else {
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

            // SAFETY: non-null arity array of `arities_len` bytes (ABI
            // contract); copied out immediately.
            let arities =
                unsafe { std::slice::from_raw_parts(function.arities, function.arities_len) };
            // Leaked once per function at load time — bounded, since
            // plugins are cached per path and never unloaded.
            let arities: &'static [u8] = Box::leak(arities.to_vec().into_boxed_slice());

            let name_id = self.heap.string_id(&name);
            exports.push((name_id, arities, function.fun));
        }

        self.plugins.libraries.push(library);
        self.plugins
            .loaded
            .insert(path.to_path_buf(), exports.clone());
        Ok(exports)
    }

    /// Build each export's trampoline `NativeFunction` and hand them to
    /// `install_native_module`, which registers the module honoring
    /// `from`-imports, aliases, and local imports.
    fn import_plugin_module(
        &mut self,
        name_id: StringId,
        path: PathBuf,
        alias: Option<StringId>,
        exports: &[PluginExport],
        names_to_import: Option<&[StringId]>,
        local_import: bool,
    ) -> VmResult {
        let natives = exports
            .iter()
            .map(|(fn_name_id, arity, fun)| {
                (
                    *fn_name_id,
                    self.add_plugin_native(*fn_name_id, arity, *fun),
                )
            })
            .collect();
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
