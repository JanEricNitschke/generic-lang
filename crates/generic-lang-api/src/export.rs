//! The `export_module!` macro generating a plugin's `extern "C"` glue.

/// Export the plugin's functions to the generic interpreter.
///
/// Takes `(name, arities, function)` triples, where `arities` is a
/// `&'static [u8]` of accepted argument counts and `function` is a
/// [`RustPluginFn`](crate::RustPluginFn):
///
/// ```ignore
/// use generic_lang_api::{GenericValue, Host, PluginError};
///
/// fn add(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
///     let (Some(a), Some(b)) = (host.as_int(args[0]), host.as_int(args[1])) else {
///         return Err(host.type_error("add expects two integers"));
///     };
///     Ok(host.make_int(a + b))
/// }
///
/// generic_lang_api::export_module![("add", &[2], add)];
/// ```
///
/// Expands to static descriptor tables (the same shape a C plugin declares
/// by hand) and the one symbol every plugin must export,
/// `generic_plugin_init`, plus a panic-safe `extern "C"` wrapper per
/// function (a panicking plugin function becomes a catchable generic
/// exception instead of aborting the whole interpreter process, which is
/// what an unwind reaching an `extern "C"` boundary does).
#[macro_export]
macro_rules! export_module {
    [$(($name:expr, $arities:expr, $fun:expr)),* $(,)?] => {
        /// Entry point resolved by the generic interpreter's plugin loader.
        #[unsafe(no_mangle)]
        pub extern "C" fn generic_plugin_init() -> *const $crate::ModuleDesc {
            const FUNCTIONS: &[$crate::FunctionDesc] = &[
                $(
                    {
                        extern "C" fn wrapper(
                            host: *const $crate::HostApi,
                            args: *const $crate::GenericValue,
                            nargs: usize,
                        ) -> $crate::FfiReturn {
                            // SAFETY: the host passes a valid vtable and
                            // `nargs` contiguous argument values.
                            unsafe { $crate::__invoke_plugin_fn($fun, host, args, nargs) }
                        }
                        const NAME: &str = $name;
                        const ARITIES: &[u8] = $arities;
                        $crate::FunctionDesc {
                            name: $crate::FfiStr {
                                ptr: NAME.as_ptr(),
                                len: NAME.len(),
                            },
                            arities: ARITIES.as_ptr(),
                            arities_len: ARITIES.len(),
                            fun: Some(wrapper),
                        }
                    }
                ),*
            ];
            static DESC: $crate::ModuleDesc = $crate::ModuleDesc {
                abi_version: $crate::GENERIC_PLUGIN_ABI_VERSION,
                functions: FUNCTIONS.as_ptr(),
                functions_len: FUNCTIONS.len(),
            };
            &raw const DESC
        }
    };
}
