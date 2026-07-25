//! The `export_module!` macro generating a plugin's `extern "C"` glue.

/// Export the plugin's functions and classes to the generic interpreter.
///
/// Takes a comma-separated list of entries, each either:
/// - A function: `(name, arities, function)`, where `arities` is a
///   `&'static [u8]` of accepted argument counts and `function` is a
///   [`RustPluginFn`](crate::RustPluginFn).
/// - A class: `class("Name") { (method, arities, fun), ... }`, optionally
///   followed by `drop: drop_fn,` and/or `traverse: traverse_fn,` (in that
///   order). Method `arities` count `self`; the receiver arrives as `args[0]`.
///
/// ```ignore
/// use generic_lang_api::{GenericValue, Host, PluginError, PluginVisitFn};
///
/// fn add(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
///     let (Some(a), Some(b)) = (host.as_int(args[0]), host.as_int(args[1])) else {
///         return Err(host.type_error("add expects two integers"));
///     };
///     Ok(host.make_int(a + b))
/// }
///
/// struct CounterState { value: i64 }
/// // Methods take the receiver (`self`) as a separate parameter; `args` are the
/// // remaining arguments, and arities exclude the receiver.
/// fn counter_init(host: &mut Host, this: GenericValue, _args: &[GenericValue])
///     -> Result<GenericValue, PluginError> {
///     let ptr = Box::into_raw(Box::new(CounterState { value: 0 })).cast();
///     host.set_opaque(this, ptr)?;
///     Ok(this) // like every __init__, return the receiver
/// }
/// extern "C" fn drop_counter(ptr: *mut core::ffi::c_void) {
///     if !ptr.is_null() { unsafe { drop(Box::from_raw(ptr.cast::<CounterState>())) }; }
/// }
///
/// generic_lang_api::export_module![
///     ("add", &[2], add),
///     class("Counter") {
///         ("__init__", &[0], counter_init), // no extra args beyond the receiver
///         drop: drop_counter,
///     },
/// ];
/// ```
///
/// Expands to static descriptor tables (the same shape a C plugin declares by
/// hand) and the one symbol every plugin must export, `generic_plugin_init`,
/// plus a panic-safe `extern "C"` wrapper per function/method (a panicking
/// plugin call becomes a catchable generic exception instead of aborting the
/// whole interpreter process, which is what an unwind reaching an `extern "C"`
/// boundary does).
#[macro_export]
macro_rules! export_module {
    [$($t:tt)*] => {
        $crate::__export_go!(@go [] [] $($t)*);
    };
}

/// Single tt-muncher behind [`export_module!`]: walks the entry list once,
/// appending each entry to the functions accumulator or the classes
/// accumulator, then emits `generic_plugin_init` with both `const` tables.
#[doc(hidden)]
#[macro_export]
macro_rules! __export_go {
    (@go [$($fa:tt)*] [$($ca:tt)*]) => {
        /// Entry point resolved by the generic interpreter's plugin loader.
        #[unsafe(no_mangle)]
        pub extern "C" fn generic_plugin_init() -> *const $crate::ModuleDesc {
            const FUNCTIONS: &[$crate::FunctionDesc] = &[ $($fa)* ];
            const CLASSES: &[$crate::ClassDesc] = &[ $($ca)* ];
            static DESC: $crate::ModuleDesc = $crate::ModuleDesc {
                abi_version: $crate::GENERIC_PLUGIN_ABI_VERSION,
                functions: FUNCTIONS.as_ptr(),
                functions_len: FUNCTIONS.len(),
                classes: CLASSES.as_ptr(),
                classes_len: CLASSES.len(),
            };
            &raw const DESC
        }
    };
    (@go [$($fa:tt)*] [$($ca:tt)*] ($n:expr, $a:expr, $f:expr) $(, $($r:tt)*)?) => {
        $crate::__export_go!(
            @go [$($fa)* $crate::__function_desc!($n, $a, $f),] [$($ca)*] $($($r)*)?
        );
    };
    (@go [$($fa:tt)*] [$($ca:tt)*] class($cn:expr) { $($b:tt)* } $(, $($r:tt)*)?) => {
        $crate::__export_go!(
            @go [$($fa)*] [$($ca)* $crate::__class_desc!($cn, { $($b)* }),] $($($r)*)?
        );
    };
}

/// `__opt!()` -> `None`; `__opt!(expr)` -> `Some(expr)` (const-friendly, for
/// the optional `drop:` / `traverse:` fields).
#[doc(hidden)]
#[macro_export]
macro_rules! __opt {
    () => {
        None
    };
    ($e:expr) => {
        Some($e)
    };
}

/// Build one [`FunctionDesc`](crate::FunctionDesc) with its panic-safe wrapper.
#[doc(hidden)]
#[macro_export]
macro_rules! __function_desc {
    ($name:expr, $arities:expr, $fun:expr) => {{
        extern "C" fn wrapper(
            host: *const $crate::HostApi,
            args: *const $crate::GenericValue,
            nargs: usize,
        ) -> $crate::FfiReturn {
            // SAFETY: the host passes a valid vtable and `nargs` contiguous
            // argument values.
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
    }};
}

/// Build one [`MethodDesc`](crate::MethodDesc). The wrapper takes the receiver
/// as a separate value and passes it through; `arities` exclude the receiver.
#[doc(hidden)]
#[macro_export]
macro_rules! __method_desc {
    ($name:expr, $arities:expr, $fun:expr) => {{
        extern "C" fn wrapper(
            host: *const $crate::HostApi,
            receiver: $crate::GenericValue,
            args: *const $crate::GenericValue,
            nargs: usize,
        ) -> $crate::FfiReturn {
            // SAFETY: as in `__function_desc!`.
            unsafe { $crate::__invoke_plugin_method_fn($fun, host, receiver, args, nargs) }
        }
        const NAME: &str = $name;
        const ARITIES: &[u8] = $arities;
        $crate::MethodDesc {
            name: $crate::FfiStr {
                ptr: NAME.as_ptr(),
                len: NAME.len(),
            },
            arities: ARITIES.as_ptr(),
            arities_len: ARITIES.len(),
            fun: Some(wrapper),
        }
    }};
}

/// Build one [`ClassDesc`](crate::ClassDesc) from a `class(...) { ... }` body:
/// method triples first, then optional `drop:` / `traverse:` (in that order).
#[doc(hidden)]
#[macro_export]
macro_rules! __class_desc {
    ($cname:expr, {
        $( ($mn:expr, $ma:expr, $mf:expr) ),* $(,)?
        $(drop: $drop:expr,)? $(traverse: $trav:expr,)?
    }) => {{
        const NAME: &str = $cname;
        const METHODS: &[$crate::MethodDesc] =
            &[ $( $crate::__method_desc!($mn, $ma, $mf) ),* ];
        $crate::ClassDesc {
            name: $crate::FfiStr { ptr: NAME.as_ptr(), len: NAME.len() },
            methods: METHODS.as_ptr(),
            methods_len: METHODS.len(),
            drop: $crate::__opt!($($drop)?),
            traverse: $crate::__opt!($($trav)?),
        }
    }};
}
