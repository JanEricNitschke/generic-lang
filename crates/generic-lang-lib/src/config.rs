//! Collection of global config variables.

use include_dir::{Dir, include_dir};

/// Maximum number of frames that can be active simultaneously.
pub const FRAMES_MAX: usize = 64;
/// Maximum size of the total stack.
pub const STACK_MAX: usize = FRAMES_MAX * 256;
/// Maximum depth of nested native re-entry (dunder calls such as `__str__`,
/// `__eq__`, `__lt__`, `__hash__`) before a `RecursionError` is raised. Each
/// level is a real host-stack frame (unlike bytecode calls, which run in a
/// flat loop). Every platform gets an 8 MiB main-thread stack (Windows via the
/// `/STACK` link-arg in `.cargo/config.toml`), which holds roughly 1800 such
/// frames in a debug build, so this sits below that with margin. Genuine cycles
/// are elided by `value_to_string` at depth 1 and never approach this; it only
/// bounds pathologically deep-but-finite nesting.
pub const REENTRY_MAX: usize = 1024;
/// Maximum depth to which the infallible, host-recursive `Value::to_string`
/// (used for error messages and debug output) descends before eliding the
/// rest as `...`, so a cyclic or very deep value cannot overflow the host
/// stack while being formatted. Kept small: this only formats values inside
/// error messages, where deep nesting is noise.
pub const REPR_MAX_DEPTH: usize = 10;
/// Garbage collection occurs whenever the heap has reached a certain size.
/// To avoid constant collection at large sizes, the next collection
/// is performed when the heap has grown by a constant factor compared
/// to the last one.
pub const GC_HEAP_GROW_FACTOR: usize = 2;

pub const LAMBDA_NAME: &str = "lambda";

/// Embedded directory containing generic builtin modules.
pub static GENERIC_BUILTINS_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/src/builtins");
/// Embedded directory containing generic stdlib modules.
pub static GENERIC_STDLIB_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/src/stdlib/generic");
