//! Collection of global config variables.

/// Maximum number of frames that can be active simultaneously.
pub const FRAMES_MAX: usize = 64;
/// Maximum size of the total stack.
pub const STACK_MAX: usize = FRAMES_MAX * 256;
/// Garbage collection occurs whenever the heap has reached a certain size.
/// To avoid constant collection at large sizes, the next collection
/// is performed when the heap has grown by a constant factor compared
/// to the last one.
pub const GC_HEAP_GROW_FACTOR: usize = 2;

pub const LAMBDA_NAME: &str = "lambda";
