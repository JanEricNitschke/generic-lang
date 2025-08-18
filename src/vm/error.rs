/// Runtime error type for the virtual machine
///
/// This unified error type provides consistent error handling throughout the VM.
#[derive(thiserror::Error, Debug)]
pub enum RuntimeError {
    #[error("Runtime error occurred")]
    Runtime,
}

impl RuntimeError {
    /// Create a new runtime error
    pub fn new() -> Self {
        Self::Runtime
    }
}
