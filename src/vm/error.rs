/// Runtime error type for the virtual machine
///
/// This unified error type replaces the previous inconsistent mix of Option<InterpretResult>
/// and other error handling mechanisms throughout the VM.
#[derive(thiserror::Error, Debug)]
pub enum RuntimeError {
    #[error("{message}")]
    Runtime { message: String },
}

impl RuntimeError {
    /// Create a new runtime error with the given message
    pub fn new(message: String) -> Self {
        Self::Runtime { message }
    }
}