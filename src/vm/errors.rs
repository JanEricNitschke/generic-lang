//! Error types for VM execution using thiserror.

use thiserror::Error;

/// A runtime error that should terminate the program.
#[derive(Error, Debug)]
#[error("Runtime error: {message}")]
pub struct RuntimeError {
    pub message: String,
}

/// An exception was raised and should be handled by exception handlers.
#[derive(Error, Debug)]
#[error("Exception raised")]
pub struct ExceptionRaisedError;

/// Combined error type for VM operations that can fail with either runtime errors or raised exceptions.
#[derive(Error, Debug)]
pub enum VmError {
    /// A runtime error that should terminate the program.
    #[error(transparent)]
    RuntimeError(#[from] RuntimeError),

    /// An exception was raised and should be handled by exception handlers.
    #[error(transparent)]
    ExceptionRaised(#[from] ExceptionRaisedError),
}

impl RuntimeError {
    /// Create a new runtime error.
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl ExceptionRaisedError {
    /// Create a new exception raised error.
    pub fn new() -> Self {
        Self
    }
}
