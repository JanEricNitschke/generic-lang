use thiserror::Error;

use crate::vm::callstack::CallFrame;

// === Raw error types ===
#[derive(Debug, Error, Clone, Copy)]
#[error("Runtime error occurred")]
pub struct RuntimeErrorKind;

#[derive(Debug, Error, Clone, Copy)]
#[error("Exception was raised")]
pub struct ExceptionRaisedKind;

#[derive(Debug, Error, Clone, Copy)]
pub enum VmErrorKind {
    #[error(transparent)]
    Runtime(#[from] RuntimeErrorKind),

    #[error(transparent)]
    Exception(#[from] ExceptionRaisedKind),
}

pub type RuntimeResult<T = Option<CallFrame>> = Result<T, RuntimeErrorKind>;
pub type VmResult<T = Option<CallFrame>> = Result<T, VmErrorKind>;

impl From<Return> for VmResult {
    fn from(value: Return) -> Self {
        match value {
            Return::Function(frame) | Return::Program(frame) => Ok(Some(frame)),
        }
    }
}

pub enum Return {
    Function(CallFrame),
    Program(CallFrame),
}
