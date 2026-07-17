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
    /// A hard runtime error: fatal, always propagates.
    #[error(transparent)]
    Runtime(#[from] RuntimeErrorKind),

    /// A pending exception: the exception instance sits on the stack top
    /// (rooted) and no unwinding has happened yet. The dispatch loops
    /// resolve it against the surrounding handlers; a native receiving this
    /// from a VM re-entry may pop the exception and handle it, or propagate
    /// with `?`.
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
