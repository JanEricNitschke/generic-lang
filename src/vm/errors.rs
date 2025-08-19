use thiserror::Error;

// === Raw error types ===
#[derive(Debug, Error)]
#[error("Runtime error occurred")]
pub struct RuntimeErrorKind;

#[derive(Debug, Error)]
#[error("Exception was raised")]
pub struct ExceptionRaisedKind;

#[derive(Debug, Error)]
pub enum VmErrorKind {
    #[error(transparent)]
    Runtime(#[from] RuntimeErrorKind),

    #[error(transparent)]
    Exception(#[from] ExceptionRaisedKind),
}

pub type RuntimeError<T = ()> = Result<T, RuntimeErrorKind>;
pub type ExceptionRaised<T = ()> = Result<T, ExceptionRaisedKind>;
pub type VmError<T = ()> = Result<T, VmErrorKind>;
