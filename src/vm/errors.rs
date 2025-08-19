use thiserror::Error;

// === Raw error types ===
#[derive(Debug, Error)]
#[error("Runtime error occurred")]
#[allow(dead_code)]
pub struct RuntimeErrorKind;

#[derive(Debug, Error)]
#[error("Exception was raised")]
#[allow(dead_code)]
pub struct ExceptionRaisedKind;

#[derive(Debug, Error)]
#[allow(dead_code)]
pub enum VmErrorKind {
    #[error(transparent)]
    Runtime(#[from] RuntimeErrorKind),

    #[error(transparent)]
    Exception(#[from] ExceptionRaisedKind),
}

#[allow(dead_code)]
pub type RuntimeError<T = ()> = Result<T, RuntimeErrorKind>;
#[allow(dead_code)]
pub type ExceptionRaised<T = ()> = Result<T, ExceptionRaisedKind>;
#[allow(dead_code)]
pub type VmError<T = ()> = Result<T, VmErrorKind>;
