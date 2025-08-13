use thiserror::Error;

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

pub type RuntimeError<T = ()> = Result<T, RuntimeErrorKind>;
pub type VmError<T = ()> = Result<T, VmErrorKind>;

pub enum Return {
    Function,
    Program,
}
