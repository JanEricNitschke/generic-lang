/// Report runtime errors with the correct line number and function name.
///
/// Macro for borrow checking reasons.
macro_rules! runtime_error {
    ($($arg:expr),* $(,)?) => {
        eprintln!($($arg),*);
    };
}
