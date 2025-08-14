/// Report runtime errors with the correct line number and function name.
///
/// Macro for borrow checking reasons.
macro_rules! runtime_error {
    ($self:ident, $($arg:expr),* $(,)?) => {
        eprintln!($($arg),*);
        for frame in $self.callstack.iter().rev() {
            let line = frame.closure(&$self.heap).function.to_value(&$self.heap).chunk.get_line(CodeOffset(frame.ip - 1));
            eprintln!("[line {}] in {}", *line, frame.closure(&$self.heap).function.to_value(&$self.heap).name.to_value(&$self.heap));
        }
    };
}

/// Report runtime errors and return a `VmError::RuntimeError`.
///
/// This version can be used with the ? operator to propagate errors.
#[allow(unused_macros)]
macro_rules! runtime_error_result {
    ($self:ident, $($arg:expr),* $(,)?) => {{
        runtime_error!($self, $($arg),*);
        Err($crate::vm::VmError::RuntimeError)
    }};
}

/// Helper macro to convert legacy Option<InterpretResult> returns to early returns
/// in a `VmResult` context. This allows gradual migration.
macro_rules! try_interpret_result {
    ($self:ident, $expr:expr) => {
        if let Some(result) = $expr {
            match result {
                InterpretResult::Ok => return Ok(()),
                InterpretResult::RuntimeError => return Err($crate::vm::VmError::RuntimeError),
                InterpretResult::CompileError => return Err($crate::vm::VmError::RuntimeError), // Should not happen in runtime
            }
        }
    };
}
