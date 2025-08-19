/// Report runtime errors with the correct line number and function name.
///
/// Macro for borrow checking reasons.
#[allow(unused_macros)]
macro_rules! runtime_error {
    ($self:ident, $($arg:expr),* $(,)?) => {
        eprintln!($($arg),*);
        for frame in $self.callstack.iter().rev() {
            let line = frame.closure(&$self.heap).function.to_value(&$self.heap).chunk.get_line(CodeOffset(frame.ip - 1));
            eprintln!("[line {}] in {}", *line, frame.closure(&$self.heap).function.to_value(&$self.heap).name.to_value(&$self.heap));
        }
    };
}
