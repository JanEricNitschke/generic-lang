use std::path::Path;

pub(crate) fn get_file_stem(path: &str) -> Option<&str> {
    Path::new(path).file_stem()?.to_str()
}

pub(crate) fn repeat_string(value: &str, count: usize, max_len: usize) -> Option<String> {
    let len = value.len().checked_mul(count)?;
    if len > max_len {
        return None;
    }
    Some(value.repeat(count))
}
