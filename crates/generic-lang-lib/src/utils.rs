use std::path::Path;

pub(crate) fn get_file_stem(path: &str) -> Option<&str> {
    Path::new(path).file_stem()?.to_str()
}

pub(crate) fn repeat_string(value: &str, count: usize, max_len: usize) -> Option<String> {
    let len = value.len().checked_mul(count)?;
    if len > max_len {
        return None;
    }
    if len == 0 {
        return Some(String::new());
    }

    // Reserve the full result up front so an absurd count cannot abort the
    // process in the allocator; callers turn `None` into a language error.
    let mut repeated = String::new();
    repeated.try_reserve_exact(len).ok()?;
    repeated.extend(std::iter::repeat_n(value, count));
    Some(repeated)
}
