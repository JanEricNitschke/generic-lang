use std::path::Path;

pub fn get_file_stem(path: &[u8]) -> Option<&[u8]> {
    Path::new(std::str::from_utf8(path).ok()?)
        .file_stem()
        .map(std::ffi::OsStr::as_encoded_bytes)
}
