use std::path::Path;

pub fn get_file_stem(path: &str) -> Option<&str> {
    Path::new(path).file_stem()?.to_str()
}
