#![no_main]

use generic_lang_lib::{RunOptions, run_source};
use libfuzzer_sys::fuzz_target;
use std::path::PathBuf;

const OPTIONS: RunOptions = RunOptions {
    cache_read: false,
    cache_write: false,
    optimize: true,
};

fuzz_target!(|data: &[u8]| {
    let source = String::from_utf8_lossy(data);
    drop(run_source(
        source.as_ref(),
        PathBuf::from("<fuzz>/input.gen"),
        OPTIONS,
    ));
});
