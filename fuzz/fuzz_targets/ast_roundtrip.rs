#![no_main]

use generic_lang_lib::check_ast_roundtrip;
use libfuzzer_sys::fuzz_target;

const OPTIMIZE: [bool; 2] = [false, true];

fuzz_target!(|data: &[u8]| {
    let source = String::from_utf8_lossy(data);
    for optimize in OPTIMIZE {
        check_ast_roundtrip(source.as_ref(), optimize);
    }
});
