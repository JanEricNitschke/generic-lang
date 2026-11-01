#![no_main]

use generic_lang_lib::check_unparse_roundtrip;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let source = String::from_utf8_lossy(data);
    check_unparse_roundtrip(source.as_ref());
});
