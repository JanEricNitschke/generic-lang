#![no_main]

use generic_lang_lib::{FormatOptions, format_source};
use libfuzzer_sys::fuzz_target;

const NARROW: FormatOptions = FormatOptions {
    indent_width: 2,
    max_line_length: Some(40),
};

const CRAMPED: FormatOptions = FormatOptions {
    indent_width: 8,
    max_line_length: Some(1),
};

fuzz_target!(|data: &[u8]| {
    let source = String::from_utf8_lossy(data);
    let source = source.as_ref();
    drop(format_source(source, FormatOptions::default()));
    drop(format_source(source, NARROW));
    drop(format_source(source, CRAMPED));
});
