//! This module contains functionality for splitting and combining bytes.

/// Split a 4 byte value into its individual bytes.
///
/// More precisely, extract the four bottom bytes from a `usize`.
#[allow(clippy::cast_possible_truncation)]
pub const fn get_4_bytes(v: usize) -> (u8, u8, u8, u8) {
    (
        ((v & 0xff00_0000) >> 24) as u8,
        ((v & 0x00ff_0000) >> 16) as u8,
        ((v & 0x0000_ff00) >> 8) as u8,
        (v & 0x0000_00ff) as u8,
    )
}
