//! Cargo does not track the directories `include_dir!` embeds, so an
//! edit to an embedded `.gen` file would ship a stale binary;
//! declaring them here makes cargo re-fingerprint their contents on
//! every build.
fn main() {
    println!("cargo:rerun-if-changed=src/builtins");
    println!("cargo:rerun-if-changed=src/stdlib/generic");
}
