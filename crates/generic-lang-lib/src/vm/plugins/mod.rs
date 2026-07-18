//! C-ABI native module plugins: loader, host vtable, and the trampoline.
//!
//! This whole directory only exists with the `plugins` cargo feature (the
//! `mod` declaration in `vm/mod.rs` is gated); without it, the inlined
//! `try_import_plugin` stub in `vm/import.rs` takes over.
//!
//! Values cross the FFI as opaque 32-byte blobs; plugins operate on them
//! exclusively through the host vtable. Plugin functions dispatch through
//! one shared trampoline that reads the real `extern "C"` pointer back off
//! the callee, so `NativeFunctionImpl` and the native dispatch sites stay
//! untouched.
//!
//! The crate denies `unsafe_code` (and forbids it entirely without this
//! feature). The exemptions: file-wide in `host_api` (the FFI boundary
//! itself) and in `tests` (which drives the vtable like a plugin would),
//! one function in `loader` (dlopen + descriptor reads). `trampolines`
//! contains no unsafe at all.

mod host_api;
mod loader;
mod trampolines;

#[cfg(test)]
mod tests;

pub(super) use loader::PluginState;
