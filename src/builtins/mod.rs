//! Module for registering and executing generic builtins.
//!
//! Builtins are always executed at startup and their globals are injected
//! into the user's program before execution starts.
//!
//! This module is now deprecated in favor of the approach in vm/setup.rs 
//! that properly executes builtins first and captures their globals.