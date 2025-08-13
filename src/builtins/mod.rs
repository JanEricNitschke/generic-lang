//! Module for registering and executing generic builtins.
//!
//! Builtins are always executed at startup and their globals are injected
//! into the user's program before execution starts.

use crate::vm::VM;
use std::path::PathBuf;

pub fn define(vm: &mut VM) {
    // Get path to builtins directory
    let mut builtins_path = PathBuf::from(file!());
    builtins_path.pop(); // Remove mod.rs
    
    // Collect all builtin source code
    let mut combined_source = String::new();
    
    // Load and combine all builtin files
    if let Ok(entries) = std::fs::read_dir(&builtins_path) {
        for entry in entries.flatten() {
            if let Some(file_name) = entry.file_name().to_str() {
                if file_name.ends_with(".gen") {
                    let file_path = entry.path();
                    if let Ok(contents) = std::fs::read_to_string(&file_path) {
                        combined_source.push_str(&contents);
                        combined_source.push('\n');
                    }
                }
            }
        }
    }
    
    // Execute the combined builtins source if we have any
    if !combined_source.is_empty() {
        vm.execute_builtins_source(combined_source.as_bytes());
    }
}