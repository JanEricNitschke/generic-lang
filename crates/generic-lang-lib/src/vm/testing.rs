//! Test execution functionality for the VM.

use super::VM;
use crate::heap::{ClosureId, StringId};
use crate::value::Value;

/// Result of running test functions
#[derive(Default)]
pub struct TestResult {
    pub passed: usize,
    pub failed: usize,
    pub errors: usize,
}

impl TestResult {
    pub fn total(&self) -> usize {
        self.passed + self.failed + self.errors
    }
}

/// Outcome of a single test function execution
pub enum TestOutcome {
    Passed,
    Failed(String),
    Error(String),
}

impl VM {
    /// Discover and run test functions in the current module.
    ///
    /// Test functions are those whose names start with "test_".
    /// Each test function is run in isolation, with exceptions caught
    /// and reported. Returns a summary of test results.
    pub fn run_tests(&mut self) -> TestResult {
        let mut results = TestResult::default();

        // Get all global functions that start with "test_"
        let test_functions = self.discover_test_functions();

        if test_functions.is_empty() {
            return results;
        }

        println!("Running {} test(s)...\n", test_functions.len());

        // Calculate the maximum width needed for test names (+ 5 padding)
        let max_name_width = test_functions
            .iter()
            .map(|(name, _)| name.to_value(&self.heap).len())
            .max()
            .unwrap_or(0);

        // Run each test function
        for (name, closure_id) in test_functions {
            let test_name = name.to_value(&self.heap);
            print!("  {test_name:<max_name_width$} ... ");

            // Run the test in isolation
            match self.run_single_test(closure_id) {
                TestOutcome::Passed => {
                    println!("PASS");
                    results.passed += 1;
                }
                TestOutcome::Failed(msg) => {
                    println!("FAIL");
                    println!("    {msg}");
                    results.failed += 1;
                }
                TestOutcome::Error(msg) => {
                    println!("ERROR");
                    println!("    {msg}");
                    results.errors += 1;
                }
            }
        }

        results
    }

    /// Discover test functions in the current module globals.
    ///
    /// Returns a vector of (name, `closure_id`) pairs for closures
    /// whose names start with "test_". Closures are returned in alphabetical order.
    fn discover_test_functions(&self) -> Vec<(StringId, ClosureId)> {
        let mut v: Vec<_> = self
            .globals()
            .iter()
            .filter_map(|(name_id, value)| match value.value {
                Value::Closure(closure_id) if name_id.to_value(&self.heap).starts_with("test_") => {
                    Some((*name_id, closure_id))
                }
                _ => None,
            })
            .collect();
        v.sort_by_key(|(name_id, _)| name_id.to_value(&self.heap));
        v
    }

    /// Run a single test function in isolation
    fn run_single_test(&mut self, closure_id: ClosureId) -> TestOutcome {
        // Save VM state before running test
        let entry = self.current_region();
        let handlers_before = self.exception_handlers.len();

        // Push the closure onto the stack and call it
        let closure_value = Value::Closure(closure_id);
        self.stack_push(closure_value);

        let result = match self.execute_call(closure_value, 0) {
            Ok(_) => {
                // Run the function
                match self.run() {
                    Ok(_) => TestOutcome::Passed,
                    Err(_) => TestOutcome::Failed("Runtime error occurred".to_string()),
                }
            }
            Err(_) => TestOutcome::Error("Failed to call test function".to_string()),
        };

        // Restore VM state. A failed test may leave its frames behind
        // (uncaught exceptions are reported, not unwound).
        self.unwind_region(entry);
        // A fatal error skips handler resolution, so a `try` the test was
        // sitting in leaves its handler behind, pointing into the frames
        // just unwound — drop it, or a later test's exception could resolve
        // against it.
        self.exception_handlers.truncate(handlers_before);

        result
    }
}
