# Unit Testing in Generic

The Generic programming language includes built-in support for discovering and running unit tests. This feature allows you to write test functions and run them in isolation with comprehensive reporting.

## Basic Usage

To run tests from a single file, use the `--test` flag with a Generic source file:

```bash
generic --test my_test_file.gen
```

To run tests from all test files in a directory (including subdirectories):

```bash
generic --test test_directory/
```

When using directory mode, Generic will **recursively search through all subdirectories** and find tests from all `.gen` files whose names contain "test" anywhere in the filename (before the file extension). For example: `test_math.gen`, `string_test.gen`, `unittest_validation.gen`, `my_test_suite.gen`, etc.

This allows you to organize your tests in subdirectories while still running them all with a single command.

## Test Discovery

The test runner automatically discovers functions whose names start with `test_`. For example:

```generic
fun test_basic_math() {
    var result = 2 + 3;
    assert(result == 5);  # Use assertion to verify result
}

fun test_string_operations() {
    var greeting = "Hello" + " " + "World";
    assert(greeting == "Hello World");  # Proper test assertion
}

# This function will be ignored (doesn't start with test_)
fun helper_function() {
    return "I'm not a test";
}
```

## Test Execution

- Each test function runs in isolation
- Tests continue running even if one fails
- Exceptions and runtime errors are caught and reported
- VM state is cleaned up between tests
- Tests run in alphabetical order

## Writing Assertions

Generic provides a built-in `assert()` function for basic assertions:

```generic
# Built-in assert function
fun test_basic_assert() {
    assert(2 + 2 == 4);  # Passes
    # assert(false);      # Would throw AssertionError
}
```

For more comprehensive testing, you can import the `testing` standard library module:

```generic
import "testing";

fun test_with_stdlib_assertions() {
    testing.assert_equal(2 + 2, 4, "Basic math should work");
    testing.assert_true(5 > 3, "5 should be greater than 3");
    testing.assert_false(false, "False should be false");
    testing.assert_nil(nil, "Nil should be nil");
    testing.assert_not_nil("hello", "String should not be nil");
    testing.assert_not_equal(1, 2, "1 and 2 should be different");
    fun error_function() { throw Exception("test"); }
    testing.assert_throws(error_function, Exception, "Should throw exception");
}
```

The testing stdlib module provides all the assertion functions you need. If you want to create additional custom assertion functions, you can do so using the built-in `AssertionError` class:

```generic
fun assert_greater_than(actual, expected, message) {
    if (actual <= expected) {
        var error_msg = message + ": expected " + str(actual) + " to be greater than " + str(expected);
        throw AssertionError(error_msg);
    }
}

# Example test using custom assertions
fun test_with_custom_assertions() {
    assert_greater_than(10, 5, "10 should be greater than 5");
}
```

## Test Results

The test runner provides detailed output:

### Single File Output
```
Running tests in test_example.gen...

Main script output here
Running 3 test(s)...

  test_basic_math           ... PASS
  test_string_operations    ... PASS
  test_with_assertions      ... PASS

=== Test Results ===
Tests run: 3
✓ Passed: 3

All tests passed! ✓
```

### Multi-File/Directory Output
```
Found 3 test file(s) in test_suite/...

Running tests in test_suite/math_test.gen...
Running 2 test(s)...

  test_addition             ... PASS
  test_multiplication       ... PASS
  2 tests in test_suite/math_test.gen: 2 passed

Running tests in test_suite/string_test.gen...
Running 3 test(s)...

  test_concatenation        ... PASS
  test_length               ... PASS
  test_substring            ... FAIL
    AssertionError: Expected 'ell', got 'hell'
  3 tests in test_suite/string_test.gen: 2 passed
    1 failed

=== Overall Test Results ===
=== Test Results ===
Tests run: 5
✓ Passed: 4
⚠ Failed: 1

Some tests failed or had errors.
```

For failures and errors:

```
Running 2 test(s)...

  test_that_fails      ... FAIL
    AssertionError: Expected 5, got 4
  test_that_passes     ... PASS

=== Test Results ===
Tests run: 2
✓ Passed: 1
✗ Failed: 1

Some tests failed or had errors.
```

## Exit Codes

- `0`: All tests passed
- `1`: One or more tests failed or had errors
- `64`: Invalid usage (e.g., `--test` without a file)
- `65`: Compile error
- `70`: Runtime error during main script execution
- `74`: File not found

## Best Practices

1. **Start test functions with `test_`** - Only these will be discovered
2. **Use descriptive names** - `test_calculator_division` vs `test1`
3. **Test one thing per function** - Keep tests focused and small
4. **Use assertion helpers** - Create reusable assertion functions
5. **Handle exceptions properly** - Use try/catch for testing error conditions
6. **Keep tests independent** - Don't rely on execution order or shared state

## Example Test Suite

See `test/testing_stdlib.gen` for a complete example showing how to use the testing stdlib module with all its assertion functions.

## Integration with Existing Tests

The unit testing feature complements the existing Dart-based integration test suite:

- **Unit tests** (`--test`): Test individual functions and classes in isolation
- **Integration tests** (`make test`): Test entire programs with expected output

Both testing approaches work together to ensure code quality and correctness.
