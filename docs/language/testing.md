# Testing

[← Guide index](README.md)

Generic has a built-in test runner. Any function whose name starts with
`test_` is a test; run them with the `--test` flag:

```sh
generic --test my_module.gen    # run one file's tests
generic --test tests/           # recurse a directory (files named `test_*.gen`)
```

Tests run in alphabetical order. The VM's stack, call frames, and module
state are restored between tests, but the tests in a file share one VM:
global variables mutated by one test remain visible to later ones.
Failures and uncaught exceptions are reported per test and never stop the
other tests; the process exits non-zero if any test failed.

Directory mode recurses through subdirectories and picks up only files
whose names **start with `test_`** (`test_math.gen` is discovered,
`math_test.gen` is not). Each file runs as a normal script first — its
top-level code executes — and then its tests run, with a per-file summary
and an overall summary at the end.

```generic
import "testing";

fun test_addition() {
    testing.assert_equal(2 + 3, 5, "addition works");
}

fun test_truth() {
    assert(1 + 1 == 2);           # the builtin assert also works
}

fun helper() { return 1; }        # not a test — name lacks the test_ prefix
```

```text
$ generic --test my_module.gen
Running tests in my_module.gen...

Running 2 test(s)...

  test_addition ... PASS
  test_truth    ... PASS
=== Test Results ===
Tests run: 2
✓ Passed: 2

All tests passed! ✓
```

## Failures and errors

A test that throws — a failed assertion or any other uncaught exception —
is reported as `FAIL`, and the exception's traceback is printed as it
occurs, just above the `FAIL` line. `ERROR` means the function could not be
run as a test at all: **test functions must take no parameters** (a `test_`
function with parameters cannot be invoked by the runner and is reported as
`Failed to call test function`).

```text
$ generic --test math.gen
Running tests in math.gen...

Running 2 test(s)...

  test_addition    ... PASS
AssertionError: subtraction works: expected 1, got 2
Traceback (most recent call last):
  File "math.gen", line 8, in test_subtraction
  ...
  test_subtraction ... FAIL
    Runtime error occurred
=== Test Results ===
Tests run: 2
✓ Passed: 1
⚠ Failed: 1

Some tests failed or had errors.
```

In directory mode each file additionally gets its own summary line, and an
overall summary closes the run:

```text
$ generic --test tests/
Found 2 test file(s) in tests/...

Running tests in tests/test_math.gen...
Running 2 test(s)...
[...]
  2 tests in tests/test_math.gen: 1 passed
    1 failures

Running tests in tests/test_strings.gen...
Running 1 test(s)...

  test_concat ... PASS
  1 tests in tests/test_strings.gen: 1 passed

=== Overall Test Results ===
=== Test Results ===
Tests run: 3
✓ Passed: 2
⚠ Failed: 1

Some tests failed or had errors.
```

## Assertions

Two options:

- The builtin `assert(expr)` raises `AssertionError` when `expr` is falsey.
- The `testing` stdlib module gives richer, self-describing assertions:

| Function | Checks |
|---|---|
| `assert_equal(actual, expected, msg)` | `actual == expected` |
| `assert_not_equal(actual, expected, msg)` | `actual != expected` |
| `assert_true(cond, msg)` / `assert_false(cond, msg)` | truthiness |
| `assert_nil(v, msg)` / `assert_not_nil(v, msg)` | nil-ness |
| `assert_throws(func, ExceptionClass, msg)` | that calling `func` throws that class |

Each raises `AssertionError` with a message describing the mismatch (e.g.
`expected 2, got 1`) on failure, which the runner reports against the failing
test.

```generic
import "testing";

testing.assert_equal(2 + 2, 4, "math works");
testing.assert_not_equal(1, 2, "should differ");
testing.assert_true(5 > 3, "5 is bigger");
testing.assert_nil(nil, "nil is nil");

fun raises_value_error() { throw ValueError("bad input"); }
testing.assert_throws(raises_value_error, ValueError, "should throw");
```

A complete suite exercising every assertion lives at
[`test/testing_stdlib.gen`](../../test/testing_stdlib.gen).

### Custom assertions

Assertion helpers are ordinary functions — throw `AssertionError` yourself
for anything the module does not cover:

```generic
fun assert_greater_than(actual, expected, message) {
    if actual <= expected {
        throw AssertionError(f"${message}: expected ${actual} > ${expected}");
    }
}

assert_greater_than(10, 5, "10 should be greater than 5");
```

## Best practices

1. **Start test functions with `test_`** - Only these will be discovered
2. **Use descriptive names** - `test_calculator_division` vs `test1`
3. **Test one thing per function** - Keep tests focused and small
4. **Use assertion helpers** - Create reusable assertion functions
5. **Handle exceptions properly** - Use try/catch for testing error conditions
6. **Keep tests independent** - Don't rely on execution order or shared state

## Exit codes

| Code | Meaning |
|---|---|
| 0 | every test passed (or none were found) |
| 1 | a test failed or errored, or a test file failed to compile or run |
| 64 | `--test` was given without a file or directory |
| 74 | the given path does not exist |

[← Guide index](README.md)
