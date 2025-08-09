# Generic Programming Language

The Generic programming language is a Rust-based interpreter for a Lox-inspired language. It includes a comprehensive test suite, benchmarking system, and supports features like classes, closures, exceptions, and more.

Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.

## CRITICAL: CI Validation Requirements

**⚠️ HIGHEST PRIORITY: No code will be merged if these CI checks fail. Run these IMMEDIATELY after any code changes:**

### Mandatory Formatting and Linting (MUST PASS)
- **Install clippy component**: `rustup component add clippy` -- takes 5 seconds. Set timeout to 60 seconds.
- **Format check**: `cargo fmt --check` -- takes <1 second. MUST PASS or CI fails.
- **Critical linting**: `cargo clippy -- -D warnings` -- takes 8 seconds. Set timeout to 60 seconds. MUST PASS or CI fails.
- **Comprehensive linting**: `cargo clippy -- -W clippy::all -W clippy::pedantic -W clippy::nursery -W clippy::cargo` -- takes 10 seconds. Set timeout to 60 seconds.

### Mandatory Testing (MUST PASS)
- **Rust unit tests**: `cargo test --verbose` -- takes 2 seconds. Set timeout to 60 seconds. MUST PASS or CI fails.
- **Dart integration tests**: `export PATH=/tmp/dart-sdk/bin:$PATH && dart tool/bin/test.dart clox --interpreter ./target/debug/generic` -- takes 30 seconds. NEVER CANCEL. Set timeout to 120+ seconds. MUST PASS or CI fails.
- **Complete test suite**: `export PATH=/tmp/dart-sdk/bin:$PATH && make test` -- takes 15 seconds. NEVER CANCEL. Set timeout to 120+ seconds. MUST PASS or CI fails.

### MIRI Memory Safety Tests (MUST PASS)
- **Install nightly with miri**: `rustup toolchain install nightly --component miri` -- takes 60 seconds. NEVER CANCEL. Set timeout to 120+ seconds.
- **Setup miri**: `cargo +nightly miri setup` -- takes 30 seconds. Set timeout to 60 seconds.
- **Run miri tests**: `cargo +nightly miri test --verbose` -- takes 15 seconds. Set timeout to 60 seconds. MUST PASS or CI fails.
- **Run specific miri test**: `MIRIFLAGS=-Zmiri-disable-isolation cargo +nightly miri run -- test/overloading/overloading_for_in.gen` -- takes 5 seconds. Set timeout to 60 seconds. MUST PASS or CI fails.

### Commit Management
- **ALWAYS squash all commits into a single commit before marking work as done**
- **Final PR must contain exactly ONE commit**
- **How to squash commits:**
  ```bash
  # If you have N commits to squash (e.g., 3 commits)
  git reset --soft HEAD~3
  git commit -m "Descriptive commit message for entire change"
  git push --force-with-lease
  ```
- **Commit message should describe the complete change, not individual steps**

## Working Effectively

### Bootstrap, Build, and Test the Repository
- Install Rust (if not available): `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`
- **Debug build**: `cargo build` -- takes 15 seconds. Set timeout to 60 seconds.
- **Release build**: `cargo build --release` -- takes 75 seconds. NEVER CANCEL. Set timeout to 120+ seconds.
- **Stress GC build**: `cargo build --profile stress_gc --features stress_gc` -- takes 15 seconds. Set timeout to 60 seconds.

### Install Dependencies for Full Testing
- **Install Dart SDK** (required for comprehensive test suite):
  ```bash
  cd /tmp
  wget https://storage.googleapis.com/dart-archive/channels/stable/release/3.6.1/sdk/dartsdk-linux-x64-release.zip
  unzip -q dartsdk-linux-x64-release.zip
  export PATH=/tmp/dart-sdk/bin:$PATH
  dart pub get --directory=tool
  ```
  Setup takes 20 seconds. NEVER CANCEL. Set timeout to 120+ seconds.

- **Install hyperfine** (required for benchmarking):
  ```bash
  cargo install --locked hyperfine
  ```
  Takes 3 minutes. NEVER CANCEL. Set timeout to 300+ seconds.

### Running Tests
- **Rust unit tests**: `cargo test --verbose` -- takes 2 seconds. Set timeout to 60 seconds.
- **Dart integration tests**: `export PATH=/tmp/dart-sdk/bin:$PATH && dart tool/bin/test.dart clox --interpreter ./target/debug/generic` -- takes 30 seconds. NEVER CANCEL. Set timeout to 120+ seconds.
- **Stress GC tests**: `export PATH=/tmp/dart-sdk/bin:$PATH && dart tool/bin/test.dart clox --interpreter ./target/stress_gc/generic` -- takes 30 seconds. NEVER CANCEL. Set timeout to 120+ seconds.
- **Complete test suite**: `export PATH=/tmp/dart-sdk/bin:$PATH && make test` -- takes 15 seconds. NEVER CANCEL. Set timeout to 120+ seconds.

### Running the Interpreter
- **REPL mode**: `./target/debug/generic` or `./target/release/generic`
- **Execute file**: `./target/debug/generic <filename.gen>` or `./target/release/generic <filename.gen>`
- **Help**: `./target/debug/generic --help`
- **Version**: `./target/debug/generic --version`

## Validation

### Critical CI Validation Workflow
**⚠️ ALWAYS run the CI validation commands from the "CRITICAL: CI Validation Requirements" section above IMMEDIATELY after any code changes. These exact commands run in CI and MUST PASS.**

### Additional Manual Validation
- **ALWAYS run through at least one complete end-to-end scenario after making changes.**
- Test basic language functionality:
  ```bash
  echo 'print("Hello Generic!"); print(1 + 2 * 3);' > /tmp/test.gen
  ./target/debug/generic /tmp/test.gen
  ```
  Expected output: `Hello Generic!` followed by `7`
- Test REPL functionality:
  ```bash
  ./target/debug/generic
  # In REPL, type: print("REPL working!");
  # Should output: REPL working!
  # Exit with Ctrl+C
  ```
- Test fibonacci benchmark:
  ```bash
  ./target/release/generic benchmark/fib/fib.gen
  # Should complete without output in ~0.4 seconds
  ```

### Pre-Commit Checklist
**Before marking work as done:**
1. ✅ All CI validation commands from "CRITICAL" section pass
2. ✅ Manual end-to-end testing completed successfully
3. ✅ All commits squashed into single commit
4. ✅ Commit message describes the complete change

### Optional: Benchmarking (NOT required for CI)
- **Simple benchmark**: `hyperfine --warmup 1 "./target/release/generic benchmark/fib/fib.gen"` -- takes 10 seconds.
- **Full benchmark suite**: `make benchmark-ci` (requires reference implementations from craftinginterpreters repo)

**Note: Benchmarks are NOT run as part of standard CI validation and are not required for code to be merged.**

## Common Tasks

### Repository Structure
```
.
├── .github/workflows/build.yaml    # CI/CD pipeline
├── .pre-commit-config.yaml         # Pre-commit hooks configuration
├── Cargo.toml                      # Rust project configuration
├── Makefile                        # Build and test automation
├── README.md                       # Project documentation
├── benchmark/                      # Performance benchmarks
├── src/                           # Rust source code
├── test/                          # Test files (.gen programs)
└── tool/                          # Dart-based test runner
```

### Key Source Files
```
src/
├── main.rs                        # Entry point
├── compiler/                      # Compiler implementation
├── vm/                           # Virtual machine
├── value/                        # Value types and operations
├── heap/                         # Memory management
├── natives/                      # Built-in functions
├── stdlib/                       # Standard library
└── types.rs                      # Core type definitions
```

### Test Structure
- **Unit tests**: Embedded in Rust source files (run with `cargo test`)
- **Integration tests**: `.gen` files in `test/` directory (run with Dart test runner)
- **Benchmarks**: `.gen` files in `benchmark/` directory (run with hyperfine)

### Language Features
The Generic language supports:
- Variables and constants
- Functions and closures
- Classes and inheritance
- Exception handling (try/catch)
- Lists and dictionaries
- Control flow (if/unless, for/foreach/while/until, switch)
- Loop control (break/continue with labels)
- Import system
- Operator overloading
- Native functions (type, print, assert, etc.)

### Common File Extensions
- `.gen` - Generic language source files
- `.rs` - Rust source files
- `.dart` - Dart test runner files

### Debugging Features
Build with debugging features:
- `cargo build --features "print_code"` - Print bytecode
- `cargo build --features "trace_execution_verbose"` - Trace execution
- `cargo build --features "debug_parser"` - Debug parser
- `cargo build --features "log_gc"` - Log garbage collection
- `cargo build --features "stress_gc"` - Stress test garbage collector

### Installation
To install the Generic interpreter globally:
```bash
cargo install --path .
generic --version
```

### Performance Notes
- Debug builds are much faster to compile (~15s vs 75s) but run slower
- Release builds are optimized for runtime performance
- The interpreter can handle recursive fibonacci(30) in ~0.4 seconds in release mode
- All 462 integration tests pass and run in ~30 seconds

### CI/CD Information
The `.github/workflows/build.yaml` pipeline runs these CRITICAL commands that MUST PASS:
- **Formatting**: `cargo fmt --check`
- **Linting**: `cargo clippy -- -D warnings` (fails build if warnings exist)
- **Linting comprehensive**: `cargo clippy -- -W clippy::all -W clippy::pedantic -W clippy::nursery -W clippy::cargo` (warnings only)
- **Unit tests**: `cargo test --verbose`
- **Integration tests**: Dart-based test suite via `make test`
- **MIRI tests**: `cargo +nightly miri test --verbose`
- **MIRI example**: `MIRIFLAGS=-Zmiri-disable-isolation cargo +nightly miri run -- test/overloading/overloading_for_in.gen`
- **Feature builds**: Various `cargo build` and `cargo run` commands with different features
- **Benchmarks**: `make benchmark-ci` (runs last, not critical for merge)

**⚠️ The pipeline typically takes 5-10 minutes to complete. All steps except benchmarks MUST PASS for code to be merged.**

Always ensure your changes pass local testing using the commands in "CRITICAL: CI Validation Requirements" before pushing to avoid CI failures.
