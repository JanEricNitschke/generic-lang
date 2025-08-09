# Generic Programming Language

The Generic programming language is a Rust-based interpreter for a Lox-inspired language. It includes a comprehensive test suite, benchmarking system, and supports features like classes, closures, exceptions, and more.

Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.

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

### Always Manually Validate Changes
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

### Linting and Formatting
- **Install clippy component**: `rustup component add clippy` -- takes 5 seconds. Set timeout to 60 seconds.
- **Always run `cargo fmt --check` before committing** -- takes <1 second.
- **Basic clippy checks**: `cargo clippy -- -D warnings` -- takes 8 seconds. Set timeout to 60 seconds.
- **Comprehensive clippy checks**: `cargo clippy -- -W clippy::all -W clippy::pedantic -W clippy::nursery -W clippy::cargo` -- takes 10 seconds. Set timeout to 60 seconds.
- **Install nightly with miri**: `rustup toolchain install nightly --component miri` -- takes 60 seconds. NEVER CANCEL. Set timeout to 120+ seconds.
- **Setup miri**: `cargo +nightly miri setup` -- takes 30 seconds. Set timeout to 60 seconds.
- **Run miri tests**: `cargo +nightly miri test --verbose` -- takes 15 seconds. Set timeout to 60 seconds.
- **Run specific miri test**: `MIRIFLAGS=-Zmiri-disable-isolation cargo +nightly miri run -- test/overloading/overloading_for_in.gen` -- takes 5 seconds. Set timeout to 60 seconds.
- **Ensure all of these pass before pushing** -- CI will fail if code is not properly formatted or has clippy warnings.

### Benchmarking
- **Simple benchmark**: `hyperfine --warmup 1 "./target/release/generic benchmark/fib/fib.gen"` -- takes 10 seconds.
- **Full benchmark suite**: `make benchmark-ci` (requires reference implementations from craftinginterpreters repo)

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
The `.github/workflows/build.yaml` pipeline:
- Builds with multiple feature combinations
- Runs unit tests and integration tests
- Performs linting and formatting checks
- Tests with MIRI for memory safety
- Runs benchmarks against reference implementations
- Typically takes 5-10 minutes to complete

Always ensure your changes pass local testing before pushing to avoid CI failures.