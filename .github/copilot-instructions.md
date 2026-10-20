# Generic Programming Language

Rust-based interpreter for a Lox-inspired dynamic programming language.
Compiles source to bytecode through a parser, an AST, and a codegen pass,
then executes it on a virtual machine.

## Code Standards

### Required Before Each Commit
- **Format check**: `cargo fmt --check`
- **Critical linting**: `cargo clippy -- -D warnings`
- **Comprehensive linting**: `cargo clippy -- -W clippy::all -W clippy::pedantic -W clippy::nursery -W clippy::cargo`

### Development flow
- **Rust unit tests**: `cargo test --verbose`
- **Dart integration tests**: `make test`


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

## Testing Setup

### Test Format with Expectation Comments
Integration tests are `.gen` files in the `test/` directory that use expectation comments:

```generic
var a = "hello";
print(a); # expect: hello

var result = 1 + 2 * 3;
print(result); # expect: 7
```

The Dart test runner validates that `print()` statements produce output
matching the `# expect: value` comments. Compile-error tests use
`# [line N] Error ...` comments; `#|` rows pin exact stderr snippets
(a bare `#|` asserts no snippet).

## Key Guidelines
1. Follow Rust best practices and idiomatic patterns
2. Maintain existing code structure and organization
3. Write integration tests for new functionality.
4. Document public APIs and complex logic.
