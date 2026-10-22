# The Generic Programming Language

This is the main source code repository for **Generic**. It contains the interpreter, the bundled standard library, and
the documentation.

## Documentation

- **[Language guide](docs/language/README.md)** - syntax, types, classes, exceptions, modules, the standard library, and the built-in unit-test framework, for people writing generic programs.
- **[Plugin authoring guide](docs/plugin-authors.md)** - write native modules (Rust, C, C++, Zig) loaded with a plain `import`. API reference: [`generic-lang-api` on docs.rs](https://docs.rs/generic-lang-api).
- **[Architecture](docs/ARCHITECTURE.md)** - internals for contributors: the scanner → parser/AST → codegen → bytecode → VM pipeline, the value/heap/GC model, the exception model, soudness/correctness invariants that need to be upheld manually.

The workspace is three crates: `generic-lang` (the `generic` binary), `generic-lang-lib` (the interpreter as a library), and `generic-lang-api` (the plugin ABI/authoring API).

## Influence

**Generic** is based on the Lox programming language created by [Bob Nystrom](https://twitter.com/intent/user?screen_name=munificentbob) for his excellent book [Crafting Interpreters](https://craftinginterpreters.com).

This implementation is written in [Rust](https://github.com/rust-lang/rust) and *very* closely based on [clox-rs](https://github.com/abesto/clox-rs/). The walkthrough of the book alongside clox-rs can be found in the previous [repo](https://github.com/JanEricNitschke/CraftingInterpreters).

## Syntax highlighting

A rudimentary vscode extension exists in the form of [generic-lang-vscode](https://github.com/JanEricNitschke/generic-lang-vscode).

## Building

The interpreter can be built like this:
```bash
cargo build --release
```

## Installation

I recommend installing Generic by building from the source code as follows:

```bash
# Download the source code
git clone https://github.com/JanEricNitschke/generic-lang
cd generic-lang

# Install 'generic'
cargo install --path .
```

Or more simply:

```bash
cargo install generic-lang
```

Or install with uv (recommended), pip, or pipx:

```bash
# With uv.
uv tool install generic-lang  # Install generic globally

# With pip.
pip install generic-lang

# With pipx.
pipx install generic-lang
```

Now to use generic, in your terminal, run:
```bash
generic
```

## Formatting

Format a source file to stdout with:

```bash
generic fmt path/to/file.gen
```

Rewrite one or more files in place with `-w` / `--write`:

```bash
generic fmt -w path/to/file.gen other.gen
```

The formatter parses the file and prints a canonical layout with four-space
indentation by default. Use `--indent-width N` to choose a different number
of spaces per level (at least one). Use `--line-length N` to set a soft maximum
line length for comma-separated constructs; omit it or pass `0` to disable
wrapping.

## Testing

**Generic** supports two types of testing:

### Unit Testing (Built-in)

The language includes built-in unit testing support. Write test functions (starting with `test_`) and run them with:

```bash
generic --test your_file.gen
```

This will discover and run all test functions in isolation, providing detailed reporting. See the [Testing chapter](docs/language/testing.md) of the language guide for complete documentation.

### Integration Testing

A comprehensive integration test suite and driver can be found in [test](test) and [tool](tool) respectively.
The driver is written in [Dart](https://dart.dev/) and taken directly from the [book repo](https://github.com/munificent/craftinginterpreters) of Crafting Interpreters.
The suite is an extension of the one that can be found in the same repository.

To run the full integration test suite:
```bash
make test
```

## Module cache

Release builds cache compiled imported `.gen` modules in `__generic_cache__/` next to the source, as `<stem>.generic-<major>.<minor>.genc`. Debug builds compile fresh by default. Cache files contain executable bytecode and are trusted local state; do not commit them.

The cache can be controlled per run:

```bash
generic --no-cache script.gen           # disable reads and writes
generic --no-cache-read script.gen      # ignore existing cache files
generic --no-cache-write script.gen     # do not create or update cache files
```

Flags must come before the script path.

## Native plugins

Generic programs can load native modules - shared libraries (`.so`/`.dylib`/`.dll`) - with a normal `import`:

```generic
import "demo";
print(demo.add(19, 23));   # 42
```

Plugins are written in Rust against the [`generic-lang-api`](https://docs.rs/generic-lang-api) crate, or in any C-ABI language against the generated `crates/generic-lang-api/include/generic.h`. Worked examples in Rust, C, C++, and Zig live in [`plugin-examples/`](plugin-examples). See the [plugin authoring guide](docs/plugin-authors.md).

## Benchmark

Benchmarking **Generic** against the official [c](https://github.com/munificent/craftinginterpreters/tree/master/c) and [java](https://github.com/munificent/craftinginterpreters/tree/master/java/com/craftinginterpreters) lox implementations as well as, to a limited degree, [python](https://www.python.org/) and [ruby](https://www.ruby-lang.org/) is also possible. The reference programs for that can be found in [benchmark](benchmark).

The benchmarking setup is driven by [hyperfine](https://github.com/sharkdp/hyperfine) which has to be installed and added to the path separately.
This also holds for ruby and python.

However, the main benchmarking is against the lox implementations. For those runs to be possible the two versions have to be install from the [book repo](https://github.com/munificent/craftinginterpreters)
and placed into the (relative) path

`{generic-lang}/reference/craftinginterpreters`.

Running the benchmark on linux is done via:
```
make benchmark-ci
```

To run the benchmarking on windows a `jlox.bat` with the following content:
```bat
@echo off

set "script_dir=%~dp0"
java -cp "%script_dir%\build\java" com.craftinginterpreters.lox.Lox %*
```

first has to be placed in the `craftinginterpreters` directory.

The benchmarking is then run via:
```bash
make benchmark
```
