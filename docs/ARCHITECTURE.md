# generic-lang Architecture

Internals documentation for contributors and coding agents. One concept per
section, stable headings, `file:line` anchors into the code. Paths are
relative to `crates/generic-lang-lib/` unless stated otherwise. Line
numbers drift — treat them as "near here" and re-verify when precision
matters; function names are the stable handle.

The interpreter descends from *Crafting Interpreters*' clox (bytecode VM,
single-pass compiler) and has grown far beyond it: modules, exceptions,
generators, native container classes, big integers/rationals, f-strings,
and a C-ABI plugin system.

## Workspace layout

```
crates/generic-lang/       binary crate: CLI (clap) → calls into the lib
crates/generic-lang-lib/   the language: scanner, compiler, heap, VM, natives, stdlib
crates/generic-lang-api/   plugin FFI/ABI crate — the only repr(C) code anywhere
test/                      .gen end-to-end tests (dart runner, `# expect:` comments)
tool/                      the dart test runner
docs/                      this file, plugin-authors.md
PLUGIN_SYSTEM.md           plugin design doc (its Part 1 is a deeper cut of several
                           sections below); PLUGIN_OVERVIEW.md, PLUGIN_NEXT_STEPS.md
```

## Execution pipeline

`interpret` (`src/vm/mod.rs`, `VM::interpret`) drives: register native
functions/classes/methods (`src/natives/mod.rs::define`) → execute the
embedded generic builtins (`src/builtins/*.gen` — defines the exception
class hierarchy, populates `VM::builtins`) → register rust-stdlib modules →
compile the script → run.

Compilation is scanner → single-pass compiler → bytecode `Chunk`
(`src/scanner.rs`, `src/compiler/`, `src/chunk.rs`). Even the top-level
script is a function wrapped in a closure. The VM
(`src/vm/`) is a stack machine: `run` (`src/vm/mod.rs:289`) loops over
`run_instruction!` (`src/vm/run_instruction.rs`, a macro so tracing and GC
hooks inline).

## Value representation

`Value` (`src/value/mod.rs:29`) is a 32-byte `Copy` enum: immediates
(`Bool`, `Nil`, `StopIteration`, `Number`) plus arena IDs for everything
heap-allocated (`String`, `Closure`, `Function`, `Module`, `Upvalue`,
`NativeFunction`, `NativeMethod`, `Class`, `Instance`, `BoundMethod`).
The size is load-bearing (plugin ABI bit-copies the enum);
`test_value_size` (`src/value/mod.rs`) asserts it, and
`src/vm/plugins/host_api.rs` has `const` asserts that fail the build on
drift.

- IDs are versioned `slotmap` keys (`src/heap/arenas.rs`): using a stale ID
  after its slot was reused is a deterministic panic, never silent
  aliasing. This converts use-after-free bugs into loud crashes.
- `Number` = `Float(f64) | Integer(GenericInt) | Rational(..)`;
  `GenericInt = Small(i64) | Big(BigIntId)` (`src/value/number.rs`).
- Strings are interned: `Heap::string_id` (`src/heap/mod.rs`) returns the
  existing ID for equal content. String heap buffers are address-stable
  while the string lives (the arena may move the `String` struct, not its
  buffer).
- Containers (`List`, `Tuple`, `Dict`, `Set`, `Range`), iterators,
  exceptions, generators, and templates are all `Instance` values whose
  `backing: Option<NativeClass>` holds the native payload
  (`src/value/natives.rs:100`, `src/value/classes.rs::Instance`). A user
  class inheriting from a native class gets that backing on
  instantiation — behavior follows the backing, identity follows the
  class.

## Heap and garbage collection

Mark-and-sweep over per-type arenas (`src/heap/mod.rs`,
`src/heap/arenas.rs`).

**The one invariant everything leans on: allocation never collects.**
`Heap::add_*` only track `bytes_allocated`/`needs_gc`. `collect_garbage`
(`src/vm/garbage_collection.rs:34`) has exactly one call site — the
instruction-dispatch macro (`src/vm/run_instruction.rs:14`). Therefore GC
can only run at instruction boundaries, which means: straight-line native
code cannot trigger a sweep, and only code that re-enters the interpreter
(runs bytecode) must think about rooting.

- Root set (`garbage_collection.rs:42-65`): VM stack, callstack, open
  upvalues, modules, builtins.
- The rooting discipline for mid-call temporaries in natives: push onto the
  VM stack (it is a root), rely on the dispatch site's post-call truncate
  for cleanup. `VM::for_each_rooted` (`src/vm/dunder.rs:149`) is the
  canonical helper: roots a batch, runs a stack-neutral callback per
  element (which may re-enter via `__hash__`/`__eq__`), truncates.
- Dict/Set are GC-safe via static-method APIs `Dict::{add,get,contains,
  remove,probe}` / `Set::{...}` (`src/value/natives.rs:350` / `:266`)
  taking `(vm, receiver, ...)`: the container stays in the heap the whole
  time; the table is only borrowed between re-entry points; matched
  entries are re-located by identity because a reentrant dunder may have
  mutated the container meanwhile.
- The `stress_gc` cargo feature forces a collection at every instruction
  boundary; the full `.gen` suite runs under it in CI (`make
  stress-gc-test`). Any missing root becomes a deterministic panic there.

## Exceptions

The model is **pending-on-the-stack**; there is no "currently handling"
state on the VM.

- `ExceptionKind` (`src/vm/exception_handling.rs:17`): `#[repr(u32)]`, 11
  variants named exactly like the builtin exception classes (strum
  `IntoStaticStr` derives the class name; pinned by
  `every_exception_kind_has_a_builtin_class` at the bottom of the same
  file). The enum is interpreter-internal — it does not cross the plugin
  FFI.
- `VmErrorKind` (`src/vm/errors.rs`) splits the error channel:
  `Runtime(RuntimeErrorKind)` is fatal and always propagates;
  `Exception(ExceptionRaisedKind)` means *a pending exception instance is
  sitting rooted on the stack top*. `VmResult<T> = Result<T, VmErrorKind>`.
- `VM::throw(kind, msg)` (`exception_handling.rs:411`) creates a real
  instance of the named builtin class (`create_exception`, single
  kind→class mapping), pushes it (pending, rooted), attaches the stack
  trace at the throw site, and returns `Err(Exception)`. **No unwinding
  happens at the throw site.** Natives throw with
  `Err(vm.throw(Kind, &msg).unwrap_err())`.
- Control transfer happens in exactly one place:
  `resolve_pending_exception(call_depth)` (`exception_handling.rs:244`) —
  pops the innermost handler if it belongs to the current callstack region,
  truncates modules/callstack/stack to the handler's snapshot, jumps to the
  catch block, re-delivers the exception on the stack for
  `OP_COMPARE_EXCEPTION`. Returns `false` when the handler belongs to an
  outer region — the pending exception then *escapes* to whoever entered
  the region.
- Two dispatch loops resolve: the top level (`run`, depth 0; unhandled →
  `report_uncaught_exception` → fatal) and region-scoped
  `run_function_from_depth` (`src/vm/functions.rs`), which hands escapes
  over **untruncated** — cleanup belongs to the caller.
- `invoke_and_run_function` (`src/vm/functions.rs:44`) is the single entry
  point for every native/dunder re-entry and owns the cleanup contract:
  snapshot frames/modules/stack; on `Err(Exception)` restore the snapshot
  with the exception re-pushed on top. Result: a native always sees "state
  exactly as before the call, exception on top" and can pop-and-handle or
  propagate with `?`. This is what makes natives (and plugin host
  callbacks) able to catch exceptions from generic code.
- Generators suspend their active handlers into the generator object
  (`SuspendedExceptionHandler`, re-based on resume) and mark themselves
  `Completed` when an exception escapes them (`Generator::resume_with`,
  `src/value/natives.rs:782`).

## Native calling convention

- Signatures (`src/value/natives.rs:94-96`):
  `NativeFunctionImpl = fn(&mut VM, &[Value]) -> VmResult<Value>`, methods
  additionally take a `&Value` receiver.
- Both dispatch sites live in `src/vm/functions.rs`
  (`execute_native_function_call` / `execute_native_method_call`): copy the
  args off the stack into a `TinyVec<[Value; 8]>` (stack storage for ≤8,
  heap only for large variadic calls), call, truncate down to and
  including the callee slot, push the result. The originals stay on the VM
  stack during the call — that is why arguments are always GC-safe inside
  a native.
- **Callee-below-the-args invariant**: every path into
  `execute_native_function_call` keeps the callee `Value` at
  `stack[len - argc - 1]` until the post-call truncate. The plugin
  trampoline reads its `extern "C"` pointer back off the callee through
  this invariant (`src/vm/plugins/trampolines.rs`).
- Arity is a `&'static [u8]` of accepted counts, checked before the call
  (`VARIADIC_0_PLUS` in `src/natives/mod.rs` covers 0–255).
- Helpers natives may call must be **stack-neutral on return** (push/pop
  freely while running, restore depth before returning) — several rooting
  patterns index the stack by position and rely on it
  (`for_each_rooted` debug-asserts it).

## Import machinery

`import_file` (`src/vm/import.rs:22`) resolves a fallback chain:

1. user `.gen` file on disk (with a circular-import check against the
   module stack),
2. **native plugin** — `{name, lib<name>}.<dylib-ext>` next to the resolved
   path (`VM::try_import_plugin`; an inlined no-op stub when the `plugins`
   feature is off),
3. embedded generic stdlib (`GENERIC_STDLIB_DIR`, compiled-in `.gen`
   sources),
4. rust stdlib map (`VM::stdlib`, registered at startup).

Generic modules compile to a closure that executes to completion; its
globals become the module object. Rust-stdlib and plugin modules build a
`Module` directly, one heap `NativeFunction` per export
(`import_rust_stdlib` / `import_plugin_module`). `from`-imports move
individual globals instead of registering the module.

## Plugin host

Full design and rationale: `PLUGIN_SYSTEM.md` (+ condensed
`PLUGIN_OVERVIEW.md`); author-facing docs: `docs/plugin-authors.md`. The
one-paragraph internal view: values cross the C ABI as opaque 32-byte
bit-copies of `Value`; plugins operate on them exclusively through a
`HostApi` vtable whose re-entering callbacks run bytecode through the
`invoke_and_run_function` contract and root their arguments first; plugin
functions are ordinary heap `NativeFunction`s whose `fun` is one shared
trampoline and whose `plugin_fn` field holds the real `extern "C"`
pointer; exceptions cross as instances with full identity (created from /
checked against class values via `builtin_get`/`exception_new`/
`is_instance`), fatal errors as `FfiStatus::Fatal`, which stays
uncatchable.
Everything is feature-gated (`plugins`, in default); libraries are never
unloaded. Code: `src/vm/plugins/` (mod/host_api/loader/trampolines/tests).

## Load-bearing invariants (the "do not break" list)

1. Allocation never collects; `collect_garbage` has exactly one call site.
2. `Value` is 32 bytes, ≤8-aligned, `Copy` (plugin ABI bit-copies it).
3. A pending exception is always rooted on the stack top, and
   `resolve_pending_exception` is the only place control transfers to a
   handler.
4. `invoke_and_run_function`'s contract: consume receiver+args, leave
   exactly one value (result or pending exception) on top, state otherwise
   restored.
5. VM helpers that natives call are stack-neutral on return.
6. The callee sits at `stack[len - argc - 1]` throughout a native call.
7. Arena keys are versioned; never cache raw references across possible
   re-entry — re-fetch through IDs.
8. `NativeFunction.arity` is `&'static [u8]`; plugin arities are leaked
   once at load (bounded: libraries never unload).
9. Exception class names ↔ `ExceptionKind` variant names stay in sync
   (pinned by `every_exception_kind_has_a_builtin_class`); the FFI carries
   no exception vocabulary to drift.

## Testing and CI

Test pyramid:
- `cargo test` — unit tests in the lib (incl. the plugin host tests) and
  the API crate (mock-vtable FFI glue tests, layout asserts).
- `.gen` end-to-end tests under `test/` — `# expect:` /
  `# expect runtime error:` comments, run by the dart tool
  (`make custom-dart-test`), again under `stress_gc`
  (`make stress-gc-test`), plus the unittest-runner suites. The Rust plugin
  tests (`test/plugin/rust/`) run in the normal suite; the cross-language
  plugin tests (`test/plugin/lang/`, skipped by the normal suite) build the
  C/C++/Zig example plugins from `plugin-examples/` and run via
  `make plugin-lang-test`.
- miri runs the whole workspace test suite (plugin blob-crossing tests are
  `#[cfg(not(miri))]` — bit-copying `Value` padding is what miri rejects,
  by ABI design) and one example program.

**Run the full CI matrix locally before declaring any change done** — CI
only runs on `main` pushes/PRs, so local runs are the only gate on feature
branches (this has bitten: a stale call in `trace_execution_verbose`-gated
code shipped three commits before anything compiled it). The complete
list mirrors `.github/workflows/build.yaml`:

```sh
cargo hack build --feature-powerset --skip print_code_builtin,trace_execution_builtin,debug_parser_builtin,debug_scanner_builtin --no-dev-deps -p generic-lang-lib
cargo build
cargo fmt --all --check
cargo clippy -- -D warnings
cargo clippy --all-targets -- -W clippy::all -W clippy::pedantic -W clippy::nursery -W clippy::cargo   # zero warnings expected
cargo hack clippy --each-feature --no-dev-deps -p generic-lang-lib -- -W clippy::all -W clippy::pedantic -W clippy::nursery -W clippy::cargo
make check-plugin-header   # regenerate the header, fail on drift (it is purely cbindgen output)
make plugin-lint   # clang-format + cpplint (C/C++), zig fmt (Zig), rustfmt+clippy (Rust) on plugin-examples/
make plugin-std-check   # C/C++ examples must compile at the oldest supported standards (C99/C++11)
make dart-lint     # dart analyze + format check on the test runner in tool/
RUSTDOCFLAGS="-D warnings" cargo doc -p generic-lang-api --no-deps   # public ABI fully documented
cargo test
cargo test --no-default-features -p generic-lang-lib   # feature-off build/tests
cargo +nightly miri test
MIRIFLAGS=-Zmiri-disable-isolation cargo +nightly miri run -- test/overloading/overloading_for_in.gen
cargo run --features print_code_verbose      test/function_literals/literals_as_decorators.gen
cargo run --features trace_execution_verbose test/exceptions/exceptions_deep.gen
! cargo run --features debug_parser          test/function_literals/neither_block_nor_expression.gen
cargo run --features debug_scanner           test/fstrings/nested.gen
make docs-test     # run the ```generic code samples embedded in docs/ and README.md
make test          # dart suites incl. stress-gc + unittest runners
make plugin-lang-test   # C/C++/Zig example plugins + their .gen tests (needs cc/c++/zig)
make plugin-valgrind-test   # plugin .gen tests under valgrind memcheck (Linux only)
make plugin-asan-test   # plugin .gen tests with ASan host + ASan/UBSan C/C++ plugins (rust nightly)
```

## Feature flags (`crates/generic-lang-lib/Cargo.toml`)

- `plugins` (default) — the native plugin system; off = zero
  plugin-related code and dependencies.
- `stress_gc` — collect at every instruction boundary.
- `log_gc` — GC logging.
- `print_code[_verbose|_builtin]`, `trace_execution[_verbose|_builtin]`,
  `debug_parser[_builtin]`, `debug_scanner[_builtin]` — compiler/VM debug
  output. These are real compile targets: feature-gated code that nothing
  builds locally is exactly where breakage hides.
