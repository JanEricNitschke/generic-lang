# Fuzzing Generic

This directory contains [cargo-fuzz](https://github.com/rust-fuzz/cargo-fuzz)
targets for the formatter and the in-memory scanner/compiler/VM path:

- `format` feeds arbitrary bytes (decoded lossily to UTF-8) through the
  formatter with several option combinations.
- `interpret` feeds arbitrary bytes through the in-memory
  [`run_source`](../crates/generic-lang-lib/src/lib.rs) API.

Language-level compile errors and uncaught exceptions are expected. A panic,
abort, or sanitizer report is a failure. A timeout is only an operational guard
and needs triage: valid Generic programs can loop forever by design.

The targets build `generic-lang-lib` without default features, so native
plugins are disabled. The module cache is disabled by the harness, and the
script path `<fuzz>/input.gen` is synthetic and never created.

## Setup

```sh
rustup toolchain install nightly --component rust-src
cargo install cargo-fuzz --locked
```

## Seeds

The normal integration tests under `test/` are the seed source. The formatter
uses a comma-separated seed list; the interpreter uses a seed corpus directory
because fork mode reads corpus directories rather than `-seed_inputs`.

```sh
mkdir -p target/fuzz-corpus-format target/fuzz-corpus-interpret

find test -name '*.gen' -print | paste -sd, - \
    > target/fuzz-seeds-format.txt

rsync -a --delete --exclude='pathlib/' --exclude='requests/' \
    --exclude='benchmark/' --include='*/' --include='*.gen' --exclude='*' \
    test/ target/fuzz-corpus-interpret/test/
```

The interpreter target excludes the filesystem, network, and benchmark suites
from its seeds because libFuzzer executes every seed once while measuring
coverage.

## Running

```sh
# Formatter: 20 minutes.
ASAN_OPTIONS=allocator_may_return_null=1 cargo +nightly fuzz run format target/fuzz-corpus-format -- \
    -max_total_time=1200 -timeout=10 -max_len=8192 -close_fd_mask=3 \
    -seed_inputs=@target/fuzz-seeds-format.txt

# Interpreter: 20 minutes, in fork mode so timeouts do not stop the run.
ASAN_OPTIONS=allocator_may_return_null=1 cargo +nightly fuzz run interpret target/fuzz-corpus-interpret -- \
    -fork=1 -ignore_timeouts=1 \
    -max_total_time=1200 -timeout=10 -max_len=8192 -close_fd_mask=3
```

- `allocator_may_return_null=1` makes ASan return `NULL` for absurd allocation
  requests instead of aborting, so the interpreter can report them as normal
  language errors.
- `-max_total_time=1200` sets the fuzzing budget to 20 minutes.
- `-timeout=10` is the per-input liveness guard. The interpreter runs in fork
  mode, so the parent kills a timed-out child and continues; timeouts are
  counted, not fatal. `-ignore_timeouts` only applies in fork mode.
- `-max_len=8192` caps generated inputs at 8 KiB.
- `-close_fd_mask=3` closes stdout and stderr so expected language diagnostics
  do not flood the terminal. Drop it when reproducing a failure.

The `target/fuzz-corpus-*` directories are writable and collect new
coverage-increasing inputs; they are ignored by git. Failures are written to
`fuzz/artifacts/<target>/` (also ignored). Reproduce one with:

```sh
ASAN_OPTIONS=allocator_may_return_null=1 cargo +nightly fuzz run interpret fuzz/artifacts/interpret/crash-<hash>
ASAN_OPTIONS=allocator_may_return_null=1 cargo +nightly fuzz tmin interpret fuzz/artifacts/interpret/crash-<hash>
```

Inspect new corpus entries after a run. Coverage-increasing inputs are
candidates for the normal suite, even when they are not failures: keep valid or
previously untested cases, and drop malformed mutations. Minimize crashes with
`cargo fuzz tmin`, prune large corpora with `cargo fuzz cmin`, and add curated
cases as normal `test/` fixtures. A real finding should always become a normal
regression test.
