DEBUG_BIN := ./target/debug/generic
STRESS_GC_BIN := ./target/stress_gc/generic
REL_BIN := ./target/release/generic

test_level := chap30_optimization
sources := $(shell find crates -type f \( -name '*.rs' -o -name '*.gen' \)) \
           Cargo.toml Cargo.lock $(wildcard crates/*/Cargo.toml)

$(DEBUG_BIN): $(sources)
	cargo build

$(STRESS_GC_BIN): $(sources)
	cargo build --profile stress_gc -p generic-lang --features stress_gc

$(REL_BIN): $(sources)
	cargo build --release

.PHONY: cargo-test
cargo-test:
	cargo test

# Regenerate the C header from the plugin ABI types. The header is checked
# in; CI fails if it is out of date. Requires `cargo install cbindgen`.
.PHONY: generate-plugin-header
generate-plugin-header:
	cbindgen --config crates/generic-lang-api/cbindgen.toml \
		--crate generic-lang-api \
		--output crates/generic-lang-api/include/generic.h \
		crates/generic-lang-api

# Regenerate the header and verify the committed copy matches. The header is
# purely cbindgen's output — nothing else is checked or linted here; whether
# it compiles is covered by building the example plugins against it.
.PHONY: check-plugin-header
check-plugin-header: generate-plugin-header
	git diff --exit-code crates/generic-lang-api/include/generic.h

setup-dart:
	dart pub get --directory=tool

# Static analysis + format check for the dart test runner.
.PHONY: dart-lint
dart-lint: setup-dart
	dart analyze --fatal-infos tool
	dart format --output=none --set-exit-if-changed tool

# Run the code samples embedded in the markdown docs (docs/ and the README),
# so they cannot drift from the language: ```generic fences must run cleanly
# (and match any `# expect:` comments), ```generic-error fences must exit
# with an error. The built Rust example plugin is placed beside the samples
# as demo.<ext>, so the plugin-import samples really run.
.PHONY: docs-test
docs-test: $(DEBUG_BIN) setup-dart plugin-test-fixture
	dart tool/bin/doc_test.dart --interpreter ./$(DEBUG_BIN) \
		--docs docs --docs README.md \
		--fixture demo.$(DYLIB_EXT)=test/plugin/rust/rust_demo_plugin.$(DYLIB_EXT)

# C/C++ drivers for the plugin fixtures. make's builtin default for CXX is
# g++; use the platform driver instead. On Windows there is no `cc`/`c++`
# driver — Zig (required for the Zig fixture anyway) doubles as the C/C++
# compiler. Override on the command line if needed (make ... CC=... CXX=...).
# All recipes assume an sh-compatible shell (on Windows: Git Bash / MSYS).
CXX = c++
ifeq ($(OS),Windows_NT)
CC  = zig cc
CXX = zig c++
endif

# Warnings for the example plugins, modeled on the strictest practical set.
# -Waggregate-return is omitted on purpose: the plugin ABI returns structs by value by design.
PLUGIN_WARNINGS = -Wall -Wextra -pedantic -Werror -Wfloat-equal -Wundef \
	-Wshadow -Wpointer-arith -Wcast-align -Wstrict-overflow=5 -Wwrite-strings \
	-Wcast-qual -Wswitch-default -Wswitch-enum -Wconversion -Wunreachable-code \
	-Wformat=2 -Winit-self
PLUGIN_WARNINGS_C = $(PLUGIN_WARNINGS) -Wstrict-prototypes

# Platform shared-library extension for the plugin fixtures.
DYLIB_EXT := $(if $(filter Darwin,$(shell uname -s)),dylib,$(if $(filter Windows_NT,$(OS)),dll,so))
# The Rust cdylib file cargo produces (no `lib` prefix on Windows).
PLUGIN_LIB := $(if $(filter Windows_NT,$(OS)),generic_test_plugin.dll,libgeneric_test_plugin.$(DYLIB_EXT))
PLUGIN_INC := crates/generic-lang-api/include

# Build the Rust example plugin (its source lives in plugin-examples/rust)
# and place its dylib next to the Rust `.gen` tests as
# test/plugin/rust/rust_demo_plugin.<ext>, where import resolution looks for
# it. This runs as part of the normal dart suite, so the Rust plugin tests
# always run with everything else.
# The dylib is removed before the copy: overwriting a dylib in place
# invalidates macOS's cached code signature for that inode, and later
# dlopen()s of it get SIGKILLed.
.PHONY: plugin-test-fixture
plugin-test-fixture:
	cargo build --manifest-path plugin-examples/rust/Cargo.toml
	rm -f test/plugin/rust/rust_demo_plugin.$(DYLIB_EXT)
	cp plugin-examples/rust/target/debug/$(PLUGIN_LIB) test/plugin/rust/rust_demo_plugin.$(DYLIB_EXT)

# The Zig plugin builds through its build.zig (translate-c of the header).
# Zig's install-dir naming follows the platform dylib convention (no `lib` prefix on Windows).
# Path within zig-out/: Zig installs DLLs as runtime artifacts into bin/ on
# Windows (lib/ holds only the import .lib there); POSIX dylibs go to lib/.
ZIG_OUT_LIB := $(if $(filter Windows_NT,$(OS)),bin/zig_demo_plugin.dll,lib/libzig_demo_plugin.$(DYLIB_EXT))

# Build the cross-language example plugins (C, C++, Zig, then the bad-plugin
# loader-path fixtures) from plugin-examples/ into test/plugin/lang/, next to
# their `.gen` tests. Requires the C/C++/Zig toolchains; run via
# `make plugin-lang-test`, which is separate from the normal suite.
# The demo plugins build with the full warning set as errors — they are
# showcase code. The bad/ fixtures are deliberately wrong (wrong_abi.c uses a
# zero-length array, a GNU extension) and build without -Werror/-pedantic.
.PHONY: plugin-lang-fixture
plugin-lang-fixture: plugin-bad-fixture
	mkdir -p test/plugin/lang
	$(CC)  -shared -fPIC -std=c2x $(PLUGIN_WARNINGS_C) -I $(PLUGIN_INC) -o test/plugin/lang/c_demo_plugin.$(DYLIB_EXT) plugin-examples/c/c_demo_plugin.c
	$(CXX) -shared -fPIC -std=c++23 $(PLUGIN_WARNINGS) -I $(PLUGIN_INC) -o test/plugin/lang/cpp_demo_plugin.$(DYLIB_EXT) plugin-examples/cpp/cpp_demo_plugin.cpp
	cd plugin-examples/zig && zig build -Doptimize=ReleaseSafe -Dcpu=baseline
	rm -f test/plugin/lang/zig_demo_plugin.$(DYLIB_EXT)
	cp plugin-examples/zig/zig-out/$(ZIG_OUT_LIB) test/plugin/lang/zig_demo_plugin.$(DYLIB_EXT)

# Every loader-rejection fixture, in one place so the plain, ASan, and
# valgrind suites all exercise the same full set. Never instrumented: the
# loader rejects these before any plugin code runs.
.PHONY: plugin-bad-fixture
plugin-bad-fixture:
	mkdir -p test/plugin/lang
	$(CC)  -shared -fPIC -Wall -Wextra -I $(PLUGIN_INC) -o test/plugin/lang/wrongabi.$(DYLIB_EXT) plugin-examples/bad/wrong_abi.c
	$(CC)  -shared -fPIC -Wall -Wextra -I $(PLUGIN_INC) -o test/plugin/lang/noinit.$(DYLIB_EXT)   plugin-examples/bad/no_init.c
	$(CC)  -shared -fPIC -Wall -Wextra -I $(PLUGIN_INC) -o test/plugin/lang/nullfun.$(DYLIB_EXT)  plugin-examples/bad/null_fun.c
	$(CC)  -shared -fPIC -Wall -Wextra -I $(PLUGIN_INC) -o test/plugin/lang/badname.$(DYLIB_EXT)  plugin-examples/bad/bad_name.c
	$(CC)  -shared -fPIC -Wall -Wextra -I $(PLUGIN_INC) -o test/plugin/lang/noarities.$(DYLIB_EXT) plugin-examples/bad/no_arities.c
	$(CC)  -shared -fPIC -Wall -Wextra -I $(PLUGIN_INC) -o test/plugin/lang/nulldesc.$(DYLIB_EXT) plugin-examples/bad/null_desc.c
	$(CC)  -shared -fPIC -Wall -Wextra -I $(PLUGIN_INC) -o test/plugin/lang/nulltable.$(DYLIB_EXT) plugin-examples/bad/null_table.c
	printf 'not a real dylib, just text\n' > test/plugin/lang/corrupt.$(DYLIB_EXT)

# Build the cross-language plugins, then run only their `.gen` tests (the
# `plugin-lang` dart suite, which the normal `clox` suite skips).
.PHONY: plugin-lang-test
plugin-lang-test: $(DEBUG_BIN) plugin-lang-fixture
	dart tool/bin/test.dart plugin-lang --interpreter ./$(DEBUG_BIN)

# The example plugins must compile at the oldest standards we support — C99
# (designated initializers, // comments, <stdint.h>/<stdbool.h>) and C++11
# (the lambda-based `guarded` exception wall; the header alone is fine as
# C++03) — as well as at the newest published ones, which the fixture builds
# above use. The oldest-standard checks drop -pedantic: the header's FATAL
# status value exceeds `int`, which only became standard C in C23.
.PHONY: plugin-std-check
plugin-std-check:
	$(CC)  -c -std=c99   $(filter-out -pedantic,$(PLUGIN_WARNINGS_C)) -I $(PLUGIN_INC) -o plugin-examples/std_check.o plugin-examples/c/c_demo_plugin.c
	$(CXX) -c -std=c++11 $(filter-out -pedantic,$(PLUGIN_WARNINGS))   -I $(PLUGIN_INC) -o plugin-examples/std_check.o plugin-examples/cpp/cpp_demo_plugin.cpp
	rm -f plugin-examples/std_check.o

# Lint the example plugin sources: C/C++ with clang-format + cpplint, Zig
# with zig fmt, Rust with rustfmt + clippy (workspace-excluded, so
# `--all`/`--workspace` invocations do not cover it). The generated C header
# is generated code with its own gate (`make check-plugin-header`), so it is
# not style-linted here.
CLANG_FORMAT ?= clang-format
.PHONY: plugin-lint
plugin-lint:
	$(CLANG_FORMAT) --dry-run --Werror --style=file plugin-examples/c/*.c plugin-examples/cpp/*.cpp plugin-examples/bad/*.c
	cpplint --filter=-build/include_subdir,-build/include_order,-legal/copyright,-readability/casting,-whitespace/parens --linelength=100 plugin-examples/c/*.c plugin-examples/cpp/*.cpp plugin-examples/bad/*.c
	zig fmt --check plugin-examples/zig/*.zig
	cargo fmt --check --manifest-path plugin-examples/rust/Cargo.toml
	cargo clippy --manifest-path plugin-examples/rust/Cargo.toml --all-targets -- -W clippy::all -W clippy::pedantic -W clippy::nursery -W clippy::cargo -D warnings

# Run every plugin `.gen` test with an AddressSanitizer-instrumented host and
# Rust plugin, plus sanitized C/C++ plugins (rust nightly for -Zsanitizer;
# the Zig plugin is loaded uninstrumented — Zig has no ASan link option, and
# ReleaseSafe carries its own checks). Catches memory errors and UB across
# the dlopen/FFI boundary that miri cannot see. Leak detection is off: the
# VM does not tear down its heap at exit.
#
# The plugins get ASan only on Linux (C/C++ built with clang): instrumented
# shared libraries leave the __asan_* symbols undefined and resolve them at
# dlopen from the host executable's statically-linked runtime — which is
# only visible if the host is linked with --export-dynamic.
# macOS ships only a dynamic ASan runtime for dylibs, which cannot be
# dlopen'ed into a statically-sanitized host ("loaded too late"), so there
# they get trap-mode UBSan only (runtime-free; UB aborts the test).
ifeq ($(shell uname -s),Darwin)
ASAN_PLUGIN_CC       := $(CC)
ASAN_PLUGIN_CXX      := $(CXX)
ASAN_PLUGIN_SANITIZE := -fsanitize=undefined -fsanitize-trap=undefined
ASAN_HOST_RUSTFLAGS  := -Zsanitizer=address
else
ASAN_PLUGIN_CC       := clang
ASAN_PLUGIN_CXX      := clang++
ASAN_PLUGIN_SANITIZE := -fsanitize=address,undefined -fsanitize-trap=undefined
# --export-dynamic: instrumented plugins resolve __asan_* from the host.
# -lstdc++ (kept via --no-as-needed): ASan's __cxa_throw interceptor needs
# libstdc++ present at init, or a C++ plugin's `throw` hits a null "real"
# pointer (the interceptor CHECK-fails) once the host exports its symbols.
ASAN_HOST_RUSTFLAGS  := -Zsanitizer=address -Clink-arg=-Wl,--export-dynamic \
                        -Clink-arg=-Wl,--no-as-needed -Clink-arg=-lstdc++
endif
ASAN_TARGET := $(shell rustc -vV | sed -n 's/^host: //p')
ASAN_BIN    := target/$(ASAN_TARGET)/debug/generic
.PHONY: plugin-asan-test
plugin-asan-test: plugin-bad-fixture
	RUSTFLAGS="$(ASAN_HOST_RUSTFLAGS)" cargo +nightly build --target $(ASAN_TARGET) -p generic-lang
	RUSTFLAGS=-Zsanitizer=address cargo +nightly build --target $(ASAN_TARGET) --manifest-path plugin-examples/rust/Cargo.toml
	rm -f test/plugin/rust/rust_demo_plugin.$(DYLIB_EXT)
	cp plugin-examples/rust/target/$(ASAN_TARGET)/debug/$(PLUGIN_LIB) test/plugin/rust/rust_demo_plugin.$(DYLIB_EXT)
	mkdir -p test/plugin/lang
	$(ASAN_PLUGIN_CC)  -shared -fPIC -g -std=c2x $(ASAN_PLUGIN_SANITIZE) $(PLUGIN_WARNINGS_C) -I $(PLUGIN_INC) -o test/plugin/lang/c_demo_plugin.$(DYLIB_EXT) plugin-examples/c/c_demo_plugin.c
	$(ASAN_PLUGIN_CXX) -shared -fPIC -g -std=c++23 $(ASAN_PLUGIN_SANITIZE) $(PLUGIN_WARNINGS) -I $(PLUGIN_INC) -o test/plugin/lang/cpp_demo_plugin.$(DYLIB_EXT) plugin-examples/cpp/cpp_demo_plugin.cpp
	cd plugin-examples/zig && zig build -Doptimize=ReleaseSafe
	rm -f test/plugin/lang/zig_demo_plugin.$(DYLIB_EXT)
	cp plugin-examples/zig/zig-out/$(ZIG_OUT_LIB) test/plugin/lang/zig_demo_plugin.$(DYLIB_EXT)
	for f in test/plugin/rust/*.gen test/plugin/lang/*.gen; do \
		echo "asan $$f"; \
		ASAN_OPTIONS=detect_leaks=0 UBSAN_OPTIONS=halt_on_error=1 ./$(ASAN_BIN) $$f >/dev/null || { echo "sanitizer failure in $$f"; exit 1; }; \
	done
	$(MAKE) plugin-test-fixture plugin-lang-fixture  # restore uninstrumented fixtures

# Run every plugin `.gen` test under valgrind memcheck (Linux CI; valgrind
# does not support recent macOS). This catches memory errors across the
# dlopen/FFI boundary that neither miri (which cannot dlopen) nor the plain
# suites see. Leak checking stays off: the VM does not tear down its heap at
# exit, so leak reports would be all host noise. Any valgrind-detected error
# fails via the sentinel exit code; any other nonzero exit (a crash, panic,
# or test failure of the run itself) fails the target too.
.PHONY: plugin-valgrind-test
plugin-valgrind-test: $(DEBUG_BIN) plugin-test-fixture plugin-lang-fixture
	command -v valgrind >/dev/null || { echo "valgrind is not installed"; exit 1; }
	for f in test/plugin/rust/*.gen test/plugin/lang/*.gen; do \
		echo "valgrind $$f"; \
		valgrind --quiet --error-exitcode=97 ./$(DEBUG_BIN) $$f >/dev/null; \
		rc=$$?; \
		if [ $$rc -eq 97 ]; then echo "valgrind found errors in $$f"; exit 1; \
		elif [ $$rc -ne 0 ]; then echo "test failed (exit $$rc) in $$f"; exit 1; fi; \
	done

.PHONY: custom-dart-test
custom-dart-test: $(DEBUG_BIN) plugin-test-fixture
	dart tool/bin/test.dart clox --interpreter ./$(DEBUG_BIN)

.PHONY: unittest-test
unittest-test: $(DEBUG_BIN)
	dart tool/bin/test.dart generic-unittest --interpreter $(DEBUG_BIN) --arguments --test

.PHONY: unittest-test-single
unittest-test-single: $(DEBUG_BIN)
	dart tool/bin/test.dart generic-unittest-single --interpreter $(DEBUG_BIN) --arguments --test

.PHONY: unittest-test-directory
unittest-test-directory: $(DEBUG_BIN)
	dart tool/bin/test.dart generic-unittest-directory --interpreter $(DEBUG_BIN) --arguments --test

.PHONY: stress-gc-test
stress-gc-test: $(STRESS_GC_BIN) plugin-test-fixture
	dart tool/bin/test.dart clox --interpreter ./$(STRESS_GC_BIN)

.PHONY: test
test: cargo-test setup-dart custom-dart-test stress-gc-test unittest-test unittest-test-single unittest-test-directory

.PHONY: benchmark
benchmark: fib-benchmark more-benchmark

.PHONY: more-benchmark
more-benchmark: $(REL_BIN)
	for filename in benchmark/*.gen; do \
		echo $$filename; \
		filebase=$${filename%.*}; \
		hyperfine --reference ".\\target\\release\\generic $${filebase}.gen" --show-output --warmup 1 ".\\reference\\craftinginterpreters\\clox $${filebase}.lox.nom" ".\\reference\\craftinginterpreters\\jlox.bat $${filebase}.lox.nom"; \
	done

.PHONY: fib-benchmark
fib-benchmark: $(REL_BIN)
	hyperfine --reference ".\\target\release\\generic benchmark\\fib\\fib.gen" --warmup 1 ".\\reference\\craftinginterpreters\\clox benchmark\\fib\\fib.lox.nom"   \
	"python benchmark\\fib\\fib.py" "ruby benchmark\\fib\\fib.rb" ".\\reference\\craftinginterpreters\\jlox.bat benchmark\\fib\\fib.lox.nom" \

.PHONY: benchmark-ci
benchmark-ci: fib-benchmark-ci more-benchmark-ci

.PHONY: more-benchmark-ci
more-benchmark-ci: $(REL_BIN)
	for filename in benchmark/*.gen; do \
		echo $$filename; \
		filebase=$${filename%.*}; \
		hyperfine --reference "./target/release/generic $${filebase}.gen" --show-output --warmup 1 "./reference/craftinginterpreters/clox $${filebase}.lox.nom" "./reference/craftinginterpreters/jlox $${filebase}.lox.nom"; \
	done

.PHONY: fib-benchmark-ci
fib-benchmark-ci: $(REL_BIN)
	hyperfine --reference "./target/release/generic benchmark/fib/fib.gen" --warmup 1 "./reference/craftinginterpreters/clox benchmark/fib/fib.lox.nom"   \
	"python benchmark/fib/fib.py" "ruby benchmark/fib/fib.rb" "./reference/craftinginterpreters/jlox benchmark/fib/fib.lox.nom" \
