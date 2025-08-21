DEBUG_BIN := ./target/debug/generic
STRESS_GC_BIN := ./target/stress_gc/generic
REL_BIN := ./target/release/generic

test_level := chap30_optimization
sources := src/*.rs src/compiler/*.rs Cargo.toml

$(DEBUG_BIN): $(sources)
	cargo build

$(STRESS_GC_BIN): $(sources)
	cargo build --profile stress_gc --features stress_gc

$(REL_BIN): $(sources)
	cargo build --release

.PHONY: cargo-test
cargo-test:
	cargo test

setup-dart:
	dart pub get --directory=tool

.PHONY: custom-dart-test
custom-dart-test: $(DEBUG_BIN)
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
stress-gc-test: $(STRESS_GC_BIN)
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
