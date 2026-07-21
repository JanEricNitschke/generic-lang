# Writing Native Plugins for generic

Native plugins are shared libraries (`.so` / `.dylib` / `.dll`) that a
generic program loads with a plain `import` statement. They can be written
in Rust against the `generic-lang-api` crate, or in any language that
speaks the C ABI against the generated header
`crates/generic-lang-api/include/generic.h`.

**Trust model up front:** plugins are trusted native code. The interpreter
checks the ABI version and validates the module descriptor, but a buggy or
malicious plugin can crash or corrupt the process. Do not load plugins you
do not trust.

## How plugins are found

```generic
import "demo";
```

resolves like any import, with plugins as the second stop in the fallback
chain:

1. a user module `demo.gen` next to the importing script,
2. **a plugin**: `demo.<ext>` or `libdemo.<ext>` (platform dylib naming;
   the unprefixed name wins if both exist) in the same directory,
3. the embedded generic stdlib,
4. the native Rust stdlib.

Because plugins resolve *before* the stdlib, a plugin can shadow a
stdlib module of the same name. `from "demo" import shout;` works
like any from-import. Re-importing the same plugin reuses the already
loaded library (per-path cache); libraries are never unloaded while the
interpreter runs.

## Quickstart: Rust

`Cargo.toml`:

```toml
[package]
name = "demo-plugin"
version = "0.1.0"
edition = "2024"

[lib]
crate-type = ["cdylib"]

[dependencies]
generic-lang-api = "0.1"
```

`src/lib.rs`:

```rust
use generic_lang_api::{ArgValue, GenericValue, Host, PluginError};

fn add(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    match (host.decode(args[0]), host.decode(args[1])) {
        (ArgValue::Int(a), ArgValue::Int(b)) => Ok(host.make_int(a + b)),
        (ArgValue::Float(a), ArgValue::Float(b)) => Ok(host.make_float(a + b)),
        _ => Err(host.type_error("add expects two ints or two floats")),
    }
}

fn shout(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let Some(s) = host.as_str(args[0]) else {
        return Err(host.type_error("shout expects a string"));
    };
    let loud = format!("{}!", s.to_uppercase());
    Ok(host.make_str(&loud))
}

/// Calls a generic closure passed as the argument — plugins can run
/// generic code, and exceptions flow through in both directions.
/// (Arguments never need rooting — see the rooting rules below.)
fn call_with_21_and_double(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let arg = host.make_int(21);
    let result = host.call(args[0], &[arg])?;
    let Some(n) = host.as_int(result) else {
        return Err(host.type_error("callback must return an int"));
    };
    Ok(host.make_int(n * 2))
}

generic_lang_api::export_module![
    ("add", &[2], add),
    ("shout", &[1], shout),
    ("call_with_21_and_double", &[1], call_with_21_and_double),
];
```

Build and place it next to your script (the file name is the module name):

```sh
cargo build --release
cp target/release/libdemo_plugin.dylib demo.dylib     # macOS
# cp target/release/libdemo_plugin.so demo.so         # Linux
# copy target\release\demo_plugin.dll demo.dll        # Windows
```

Use it:

```generic
import "demo";

print(demo.add(19, 23));                     # 42
print(demo.shout("hello plugin"));           # HELLO PLUGIN!

fun answer(n) { return n + 21; }
print(demo.call_with_21_and_double(answer)); # 84

fun raises(n) { throw TypeError("from generic"); }
try {
    demo.call_with_21_and_double(raises);    # exception passes through the plugin...
} catch TypeError as e {
    print("forwarded: " + str(e));           # ...back to the generic caller
}
```

Errors you return are real generic exceptions: `host.type_error(..)` is
caught by `catch TypeError`, and so on for every builtin exception class
(the typed constructors on `Host` create the instance on the spot). A
panicking plugin
function does **not** abort the interpreter — the `export_module!` glue
catches it and throws a base `Exception` with the message `panic: <msg>`.

## Quickstart: C

Everything the Rust wrapper does maps to plain C against
`generic.h`. A module is a table of `FunctionDesc`s and one exported
symbol:

```c
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "generic.h"

/* Each host call can itself fail (EXCEPTION or FATAL); forward any non-OK
 * FfiReturn unchanged, immediately — never relabel or swallow it. */
static FfiReturn throw_new(const HostApi *host, const char *class_name, const char *msg) {
    FfiStr name = {.ptr = (const uint8_t *)class_name, .len = strlen(class_name)};
    FfiStr message = {.ptr = (const uint8_t *)msg, .len = strlen(msg)};
    FfiReturn cls = host->builtin_get(host->ctx, name);
    if (cls.status != GENERIC_FFI_STATUS_OK) {
        return cls;
    }
    FfiReturn exc = host->exception_new(host->ctx, cls.value, message);
    if (exc.status != GENERIC_FFI_STATUS_OK) {
        return exc;
    }
    FfiReturn ret = {.status = GENERIC_FFI_STATUS_EXCEPTION, .value = exc.value};
    return ret;
}

static FfiReturn add(const HostApi *host, const GenericValue *args, size_t nargs) {
    (void)nargs; /* arity-checked by the host */
    int64_t a = 0, b = 0;
    if (!host->int_get(host->ctx, args[0], &a) || !host->int_get(host->ctx, args[1], &b)) {
        return throw_new(host, "TypeError", "add expects two ints");
    }
    FfiReturn ret = {.status = GENERIC_FFI_STATUS_OK, .value = host->int_new(host->ctx, a + b)};
    return ret;
}

static const uint8_t ARITY_2[] = {2};
static const FunctionDesc FUNCTIONS[] = {
    {.name = {.ptr = (const uint8_t *)"add", .len = 3},
     .arities = ARITY_2, .arities_len = 1, .fun = add},
};
static const ModuleDesc DESC = {
    .abi_version = GENERIC_PLUGIN_ABI_VERSION,
    .functions = FUNCTIONS,
    .functions_len = sizeof FUNCTIONS / sizeof FUNCTIONS[0],
};

const ModuleDesc *generic_plugin_init(void) { return &DESC; }
```

```sh
cc -shared -fPIC -I <path-to>/generic-lang-api/include -o cdemo.dylib cdemo.c   # macOS
cc -shared -fPIC -I <path-to>/generic-lang-api/include -o cdemo.so cdemo.c     # Linux
```

(Windows builds — `cl /LD` or `zig cc -shared` — are expected to work but
not yet exercised by the test suite.)

The header is generated from the Rust ABI types by cbindgen and CI-checked;
it is the single source of truth for non-Rust authors.

## Quickstart: C++

C++ uses the same header. Two rules matter: keep `generic_plugin_init`
`extern "C"`, and **never let a C++ exception unwind across an exported
function** (that is undefined behavior through a C ABI frame). Wrap every
body in a helper that converts an escaping `std::exception` into a generic
exception — the C++ analogue of Rust's `catch_unwind`:

```cpp
static FfiReturn guarded(const HostApi *host, const std::function<FfiReturn()> &body) {
    try {
        return body();
    } catch (const std::exception &e) {
        return throw_new(host, "Exception", e.what());
    } catch (...) {
        return throw_new(host, "Exception", "unknown C++ exception");
    }
}
```

Any standard from C++11 up works (the lambda in `guarded` is what sets the
floor; initialize the descriptor structs positionally, or use C-style
designated initializers if you build with C++20 or newer):

```sh
c++ -shared -fPIC -std=c++20 -I <path-to>/generic-lang-api/include \
    -o cppdemo.dylib cppdemo.cpp
```

## Quickstart: Zig

C interop goes through the build system's translate-c step (the `@cImport`
builtin is deprecated since Zig 0.16 and will be removed): a `build.zig`
translates the generated header into a regular module the plugin imports.

`build.zig`:

```zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Translate the plugin header into an importable module.
    const generic_h = b.addTranslateC(.{
        .root_source_file = b.path("<path-to>/generic-lang-api/include/generic.h"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const mod = b.createModule(.{
        .root_source_file = b.path("zig_demo_plugin.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    mod.addImport("generic_h", generic_h.createModule());

    const lib = b.addLibrary(.{
        .name = "zig_demo_plugin",
        .root_module = mod,
        .linkage = .dynamic,
    });
    b.installArtifact(lib);
}
```

The plugin source imports the translated header as a module. Function
pointers come out optional (`?fn`), so unwrap them with `.?`:

```zig
const c = @import("generic_h");

fn add(host: [*c]const c.HostApi, args: [*c]const c.GenericValue, nargs: usize) callconv(.c) c.FfiReturn {
    _ = nargs;
    var a: i64 = 0;
    var b: i64 = 0;
    if (!host.*.int_get.?(host.*.ctx, args[0], &a) or !host.*.int_get.?(host.*.ctx, args[1], &b)) {
        return throwNew(host, "TypeError", "add expects two ints");
    }
    return ok(host.*.int_new.?(host.*.ctx, a + b));
}

export fn generic_plugin_init() callconv(.c) [*c]const c.ModuleDesc {
    return &DESC;
}
```

```sh
zig build -Doptimize=ReleaseSafe
# The dylib lands in zig-out/lib/ with platform naming (on Windows the DLL
# is a runtime artifact and lands in zig-out/bin/ instead); place it next
# to your script under the module name you want to import.
cp zig-out/lib/libzig_demo_plugin.dylib zig_demo_plugin.dylib   # macOS
# cp zig-out/lib/libzig_demo_plugin.so zig_demo_plugin.so       # Linux
# copy zig-out\bin\zig_demo_plugin.dll zig_demo_plugin.dll      # Windows
```

Zig releases break source compatibility routinely — CI tracks the latest
release and expects churn (`callconv(.C)` became `callconv(.c)` in 0.14;
`@cImport` was deprecated in 0.16).

## Worked example plugins

Complete, CI-tested plugins in all four languages live in
[`plugin-examples/`](../plugin-examples) (`rust/`, `c/`, `cpp/`, `zig/`, and
the loader-error `bad/` fixtures). Their `.gen` tests are in
[`test/plugin/`](../test/plugin): the Rust ones run in the normal suite, the
cross-language ones via `make plugin-lang-test`.

## The value model

A `GenericValue` is an **opaque 32-byte handle** to an interpreter value.
Never inspect or fabricate its bytes — everything goes through the host
vtable (Rust: the `Host` methods; C: the `HostApi` function pointers with
`host->ctx` as the first argument). Passing a fabricated or byte-modified
handle is undefined behavior.

`value_kind` / `Host::kind` classifies a value:

| Kind | Notes |
|---|---|
| `Nil`, `Bool`, `Float` | immediates; `bool_get` / `float_get` |
| `Int` | fits in `i64`; `int_get` succeeds |
| `BigInt` | does **not** fit in `i64`; `int_get` returns `false` — fall back to `value_display`/`value_str` |
| `Rational` | inspect via display/str |
| `String` | `string_get` returns borrowed UTF-8 bytes (see lifetime rule below) |
| `List`, `Tuple` | `list_len`/`list_get`, `tuple_len`/`tuple_get`; lists also `list_push`/`list_set` |
| `Dict`, `Set` | `dict_get/set/contains`, `set_add/contains`, lengths — these **re-enter** (user `__hash__`/`__eq__` may run) |
| `Range` | inspect via display, or drive its iterator |
| `Instance` | plain object: fields via `attr_get`/`attr_set`/`attr_has`, methods via `invoke_method` |
| `Class`, `Function` | callable with `call_value` (calling a class instantiates it) |
| `Exception` | an exception instance |
| `Generator`, `Iterator` | drive with `invoke_method("__next__")` until `StopIteration` |
| `StopIteration` | the exhausted-iterator sentinel value |
| `Module` | a module object |
| `Other` | VM-internal; a plugin should never meaningfully receive one |

There is no dict/set enumeration callback: iterate any
container by invoking its `__iter__`/`__next__` protocol.

Constructors: `nil_new`, `bool_new`, `int_new`, `float_new`, `string_new`
(`ValueError` on invalid UTF-8), `list_new`/`list_push`. Strings are
interned by the host; creating the same string twice yields the same
handle.

**Callback return conventions**, decided solely by whether the payload
forces an out-parameter:

- A payload the caller must receive as something other than a
  `GenericValue` — a raw machine scalar (`bool`, `i64`, `f64`, `usize`) or
  a borrowed `FfiStr` — can't ride in an `FfiReturn` (whose payload is a
  `GenericValue`), so those callbacks — `bool_get`, `int_get`,
  `float_get`, the `*_len` family, and `string_get` — take an
  out-parameter and return a plain `bool`: `true` on success, `false` on
  the sole "wrong kind" failure, no exception.
- Everything else (payload is a `GenericValue`, or there is no payload —
  `list_get`, `builtin_get`, `attr_*`, `list_push`, `list_set`,
  `string_new`, the re-entering group, …) returns `FfiReturn`, carrying a
  real exception instance on failure whose class and message mirror what
  the equivalent generic operation would throw (e.g. `list_get`:
  `TypeError` for a non-list, `IndexError` out of bounds).
- Infallible callbacks return their value directly (`value_kind`,
  `nil_new`, `int_new`, `list_new`, `value_display`, …).

## Calling generic code, and exceptions

The re-entering callbacks run generic bytecode: `call_value`,
`invoke_method`, `value_str` (honors `__str__`), `value_truthy`
(`__bool__`), `value_equals` (`__eq__`), `value_hash` (`__hash__`), and the
dict/set operations. In the Rust wrapper these are the `&mut self` methods
on `Host`.

Each returns an `FfiReturn` (Rust: `Result<_, PluginError>`). The status
is a three-state discriminator (`GENERIC_FFI_STATUS_*` in C, `FfiStatus`
in Rust), and `value` is always present:

- `OK` — success, `value` is the result.
- `EXCEPTION` — generic code raised an exception; `value` is the exception
  **instance itself**. You can **handle it** (check its class with
  `is_instance`, read its message with `value_str`) or **rethrow it**
  (return it under the same status; in Rust, `?` does this). A rethrown
  exception re-raises with full identity: its exact class — user-defined
  subclasses included — its fields, and its original stack trace.
- `FATAL` — a fatal interpreter error passed through your call. **Forward
  it unchanged, immediately.** Never swallow it, never fabricate it.
  (Rust: `?` forwards it automatically; you will normally never see it.)
  Any other status value is treated as a plugin bug.

Throwing your own exception means returning an instance under the
`EXCEPTION` status. Exception classes are ordinary values: look one up
with `builtin_get("TypeError")` (any builtin exception class name — or the
base `"Exception"`), create the instance with `exception_new(class,
message)`, and return it — exactly what the `throw_new` helper in the
[C quickstart](#quickstart-c) does.

(`exception_new` sets the message directly, bypassing the class's
`__init__` — exactly like the interpreter's own throw; call the class via
`call_value` if you need full construction semantics. It also works with
user-defined exception classes your plugin received.) Rust authors use the
typed constructors on `Host` — `host.key_error("...")` and friends — which
do the lookup and creation in one call; `PluginError` itself is
`Exception(instance)` or `Fatal`, mirroring the wire statuses exactly.

Catching works like a generic `catch` clause, because it uses the same
subclass check the interpreter uses:

```c
FfiReturn type_error = host->builtin_get(host->ctx, name_str("TypeError"));
FfiReturn is = host->is_instance(host->ctx, exc, type_error.value);
bool matches = false;
host->bool_get(host->ctx, is.value, &matches);
if (matches) { /* handle it */ }
```

(`is_instance` returns a bool *value* — subclass-aware, value-type proxy
classes included: exactly the `isinstance` builtin.)

## The rooting contract

This is the one rule set you must internalize. The interpreter's GC can run
**only** while generic bytecode executes — i.e. only inside the
re-entering callbacks listed above. That gives three rules:

1. **Straight-line code needs no rooting.** If your function only inspects
   arguments and constructs values (no re-entering callback), every handle
   you hold stays valid for the whole call. Allocation never triggers
   collection in this VM.
2. **Across a re-entering callback, root every value you still hold.**
   Values you created (or extracted from containers) are not otherwise
   reachable by the GC; an unrooted handle used after a re-entering call is
   a bug (the interpreter detects it as a deterministic panic — memory-safe,
   but fatal). Root with `root(value)` / release with `unroot(n)`; in Rust
   prefer the RAII guard: `let keep = host.rooted(v);`. All roots are
   released automatically when your function returns. Your *arguments* are
   always safe — the host keeps the originals alive — and values you pass
   *into* a re-entering callback are rooted by the host for that call.
3. **Re-fetch borrowed strings after re-entering.** The `(ptr, len)` from
   `string_get` is valid only until the next re-entering callback. In Rust
   this rule is enforced at compile time: `as_str` borrows the `Host`, and
   the re-entering methods take `&mut self`, so holding the `&str` across a
   call does not compile — copy it out (`.to_owned()`) first. In C it is on
   you.

## Arity

`FunctionDesc.arities` (Rust: the `&[u8]` in `export_module!`) lists every
accepted argument count — `&[2]` for exactly two, `&[0, 1]` for optional,
up to 255. The host checks arity *before* calling you; a mismatch is an
ordinary `TypeError` in generic code and your function never runs.

## ABI stability

`GENERIC_PLUGIN_ABI_VERSION` (currently 1) is checked at load; a mismatch
is a clean `ImportError` naming both versions. The `generic-lang-api` crate
is versioned independently of the interpreter to track ABI stability —
build against the version matching the interpreter you target.
