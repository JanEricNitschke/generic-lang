# Modules and the standard library

[← Guide index](README.md)

## Imports

`import` brings another module into scope. The argument is a string: a bare
name resolves to a standard-library module, otherwise it is a path to a
`.gen` file relative to the importing script.

```text
import "math";                 # stdlib module, bound as `math`
import "utils/helpers.gen";    # file module, bound under its stem: `helpers`
```

Three forms:

```generic
import "math";                 # bind the whole module as `math`
import "math" as m;            # bind under an alias
from "math" import sqrt;       # pull selected names into the current scope
from "testing" import assert_equal, assert_true;   # several, comma-separated
```

Members are accessed with `.`:

```generic
import "math";
print(math.sqrt(9));           # 3.0
```

Imports may appear inside a function ("local import"), in which case the
binding disappears when the function returns. A module's `const` bindings stay
`const` across the boundary. The special variable `__name__` is `"<script>"`
in the entry file and the module's name (its file stem) inside an imported
module.

### Resolution order

For `import "name";` the interpreter tries, in order:

1. a user file `name.gen` next to the importing script,
2. a **native plugin** - a shared library `name.<ext>` in that directory (see
   below),
3. a bundled generic-source stdlib module,
4. a bundled Rust-native stdlib module.

Because plugins resolve before the stdlib, a plugin can shadow a stdlib
module of the same name.

## Built-in functions

These are always in scope (no import needed):

| Function | Purpose |
|---|---|
| `print(x)`, `print(x, end)` | Print `x`; the optional `end` replaces the trailing newline. |
| `str(x)`, `int(x)`, `float(x)` | Conversions. `is_int(x)` tests integer-ness. |
| `type(x)` | A type description, e.g. `<type int>`, `<type Set>`, `<type Foo>`. |
| `len(x)` | Length (via `__len__`). |
| `isinstance(x, C)`, `issubclass(A, B)` | Inheritance-aware type checks. |
| `iter(x)`, `next(x)` | The iterator protocol. |
| `all(iter)`, `any(iter)` | Boolean reductions. |
| `enumerate(iter)` | A generator of `(index, item)` tuples. |
| `assert(x)` | Raise `AssertionError` if `x` is falsey. |
| `getattr/setattr/hasattr/delattr(obj, name, …)` | Reflective attribute access on instances, classes, and modules. |
| `clock()`, `sleep(s)`, `input(prompt)` | Time, delay, read a line. |
| `rng(low, high)` | Random integer in `[low, high)`; bounds must be 64-bit integers. |
| `eval(src[, mod][, locals])` | Evaluate `src` as one expression and return its value. |
| `exec(src[, mod][, locals])` | Run `src` as statements; returns `nil`. |
| `Module(name[, init])` | A fresh anonymous module namespace, optionally initialized from a dict. |

```generic
foreach (var pair in enumerate(["a", "b"])) { print(pair); }   # (0, a) then (1, b)
```

### eval and exec

`eval` and `exec` compile and run code at runtime. Names resolve in the
caller's module by default, or in `mod` when given - reads, writes, and
`var` definitions all go there. A `locals` dict injects its entries as
local variables of the injected code (they shadow globals; writes to
them stay inside the injected code). The caller's own locals are never
visible (locals are compile-time stack slots). A compile failure raises
a catchable `SyntaxError`; runtime errors propagate as usual.

```generic
var base = 10;
print(eval("base + 1"));            # 11
print(eval("a * b", {"a": 6, "b": 7}));  # 42

var ns = Module("sandbox", {"start": 5});
exec("var doubled = start * 2;", ns);
print(ns.doubled);                  # 10
print(isinstance(ns, Module));      # true
```

## The standard library

The bundled modules:

- **`math`** - numeric helpers (`math.sqrt(x)`, `math.pi`).
- **`dataclasses`** - `@dataclass` generates `__init__`, `__str__`,
  `__eq__`, and `__hash__` from a class's
  [class variables](classes.md#class-variables) in declaration order
  (methods the class defines itself win). The field list is frozen at
  decoration time into the `__dataclass_fields__` class variable:
  `_`-prefixed names are skipped, a dataclass base's fields come first,
  and variables added after decoration (or by undecorated subclasses)
  stay ordinary class variables. `field(factory)` wraps a default so
  each instance gets a fresh value.
- **`keyword`** - the language's reserved words: `keyword.kwlist` (a
  list of strings, `true`/`false`/`nil`/`StopIteration` included) and
  `keyword.iskeyword(name)`.
- **`builtins`** - a snapshot of the built-in functions namespace, taken
  when the module is imported: `builtins.len` is the real `len` even if
  a global shadows it. Builtins are constants - assignment can shadow
  them only through a declaration (`var len = ...;`), and the module's
  entries are constants too.
- **`string`** - named constants for the control characters the
  language has no escape syntax for: `string.newline`,
  `string.carriage_return`, `string.tab`, `string.quote`,
  `string.backslash`, `string.null`. Use them via interpolation, e.g.
  `f"a${string.newline}b"`.
- **`functools`** - tools for callables: `reduce(f, iterable)` /
  `reduce(f, iterable, initial)` folds an iterable with `f(acc, item)`;
  `partial(f, args...)` returns a callable with the leading arguments
  bound; `cmp_to_key(cmp)` turns a two-argument comparator into a key
  function for `sorted` / `list.sort`.
- **`testing`** - assertion helpers for the test runner: `assert_equal`,
  `assert_not_equal`, `assert_true`, `assert_false`, `assert_nil`,
  `assert_not_nil`, `assert_throws`. Each raises `AssertionError` with a
  descriptive message on failure. See [Testing](testing.md).
- **`zen`** - a tiny example module.

```generic
import "testing";
testing.assert_equal(2 + 2, 4, "math works");
```

## Native plugins

A module can also be a native shared library (`.so`/`.dylib`/`.dll`) written
in Rust, C, C++, or Zig, loaded by the same `import` statement. This is how
you extend generic with fast native code or bind existing C libraries. See
the **[plugin authoring guide](../plugin-authors.md)** and the worked
examples in [`plugin-examples/`](../../plugin-examples).

Next: [Testing →](testing.md)
