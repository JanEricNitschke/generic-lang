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
2. a **native plugin** — a shared library `name.<ext>` in that directory (see
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
| `getattr/setattr/hasattr/delattr(obj, name, …)` | Reflective field access. |
| `clock()`, `sleep(s)`, `input(prompt)` | Time, delay, read a line. |
| `rng(low, high)` | Random integer in `[low, high)`; bounds must be 64-bit integers. |

```generic
foreach (var pair in enumerate(["a", "b"])) { print(pair); }   # (0, a) then (1, b)
```

## The standard library

The bundled modules:

- **`math`** — numeric helpers (currently just `math.sqrt(x)`).
- **`testing`** — assertion helpers for the test runner: `assert_equal`,
  `assert_not_equal`, `assert_true`, `assert_false`, `assert_nil`,
  `assert_not_nil`, `assert_throws`. Each raises `AssertionError` with a
  descriptive message on failure. See [Testing](testing.md).
- **`zen`** — a tiny example module.

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
