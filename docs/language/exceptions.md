# Exceptions

[← Guide index](README.md)

## The exception hierarchy

Every exception derives from the base class `Exception`. The built-in
subclasses are:

```
Exception
├── TypeError
├── ValueError
├── NameError
├── ConstReassignmentError
├── AttributeError
├── ImportError
├── AssertionError
├── IoError
├── KeyError
├── IndexError
└── RuntimeError
```

Runtime errors raised by the interpreter (a bad index, a missing name, a
failed `assert`, …) are instances of the matching class, so they can be
caught like any other exception.

## Raising: `throw`

`throw` takes an exception **instance**. Construct one by calling its class
with an optional message:

```generic-error
throw ValueError("expected a positive number");
```

Throwing something that is not an exception instance is itself an error.

## Handling: `try` / `catch` / `else`

```text
try {
    risky();
} catch KeyError as e {
    print(f"missing key: ${e}");
} else {
    print("no exception was raised");
}
```

- `catch <Class> as e` binds the caught instance to `e`; the `as e` is
  optional.
- A single `catch` can match several classes:
  `catch (TypeError, ValueError) as e { … }`.
- A `try` may have several `catch` blocks; the first matching class wins, and
  matching is subclass-aware (a `catch Exception` catches everything).
- The optional `else` block runs only when the `try` body completed without
  throwing. A `try` needs at least one `catch` - `else` alone is a compile
  error.

```generic
try {
    parse(input);
} catch (TypeError, ValueError) as e {
    print(f"bad input: ${e}");
} catch Exception as e {
    print(f"unexpected: ${e}");
}
```

> `finally` is a reserved word but is **not yet implemented**.

## The exception instance

`Exception(message)` takes an optional message. `e.message()` returns it, and
`str(e)` (or printing `e`) renders the message:

```generic
var e = ValueError("msg");
print(e.message());   # msg
print(str(e));        # msg
```

`e.stack_trace()` returns the traceback recorded where the exception was
thrown, or `nil` for an instance that was never thrown.

## Custom exceptions

Subclass `Exception` (or any of its subclasses). Custom exceptions are caught
by handlers for themselves or any ancestor:

```generic
class ConfigError < Exception {}

try {
    throw ConfigError("missing field");
} catch Exception as e {          # the base class catches the subclass
    print(f"caught: ${e}");       # caught: missing field
}
```

Next: [Modules and the standard library →](modules.md)
