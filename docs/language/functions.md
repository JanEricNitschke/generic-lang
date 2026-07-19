# Functions

[← Guide index](README.md)

## Declarations

Functions are declared with `fun`. Parameters are fixed-arity — calling with
the wrong number of arguments is an error — and there are no default
parameters or varargs for user functions. `return` without a value returns
`nil`.

```generic
fun greet(name) {
    return f"hello, ${name}";
}
print(greet("world"));
```

## Closures

Nested functions capture their enclosing variables by reference, so state
persists across calls:

```generic
fun make_counter() {
    var n = 0;
    fun inc() { n = n + 1; return n; }
    return inc;
}
var c = make_counter();
print(c());   # 1
print(c());   # 2
```

## Function literals (lambdas)

An anonymous function is written `->(params) body`. The body is either a
single expression (implicitly returned) or a block:

```generic
const add    = ->(a, b) a + b;          # expression body
const square = ->(a) { return a * a; }; # block body
print(add(2, 3));    # 5
print(square(4));    # 16
```

Lambdas are ordinary values — pass them to higher-order functions, store them
in variables, or use them as decorators.

## Decorators

A decorator is any callable applied to a function declaration with `@` on the
line(s) above it. The declared function is replaced by the decorator's return
value. Decorators stack, and a decorator can itself be a call that returns a
decorator ("decorator factory") or a lambda.

```generic
fun double(f) {
    return ->(a, b) f(a, b) * 2;
}

@double
fun sum(a, b) { return a + b; }

print(sum(3, 4));   # 14  — (3 + 4) * 2
```

```text
@with_args(5, 2)                 # a decorator factory: with_args(...) returns a decorator
fun scaled(a, b) { return a + b; }

@->(f) ->(a, b) f(a, b) + 1      # a lambda decorator
fun inc_sum(a, b) { return a + b; }
```

## Generators

A generator is declared with `gen` and produces values lazily with `yield`.
Calling it returns a generator object, which is itself an iterator — drive it
with `foreach`, or manually with `next(...)`.

```generic
gen squares(xs) {
    foreach (var x in xs) {
        yield x * x;
    }
}
foreach (var s in squares([1, 2, 3])) { print(s); }   # 1, 4, 9
```

`yield` is also an expression: the value passed to `gen.send(v)` becomes the
result of the `yield` that the generator is suspended on. Generators support
`next(g)`, `g.send(v)`, `g.close()`, and `g.raise(exc)`. When exhausted a
generator yields the `StopIteration` sentinel.

```generic
gen counter() {
    var i = 0;
    while (true) {
        var received = yield i;   # `received` is whatever send() passed
        i = i + 1;
    }
}
var g = counter();
print(next(g));       # 0
print(g.send(nil));   # 1
```

Any class can be made iterable by implementing the iterator protocol
(`__iter__` / `__next__`) — see [Classes](classes.md#operator-overloading).

Next: [Classes →](classes.md)
