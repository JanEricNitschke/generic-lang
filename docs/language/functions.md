# Functions

[← Guide index](README.md)

## Declarations

Functions are declared with `fun`. Calling with the wrong number of
arguments is an error. `return` without a value returns `nil`.

```generic
fun greet(name) {
    return f"hello, ${name}";
}
print(greet("world"));
```

## Default parameters

A parameter may declare a default with `= expression`, making it optional.
Any parameter with a default must come after all required ones (a required
parameter cannot follow an optional one). Calling with too few or too many
arguments reports the accepted range.

```generic
fun greet(name, greeting="Hello") {
    return f"${greeting}, ${name}";
}
print(greet("world"));         # Hello, world
print(greet("world", "Hi"));   # Hi, world
```

Defaults are evaluated **once, at definition time**, in the scope enclosing
the function - not on each call. A default therefore captures the value in
force when the function is defined, and a mutable default is shared across
calls:

```generic
var base = 100;
fun offset(x, from=base) { return x + from; }
base = 999;
print(offset(1));   # 101 - `from` captured 100 at definition

fun collect(item, into=[]) {
    into.append(item);
    return into;
}
print(collect(1));  # [1]
print(collect(2));  # [1, 2] - the same list, evaluated once
```

## Rest parameters (`*rest`)

A final parameter written `*name` collects any surplus positional arguments
into a tuple. It must be the last parameter, and it is not counted toward the
required arguments, so a function can accept any number of arguments beyond
its fixed ones.

```generic
fun tail(first, *rest) {
    return rest;
}
print(tail(1));          # ()
print(tail(1, 2, 3));    # (2, 3)

fun total(*numbers) {
    var sum = 0;
    foreach (var n in numbers) {
        sum = sum + n;
    }
    return sum;
}
print(total(1, 2, 3, 4));   # 10
```

Defaults may sit between the fixed and rest parameters: the arguments fill the
required parameters, then the optional ones (falling back to their defaults),
and only the remainder is collected.

## Argument unpacking (`*expr`)

At a call site, `*expr` spreads an iterable into individual positional
arguments. It works with any callable, and mixes freely with plain arguments
and with several spreads:

```generic
fun add(a, b, c) {
    return a + b + c;
}
var pair = [2, 3];
print(add(1, *pair));        # 6
print(add(*[1], 2, *[3]));   # 6

# Any iterable spreads - lists, tuples, generators - and it pairs naturally
# with a rest parameter to forward arguments.
fun forward(*args) {
    return add(*args);
}
print(forward(1, 2, 3));     # 6
```

An unpacking call is limited to 255 arguments after expansion, and spreading a
non-iterable raises `TypeError`.

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

Lambdas are ordinary values - pass them to higher-order functions, store them
in variables, or use them as decorators.

## Decorators

A decorator is any callable applied to a function (or
[class](classes.md#class-decorators)) declaration with `@` on the
line(s) above it. The declared function is replaced by the decorator's return
value. Decorators stack, and a decorator can itself be a call that returns a
decorator ("decorator factory") or a lambda.

```generic
fun double(f) {
    return ->(a, b) f(a, b) * 2;
}

@double
fun sum(a, b) { return a + b; }

print(sum(3, 4));   # 14  - (3 + 4) * 2
```

```text
@with_args(5, 2)                 # a decorator factory: with_args(...) returns a decorator
fun scaled(a, b) { return a + b; }

@->(f) ->(a, b) f(a, b) + 1      # a lambda decorator
fun inc_sum(a, b) { return a + b; }
```

## Generators

A generator is declared with `gen` and produces values lazily with `yield`.
Calling it returns a generator object, which is itself an iterator - drive it
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
(`__iter__` / `__next__`) - see [Classes](classes.md#operator-overloading).

Next: [Classes →](classes.md)
