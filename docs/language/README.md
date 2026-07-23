# The Generic Language Guide

Generic is a small, dynamically-typed, class-based language with closures,
generators, operator overloading, exceptions, and a module system - a
descendant of [Lox](https://craftinginterpreters.com). Programs run on a
bytecode VM.

Run a file, or start a REPL with no argument:

```sh
generic program.gen     # run a file
generic                 # REPL
generic --test file.gen # run its test_ functions (see Testing)
```

## Contents

1. [Values and expressions](values.md) - variables, the built-in types and
   their literals, operators, truthiness.
2. [Control flow](control-flow.md) - `if`/`unless`, `while`/`until`, `for`,
   `foreach`, `switch`, `break`/`continue`, loop labels, the `?:` operator.
3. [Functions](functions.md) - declarations, closures, function literals,
   decorators, generators.
4. [Classes](classes.md) - fields, methods, inheritance, `super`, operator
   overloading, `isinstance`/`issubclass`.
5. [Exceptions](exceptions.md) - the class hierarchy, `try`/`catch`/`else`,
   `throw`, custom exceptions.
6. [Modules and the standard library](modules.md) - `import`, the builtin
   functions, the bundled stdlib modules, and native plugins.
7. [Testing](testing.md) - the built-in `--test` runner and the `testing`
   module.

## A quick tour

```generic
# Comments start with '#'. Statements end with ';'.
var name = "world";
print(f"hello, ${name}!");            # f-strings interpolate with ${...}

fun fib(n) {
    if n < 2 { return n; }            # parentheses around conditions optional
    return fib(n - 1) + fib(n - 2);
}
print(fib(10));                       # 55

class Point {
    __init__(x, y) { this.x = x; this.y = y; }
    __str__() { return f"Point(${this.x}, ${this.y})"; }
}
print(str(Point(3, 4)));              # Point(3, 4)

foreach (var n in 1..=3) {            # ranges: ..< exclusive, ..= inclusive
    print(n * n);                     # 1, 4, 9
}

gen squares(xs) {                     # generators use `gen` + `yield`
    foreach (var x in xs) { yield x * x; }
}
foreach (var s in squares([1, 2, 3])) { print(s); }   # 1, 4, 9
```
