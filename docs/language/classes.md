# Classes

[← Guide index](README.md)

## Declaring a class

Methods are written inside the class body without `fun`. The constructor is
`__init__`, and the instance is referred to as **`this`**. Fields are created
by assignment; there is no separate field declaration.

```generic
class Point {
    __init__(x, y) {
        this.x = x;
        this.y = y;
    }
    magnitude() {
        return (this.x ** 2 + this.y ** 2) ** 0.5;
    }
}

var p = Point(3, 4);
print(p.x);            # 3
print(p.magnitude());  # 5.0
```

Fields can also be read and written from outside (`p.x = 10`), and the
reflection builtins `getattr`, `setattr`, `hasattr`, and `delattr` work on
instances by name.

## Inheritance and `super`

A class inherits from another with `class Sub < Base`. Single inheritance
only; the base must be a class (including the built-in native classes like
`List`). `super.method(...)` invokes the base implementation.

```generic
class Animal {
    __init__(name) { this.name = name; }
    speak() { return f"${this.name} makes a sound"; }
}

class Dog < Animal {
    speak() { return f"${super.speak()} (woof)"; }
}

print(Dog("Rex").speak());   # Rex makes a sound (woof)
```

## `isinstance` and `issubclass`

Both respect the inheritance chain:

```text
var d = Dog("Rex");
print(isinstance(d, Animal));    # true
print(issubclass(Dog, Animal));  # true
print(type(d));                  # <type Dog>
```

## Operator overloading

Classes customize built-in operations by defining dunder methods. Binary
operators dispatch on the **left** operand only: `a == b` consults `a`'s
`__eq__` and never `b`'s, and the same goes for the arithmetic, bitwise,
and comparison hooks. Without `__eq__`, equality falls back to identity.

```generic
class Vec {
    __init__(x) { this.x = x; }
    __add__(o) { return Vec(this.x + o.x); }
    __eq__(o)  { return this.x == o.x; }
    __str__()  { return f"Vec(${this.x})"; }
    __len__()  { return this.x; }
}

print(str(Vec(1) + Vec(2)));   # Vec(3)
print(Vec(3) == Vec(3));       # true
print(len(Vec(5)));            # 5
```

The supported hooks:

| Category | Methods |
|---|---|
| Lifecycle / representation | `__init__`, `__str__`, `__bool__`, `__len__`, `__hash__`, `__eq__` |
| Arithmetic | `__add__`, `__sub__`, `__mul__`, `__div__`, `__mod__`, `__pow__`, `__floor_div__` |
| Bitwise | `__bitand__`, `__bitor__`, `__bitxor__` |
| Comparison | `__lt__`, `__le__`, `__gt__`, `__ge__` |
| Indexing | `__getitem__`, `__setitem__` |
| Membership (`in`) | `contains` |
| Call (`obj(...)`) | `__call__` |
| Iteration | `__iter__`, `__next__` |

The membership hook is named `contains`, not `__contains__`.

The nil-safe operators `?.` and `?[]` (see [Values and expressions](values.md#nil-safe-access--and-))
short-circuit before method dispatch, so they have no dunder hook. When the
receiver is non-`nil`, they fall through to the regular `__getitem__` /
property-access path.

To make a class iterable, return an iterator from `__iter__` and advance it with
`__next__`, returning the `StopIteration` sentinel when exhausted:

```generic
class Countdown {
    __init__(n) { this.n = n; }
    __iter__() { return this; }
    __next__() {
        if this.n <= 0 { return StopIteration; }
        this.n = this.n - 1;
        return this.n + 1;
    }
}
foreach (var i in Countdown(3)) { print(i); }   # 3, 2, 1
```

Individual dunders can be overridden per instance by assigning a lambda
(`obj.__str__ = ->() "custom";`).

Next: [Exceptions →](exceptions.md)
