# Values and expressions

[← Guide index](README.md)

## Comments and statements

Line comments start with `#` and run to end of line - there are no block
comments. Statements end with `;`, and `{ }` groups statements into a block
that introduces a new scope.

```generic
# a comment
print("hi");
```

## Variables: `var` and `const`

`var` declares a mutable binding, `const` an immutable one.

```generic-error
var a = 5;
a = 6;              # ok

const pi = 3.14159;
pi = 3;             # error: ConstReassignmentError
```

A `const` can never be assigned after its declaration - even one declared
without a value cannot be "late-initialized". Assigning a *local* `const` is
a compile error; assigning a *global* one raises a `ConstReassignmentError`
at runtime. Blocks nest and inner declarations shadow outer ones.

## The built-in types

### nil and booleans

```text
nil
true
false
```

### Numbers

Generic has four numeric kinds that interoperate by value:

| Kind | Literal | Notes |
|---|---|---|
| Integer | `123`, `0` | 64-bit; **promotes to a bigint automatically** on overflow |
| Big integer | `9223372036854775808` | arbitrary precision, transparent |
| Float | `123.456`, `0.001` | IEEE-754 double; a bare trailing/leading dot (`1.`, `.5`) is an error |
| Rational | `5:3` | exact fractions, written `numerator:denominator` |

```generic
print(9223372036854775808);   # bigint, no overflow
print(5:3 + 2:7);             # 41:21
print(1 == 1.0);              # true - numeric kinds compare by value
```

Integer division with `/` produces a **rational** (`8 / 2` → `4:1`); use
`//` for floor division to an integer/float. `0.0 / 0.0` is NaN, and
`nan != nan` is `true`.

### Strings

Strings are double-quoted and may span multiple lines literally.

```generic
var s = "line one
line two";
```

**f-strings** interpolate expressions with `${ ... }`:

```generic
var name = "Bob";
print(f"Hi ${name}, 1 + 1 = ${1 + 1}");   # Hi Bob, 1 + 1 = 2
```

**t-strings** (`t"..."`) are *template* values - they do not eagerly build a
string. A `Template` exposes its literal `.strings()` and its
`.interpolations()` (each an `Interpolation` with `.value()` and
`.expression()`), which lets you post-process interpolated data before
rendering.

String methods include `replace(a, b)`, `find(sub)`, `contains(sub)`,
`split([sep])`, `join(parts)`, `strip()`, `startswith(prefix)`,
`endswith(suffix)`, `removeprefix(prefix)`, `removesuffix(suffix)`, `bytes()`,
`chars()`, `clusters()`, and the indexed `get_byte`/`get_char`/`get_cluster`.

```generic
print("a,b,c".split(","));      # [a, b, c]
print("  a  b  ".split());      # [a, b]   (no argument: split on whitespace runs)
print("-".join(["x", "y"]));    # x-y
print("  hi  ".strip());        # hi
print("filename.gen".removesuffix(".gen"));  # filename
```

`split()` with no argument splits on runs of whitespace and drops empty
fields; given a separator it keeps them. `join(parts)` requires every element
to be a string. A string can also be repeated with `*`: `"ab" * 3` is
`"ababab"`, and a count `<= 0` yields `""`.

Strings order with `<`, `<=`, `>`, `>=` lexicographically by NFC-normalized
code point, matching how `==` already normalizes:

```generic
print("apple" < "banana");   # true
print("Zebra" < "apple");    # true  (uppercase code points sort first)
```

### Lists, tuples

```generic
var xs = [1, 2, "three"];   # lists: mutable, heterogeneous
xs[0] = 10;                 # index assignment
xs.append(4);
print(xs[3]);               # 4  (indices must be in range; no negative indexing)

var t = (1, 2, 3);          # tuples: immutable
print(t + (4, 5));          # (1, 2, 3, 4, 5)
```

List methods: `append(x)`, `pop([i])`, `insert(i, x)`, `contains(x)`,
`reverse()`, `extend(iterable)`, `clear()`, `copy()`, plus `[]`, `+`, `len`,
and iteration. Tuples, being immutable, offer only the non-mutating ones:
`contains(x)`, `reversed()` (returns a new tuple), `[]`, `+`, `len`, and
iteration.

Lists and tuples compare structurally. `==` and `!=` check the elements
pairwise (using each element's own `__eq__`), and `<`, `<=`, `>`, `>=` order
them lexicographically. A shorter sequence that is a prefix of a longer one
sorts first. Comparing a list to a tuple, or ordering elements that are not
mutually comparable, raises a `TypeError`.

```generic
print([1, 2, 3] == [1, 2, 3]);   # true
print([1, 2] < [1, 2, 3]);       # true  (prefix sorts first)
print((1, 3) > (1, 2));          # true
```

### Dicts and sets

Both use braces, so the empty forms are disambiguated: **`{:}` is an empty
dict**, **`{}` is an empty set**.

```generic
var d = { "name": "John", "age": 30 };   # dict
print(d["name"]);                         # John
var empty_dict = {:};

var s = {1, 2, 3};                        # set
var empty_set = {};
print(2 in s);                            # true
```

Dict methods: `contains(k)`, `pop(k)`, `get(k, default)`, `keys()`,
`values()`, `items()`, `clear()`, `[]`/`[]=`, `len`. Set methods: `insert(x)`,
`remove(x)`, `contains(x)`, `clear()`, `len`, plus the set-algebra operators
`|` (union), `&` (intersection), `-` (difference), and `^` (symmetric
difference).

Dicts and sets are unordered: iteration and display order follow the hash,
not insertion. Both are iterable. A `foreach` over a dict walks its keys, and
`keys()`, `values()`, and `items()` return iterators over the keys, the
values, and `(key, value)` tuples:

```generic
var d = { "a": 1, "b": 2 };
foreach (var k in d) { print(k); }              # keys, in hash order
foreach (var pair in d.items()) { print(pair); }
print(d.get("a", 0));                            # 1
print(d.get("z", 0));                            # 0  (default when absent)

var a = { 1, 2, 3 };
var b = { 2, 3, 4 };
print(a & b);   # intersection
print(a | b);   # union
print(a - b);   # difference
```

Dicts and sets compare by contents: two dicts are `==` when they hold the same
keys mapping to equal values, and two sets when they hold the same elements,
regardless of order. There is no ordering (`<`, `<=`, `>`, `>=`) on dicts or
sets.

### Ranges

`a..<b` is exclusive of `b`; `a..=b` is inclusive. A range is iterable,
`in`-testable, and has a length; it counts down when `a > b`.

```generic
foreach (var i in 1..<5) { print(i); }    # 1 2 3 4
print(3 in 0..=3);                        # true
print(1..=10);                            # 1..<11  (inclusive prints as exclusive-of-next)
```

### Constructors

Each type also has a constructor usable as a function and as an
`isinstance` target: `List`, `Tuple`, `Set`, `Dict`, `Range`, `String`,
`Integer`, `Float`, `Rational`, `Bool`, `Template`, `Interpolation`, and the
exception classes.

## Operators

| Group | Operators |
|---|---|
| Arithmetic | `+` `-` `*` `/` (→ rational) `//` (floor) `%` `**` (power) |
| Bitwise | `&` `\|` `^` |
| Comparison | `<` `<=` `>` `>=` `==` `!=` |
| Logical | `and` `or` (word operators, short-circuiting), `!` (negation) |
| Membership | `x in collection` |
| Identity | `x is y` |
| Conditional | `cond ? a : b` |
| Access | `.` `[]` |
| Nil-safe | `?.` `?[]` (short-circuit the full expression to `nil` on a `nil` receiver) |

There is no `&&`/`||` and no `not` - use `and`/`or`/`!`. The logical
operators short-circuit and return one of their operands (not a coerced
bool):

```generic
print(true and 1);    # 1
print(false or 2);    # 2
print(!true);         # false
print(true ? "y" : "n");  # y
```

`is` compares **identity**, not equality: heap values (strings, containers,
instances, big integers, …) match only when they are the same object, while
immediates (`nil`, booleans, small integers, floats) compare by value.
Equal string literals are interned to the same object, so `"a" is "a"` is
true.

```generic
var xs = [1, 2];
var ys = xs;
print(xs is ys);       # true - same object
print(xs is [1, 2]);   # false - distinct objects
print(1 is 1);         # true - immediates compare by value
```

Compound assignment is available for the common operators: `+=`, `-=`, `*=`,
`/=`, `%=`, `&=`, `|=`, `^=`, and works on variables, fields, and indexed
targets (`xs[i] += 1`).

## nil-safe access: `?.` and `?[]`

`?.` and `?[]` are nil-safe variants of `.` and `[]`. When the receiver is
`nil`, they short-circuit the entire following access chain and yield `nil`
without evaluating any of the skipped accesses, calls, or subscripts (including
their arguments).

```generic
var x = nil;
print(x?.foo);            # nil - GetProperty not attempted
print(x?.foo.bar);        # nil - entire chain skipped
print(x?[0]);             # nil - index not evaluated

class Point {
    __init__(x, y) {
        this.x = x; this.y = y;
    }
    magnitude() {
        return (this.x**2 + this.y**2)**0.5;
    }
}
var p = Point(3, 4);
print(p?.x);              # 3 - non-nil receiver, behaves like .x
print(p?.magnitude());    # 5.0 - non-nil receiver, method called
```

Short-circuiting triggers **only on `nil`**, not on other falsey values, so
`false?.x` falls through and raises a `TypeError` (the receiver is not an
instance). A single `?.` covers the whole chain to its right until a
lower-precedence operator ends it:

```generic
print(nil?.a.b.c);        # nil - .b and .c skipped
try {
    print(nil?.a + 1);
} catch TypeError as e {
    print(str(e));        # Operands must be two numbers, strings or support `__add__`. Got: [nil, 1]
}
```

`?.`/`?[]` are not overloadable - they short-circuit before any method lookup
or property dispatch occurs. To make a chained access conditional on each
step, repeat the operator: `a?.b?.c?.d`.

Assignment through `?.` or `?[]` (`a?.b = c`, `a?[i] = x`, `a?.b += 1`) is a
compile-time error.

## Truthiness

`nil` and `false` are falsey. Empty collections (`[]`, `{}`, `{:}`, an empty
range) are falsey; non-empty ones are truthy. Instances are truthy unless the
class defines [`__bool__`](classes.md#operator-overloading).

Next: [Control flow →](control-flow.md)
