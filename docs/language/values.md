# Values and expressions

[← Guide index](README.md)

## Comments and statements

Line comments start with `#` and run to end of line — there are no block
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

A `const` can never be assigned after its declaration — even one declared
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
print(1 == 1.0);              # true — numeric kinds compare by value
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

**t-strings** (`t"..."`) are *template* values — they do not eagerly build a
string. A `Template` exposes its literal `.strings()` and its
`.interpolations()` (each an `Interpolation` with `.value()` and
`.expression()`), which lets you post-process interpolated data before
rendering.

String methods include `replace(a, b)`, `find(sub)`, `contains(sub)`,
`bytes()`, `chars()`, `clusters()`, and the indexed `get_byte`/`get_char`/
`get_cluster`.

### Lists, tuples

```generic
var xs = [1, 2, "three"];   # lists: mutable, heterogeneous
xs[0] = 10;                 # index assignment
xs.append(4);
print(xs[3]);               # 4  (indices must be in range; no negative indexing)

var t = (1, 2, 3);          # tuples: immutable
print(t + (4, 5));          # (1, 2, 3, 4, 5)
```

List methods: `append(x)`, `pop([i])`, `insert(i, x)`, `contains(x)`, plus
`[]`, `+`, `len`, and iteration.

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

Dict methods: `contains(k)`, `pop(k)`, `[]`/`[]=`, `len`. Set methods:
`insert(x)`, `remove(x)`, `contains(x)`, `len`. Dicts and sets are unordered
(display order follows the hash, not insertion) and do not implement the
iterator protocol, so they cannot be traversed with `foreach`.

### Ranges

`a..<b` is exclusive of `b`; `a..=b` is inclusive. A range is iterable,
`in`-testable, and has a length; it counts down when `a > b`.

```generic
foreach (var i in 1..<5) { print(i); }   # 1 2 3 4
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

There is no `&&`/`||` and no `not` — use `and`/`or`/`!`. The logical
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
print(xs is ys);       # true — same object
print(xs is [1, 2]);   # false — distinct objects
print(1 is 1);         # true — immediates compare by value
```

Compound assignment is available for the common operators: `+=`, `-=`, `*=`,
`/=`, `%=`, `&=`, `|=`, `^=`, and works on variables, fields, and indexed
targets (`xs[i] += 1`).

## Truthiness

`nil` and `false` are falsey. Empty collections (`[]`, `{}`, `{:}`, an empty
range) are falsey; non-empty ones are truthy. Instances are truthy unless the
class defines [`__bool__`](classes.md#operator-overloading).

Next: [Control flow →](control-flow.md)
