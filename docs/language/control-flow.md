# Control flow

[← Guide index](README.md)

Parentheses around a condition are **optional** everywhere (`if x > 3 {…}`
and `if (x > 3) {…}` are equivalent). Bodies are always blocks.

## Conditionals: `if`, `unless`, `?:`

```text
if x > 3 {
    print("big");
} else {
    print("small");
}

unless done {        # runs the body when the condition is false
    keep_going();
}

var label = x > 3 ? "big" : "small";   # the conditional operator
```

`else` is always followed by a block, so chain conditionals by nesting an
`if` inside the `else` block:

```text
if x == 1 {
    print("one");
} else {
    if x == 2 { print("two"); } else { print("other"); }
}
```

## Loops: `while`, `until`, `for`, `foreach`

`while` loops while the condition is true; `until` loops while it is false.

```text
while x > 0 { x = x - 1; }
until x >= 10 { x = x + 1; }
```

`for` is the C-style three-clause loop; `foreach` iterates any iterable and
**requires `var`** (or `const`) on the loop variable.

```generic
for (var i = 0; i < 3; i = i + 1) {
    print(i);
}

foreach (var item in [10, 20, 30]) {
    print(item);
}

foreach (var i in 1..<5) { print(i); }   # ranges are iterables
```

Iterables are lists, tuples, ranges, generators,
[templates](values.md#strings), and any class implementing the
[iterator protocol](functions.md#generators). Sets, dicts, and strings
are *not* iterable.

## `break` and `continue`

```generic
foreach (var n in 1..<100) {
    if n % 2 == 0 { continue; }
    if n > 5 { break; }
    print(n);
}
```

### Loop labels

Prefix a loop with a `'label` and target it from a nested loop with
`break 'label` / `continue 'label`:

```generic
for 'outer (var i = 0; i < 3; i = i + 1) {
    foreach (var j in 0..<3) {
        if j == 1 { continue 'outer; }   # next iteration of the OUTER loop
        if i == 2 { break 'outer; }      # exit the OUTER loop
        print(f"${i},${j}");
    }
}
```

## `switch`

`switch` matches an expression against `case` values. Cases do **not** fall
through — each runs only its own statements — and an optional `default` must
come last.

```text
switch (code) {
    case 200: print("ok");
    case 404: print("not found");
    default:  print("other");
}
```

Next: [Functions →](functions.md)
