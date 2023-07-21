# Types

Every value in XRay has a type. Types are used to determine what operations can be performed on a value, and how the value is stored in memory.

## Integer Numbers: `int`

An integer is a whole number. Integers can be positive or negative, and can be of any size.

```xray
let x: int = 5;
let y: int = -5;
let z: int = 0;
let big: int = 123456789012345678901234567890;
```

## Floating Point Numbers: `float`

A floating point number is a number with a decimal point. Floating point numbers can be positive or negative, and are represented as 64-bit float-point numbers.

```xray
let x: float = 5.0;
let y: float = -5.0;
let z: float = 0.0;
let big: float = 1e60;
```

Note that, unlike in other programming languages, floating point values can never be `NaN` or `Infinity`. If some operation would result in a `NaN` or `Infinity`, it will instead result in an error.

## Booleans: `bool`

A boolean is a value that is either `true` or `false`. Comparison operators should always return a boolean value.

```xray
let x: bool = true;
let y: bool = false;
let z = 5 > 7;  // z is a bool
```

## Strings: `str`

A string is a sequence of characters. Strings internally represented as a sequence of UTF-8 bytes. Strings can be created using double quotes (`"`) or single quotes (`'`).

```xray
let x: str = "Hello, world!";
let y: str = 'Hello, world!';
let z = "👋"
```

Note that letters that are not single utf-8 characters (like [non-spacing marks](https://www.compart.com/en/unicode/category/Mn)) are considered separate characters.
```xray
let x = "ä";
assert len(x) == 2;
```

Strings can also be created with raw string literals, or formatted string literals.

```xray
let x = r"Hello\tworld!";  // raw string literal, \t is not interpreted as a tab
let name = "Bob";
/* formatted string literal, name is replaced with the value of the variable name */
let y = f"Hello {name}!";
```

[Read more about string literals](string_literals.md).

## Unknown

A special type that represents a type that has no values, and thus can never be accessed. This type is usually used for empty containers, whose type is inferred from the values that are added to them.

```xray
let x = [];  // x is a Sequence<unknown>
let y = x.push(5);  // y is a Sequence<int>
```

Since `Unknown` is a bottom type, it can be coerced to any other type by explicitly stating the type of a variable, or by using the [cast function](../std/general.md#cast).

```xray
let y = none();  // the inferred type of y is Optional<unknown>
// we can coerce y to an Optional<int> by explicitly stating the type
let z: Optional<int> = none();
```

# Generic Types

Generic types are types that can be parameterized with other types.

## Sequences: `Sequence<T>`
A sequence is a collection of values of the same type. Sequences can be created with the array literal.

```xray
let x = [1, 2, 3];  // x is a Sequence<int>
let y = ["a", "b", "c"];  // y is a Sequence<str>
```

```admonish note
Other programming languages have arrays, which are contiguous blocks of data. While XRay sequences might be internally implemented as arrays, this is not always the case. For example, the `range` function returns a sequence of numbers, but does not allocate any memory for them.
```

## Optional Values: `Optional<T>`
An optional value is a value that may or may not be present. Optional values are created with the `none()` and `some` functions.

```xray
let x = some(5);  // x is an Optional<int>
let y = none();  // the inferred type of y is Optional<unknown>
```

Optional values can be used with the [`has_value`](../std/optional.md#has_value) and [`value`](../std/optional.md#value) functions.

```xray
let x = some(5);

assert(x.has_value())
&& assert(x.value() == 5);
```

```xray errors
let x = none();
x.value();  // error: value() called on an Optional with no value
```

## Tuples: `(T0, T1, ...)`
A tuple is a fixed-length sequence of values of different types. Tuples can be created with the tuple literal.

```xray
let x: (int, str, float) = (1, "a", 5.0);

// tuple items can be accessed with the accessor operator and the "item" prefix
assert(x::item0 == 1)
&& assert(x::item1 == "a")
&& assert(x::item2 == 5.0);
```
### nil: `()`
The empty tuple is a special type that has items, and thus only one possible value. It is used to represent variables and values that hold no information.

```xray
let x = ();
```

## Functions: `(T0, T1, ...) -> (R)`

A function is any object that can be called with a set of arguments and returns a value. Functions are first-class objects in XRay, which means that they can be passed around as arguments to other functions, returned from functions, and stored in variables.

```xray
fn apply_twice(f: (int) -> (int), x: int) -> int {
    // will apply the function f twice to x
    f(f(x))
}

fn add_one(x: int) -> int {
    x + 1
}

let x = apply_twice(add_one, 10);
assert(x == 12)
```

## Generators: `Generator<T>`
A generator is an abstraction over an iteration of some collection, whether it be a sequence, a set, or something else. Generators can be created with a variety of functions, like the [sequence's `filter` function](../std/sequences.md#filter).

```xray
fn is_square(x: int) -> bool {
    let y = sqrt(x);
    y % 1 == 0
}

let s = range(0, 150);  // a sequence of numbers from 0 to 149

let squares: Generator<int> = s.filter(fn, is_square);

assert(squares.sum() == 650)
```

## Sets: `Set<T>`
A set is a collection of unique values of the same type. Lookups in sets are very fast, but the order of the items is not preserved. To create a set, we must provide it with hash function and equality functions for the type of the items.

```xray
fn int_hash(x: int) -> int {
    x
}

fn int_eq(x: int, y: int) -> bool {
    x == y
}

let s = set(int_hash, int_eq);
let s0 = s.add(1);
let s1 = s0.add(2);

assert(s1.contains(1))
```

We can use the [`dynamic set function`](../std/sets.md#set_dyn) macro to create sets more easily.

```xray
let s = set<int>();
let s0 = s.add(1);
let s1 = s0.add(2);

assert(s1.contains(1))
```

## Mappings: `Mapping<K, V>`

A mapping is a collection of key-value pairs. Mappings can be created with the `mapping` function, that requires a hash function and an equality function for the type of the keys (much like sets do).

```xray
fn int_hash(x: int) -> int {
    x
}

fn int_eq(x: int, y: int) -> bool {
    x == y
}

let m = mapping(int_hash, int_eq);
let m0 = m.set(1, "a");
let m1 = m0.set(2, "b");

assert(m1.get(1) == "a")
```

We can use the [`dynamic mapping function`](../std/mappings.md#mapping_dyn) macro to create mappings more easily.

```xray
let m = mapping<int>();
let m0 = m.set(1, "a");
let m1 = m0.set(2, "b");

assert(m1.get(1) == "a")
```

## Additional Built-in Types

The standard library contains many more utility types, from [regular expression patterns](../std/regex.md) to [date time](../std/datetime.md). You can find a complete list of them in the [standard library reference](../std/intro.md).

# Type Aliases
Type aliases are a way to give a new name to an existing type. They are useful to make code more readable, and to avoid repeating long type names.

```xray
type Span = (int, Optional<int>);
type Spans = Sequence<Span>;

fn substrings(string: str, spans: Spans) -> Sequence<str> {
    spans.map((span: Span) => {string.substring(span::item0, span::item1)})
}
```