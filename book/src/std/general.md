# general

The following pertain to any type in the XRay language.

## dyn fn `cast<T>(x: T) -> T`

Requires dynamic specialization. Returns `x` unchanged. Can be used to "up-cast" a type to disambiguate overloaded functions.

For example, the following code will fail to compile:

```xray does_not_compile
fn foo(x: int) -> int { 0 }
fn foo(x: float) -> float { 0.0 }

x = foo([][0])
```

Because a value of type `Unknown` fits into both overloads, but we can cast it to `int` to disambiguate:

```xray
fn foo(x: int) -> int { 0 }
fn foo(x: float) -> float { 0.0 }

x = foo(
    cast<int>([][0])
)
```

## fn `debug<T>(x: T, prefix: str?) -> T`
Requires `PRINT_DEBUG` permissions.

Prints the value of `x` to STDOUT, optionally preceded by `prefix`, followed by a newline.
Note that the value is formatted by its non-user friendly internal representation, and may be difficult to read.

Returns `x`.

## dyn fn `display<T>(x: T, prefix: str?) -> T`
Where `to_str(T) -> str`

Requires `PRINT` permissions.

Prints the `to_str` of `x` to STDOUT, optionally preceded by `prefix`, followed by a newline.

Returns `x`.

## dyn fn `gt<T0, T1>(a: T0, b: T1) -> bool`
Where `cmp(T0, T1) -> int`

Returns whether `cmp(a,b)` is greater than zero.

## dyn fn `ge<T0, T1>(a: T0, b: T1) -> bool`
Where `cmp(T0, T1) -> int`

Returns whether `cmp(a,b)` is greater than or equal to zero.

## fn `if<T>(cond: bool, then: T, else: T)->T`

Returns `then` if `cond` is true, otherwise returns `else`.
This function is short-circuiting for `then` and `else`, and will only evaluate one of them.

## dyn fn `lt<T0, T1>(a: T0, b: T1) -> bool`
Where `cmp(T0, T1) -> int`

Returns whether `cmp(a,b)` is less than zero.

## dyn fn `le<T0, T1>(a: T0, b: T1) -> bool`
Where `cmp(T0, T1) -> int`

Returns whether `cmp(a,b)` is less than or equal to zero.

## fn `max<T>(a: T, b: T, lt_: (T, T)->(bool)) -> T` {#max3}

Returns `a` if `lt_(a,b)` returns false, otherwise returns `b`.

If `lt_` describes a "less-than" relation, this function would return the maximum of `a` and `b`, or `a` if they are equal.


## dyn fn `max<T0, T1>(a: T0, b: T1) -> U`
Where `lt(T0, T1) -> bool` and `max(T0, T1, lt{T0, T1}) -> U`

Dynamic variant of [`max`](#max3), that infers the less-than function.

## fn `min<T>(a: T, b: T, lt_: (T, T)->(bool)) -> T` {#min3}

Returns `a` if `lt_(a,b)` returns true, otherwise returns `b`.

If `lt_` describes a "less-than" relation, this function would return the minimum of `a` and `b`, or `a` if they are equal.

## dyn fn `min<T0, T1>(a: T0, b: T1) -> U`
Where `lt(T0, T1) -> bool` and `min(T0, T1, lt{T0, T1}) -> U`

Dynamic variant of [`min`](#min3), that infers the less-than function.

## dyn fn `ne<T0, T1>(a: T0, b: T1) -> bool`
Where `eq(T0, T1) -> bool`

Returns the negation of `eq(a, b)`.

## dyn fn `partial<...>(func: (T0, T1, ..., TK0, TK1, ...)->(R), p0: T0, p1: T1, ...)->(TK0, TK1, ...)->(R)`

Returns a function that, when called with arguments, will call `func` with the arguments preceded by the arguments sent to `partial`.

```xray
fn foo(a: int, b: int, c: int) -> int { 100*a + 10*b + c }

let bar = partial(foo, 1, 2)

assert(bar(3) == 123)
```

## fn `to_cmp<T, U>(key_func: (T)->(U), cmp_: (U,U)->(int))->(T,T)->(int)` {#to_cmp2}

Adapts a key function `key_func` and a comparison of the key `cmp_` into a comparison of the original type.

```xray
let arr = [1, 2, 3, 4, 5];
// we want to sort the array by the remainder of each element divided by 3
let my_cmp = to_cmp(
    (x: int) -> { x % 3 },
    cmp{int, int}
);

assert(arr.sort(my_cmp) == [3, 1, 4, 2, 5])
```

## dyn fn `to_cmp<T, U>(key_func: (T)->(U))->(T,T)->(int)`
Where `cmp(U, U) -> int`

Dynamic variant of [`to_cmp`](#to_cmp2), that infers the key comparison function.

## fn `to_eq<T, U>(key_func: (T)->(U), eq_: (U,U)->(bool))->(T,T)->(bool)` {#to_eq2}

Adapts a key function `key_func` and an equality of the key `eq_` into an equality of the original type.

```xray
let arr = [1, 2, 3, 4, 5];
// we want to count all elements that are equal to 1 modulo 3
let my_eq = to_eq(
    (x: int) -> { x % 3 },
    eq{int, int}
);
assert(arr.count(1, my_eq) == 2)
```

## dyn fn `to_eq<T, U>(key_func: (T)->(U))->(T,T)->(bool)`
Where `eq(U, U) -> bool`

Dynamic variant of [`to_eq`](#to_eq2), that infers the key equality function.

## fn `to_lt<T, U>(key_func: (T)->(U), lt_: (U,U)->(bool))->(T,T)->(bool)` {#to_lt2}

Adapts a key function `key_func` and a less-than of the key `lt_` into a less-than of the original type.

```xray
let num1 = 109;
let num2 = 203;

// we ant the minimum of the two numbers, by their last digit
let my_lt = to_lt(
    (x: int) -> { x % 10 },
    lt{int, int}
);

assert(min(num1, num2, my_lt) == 203)
```

## dyn fn `to_lt<T, U>(key_func: (T)->(U))->(T,T)->(bool)`

Dynamic variant of [`to_lt`](#to_lt2), that infers the key less-than function.