# bool

## type `bool`

A type representing a boolean value, either `true` or `false`.

## fn `all(x: Sequence<bool>)->bool`

Returns `true` if all the elements of `x` are `true`, otherwise returns `false`.

## fn `any(x: Sequence<bool>)->bool`

Returns `true` if any of the elements of `x` are `true`, otherwise returns `false`.

## fn `and(a: bool, b: bool)->bool`

Returns the logical and of `a` and `b`. This function is short-circuiting, and will only evaluate `b` if `a` is `true`.

## fn `assert(x: bool)->bool`

Returns `true` if the `x` is `true`, otherwise returns an error. Therefore, this function will never return `false`.

`assert` returning a boolean can be used to chain assertions with the `and` function.

```xray
assert(factorial(5) == 120)
&& assert(factorial(6) == 720)
&& assert(factorial(7) == 5040)
```

## fn `bit_xor(a: bool, b: bool)->bool`

Returns the logical xor of `a` and `b`.

## fn `cmp(a: bool, b: bool)->int`

Returns the comparison of `a` and `b`. Where `false` is considered less than `true`.

## fn `eq(a: bool, b: bool)->bool`

Returns `true` if `a` and `b` are equal, otherwise returns `false`.

## fn `hash(x: bool)->int`

Returns the hash of `x`.

## fn `indicator(x: bool)->int`

Returns `1` if `x` is `true`, otherwise returns `0`.

## fn `not(x: bool)->bool`

Returns the logical not of `x`.

## fn `or(a: bool, b: bool)->bool`

Returns the logical or of `a` and `b`. This function is short-circuiting, and will only evaluate `b` if `a` is `false`.

## fn `then<T>(cond: bool, if_true: T)->Optional<T>`

Returns `some` of `if_true` if `cond` is `true`, otherwise returns `none`. This function is short-circuiting, and will only evaluate `if_true` if `cond` is `true`.

## fn `to_str(x: bool)->str`

Returns the string representation of `x`.