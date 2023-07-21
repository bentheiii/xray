# int

## type `int`

A signed integer. Has no size limit.

## fn `abs(x: int)->int`

Returns the absolute value of `x`.

## fn `add(a: int, b: int)->int`

Returns the sum of `a` and `b`.

## fn `binom(n: int, k: int)->int`

Returns the binomial coefficient of two numbers. Equivalent to 

\\[
    \frac{n!}{k!(n-k)!}
\\]

## fn `bit_and(a: int, b: int)->int`

Returns the bitwise AND of `a` and `b`.

## fn `bit_or(a: int, b: int)->int`

Returns the bitwise OR of `a` and `b`.

## fn `bit_xor(a: int, b: int)->int`

Returns the bitwise XOR of `a` and `b`.

## fn `ceil_root(x: int, r: int?)->int`

Returns the smallest integer greater than or equal to the root of `x`. The Optional argument `r` specifies the root to take. If it is not specified, the default is 2.

## fn `chr(x: int)->str`

Returns the unicode character represented by `x`. Returns an error if the number is not a valid unicode codepoint.

## fn `cmp(a: int, b: int)->int`

Returns the sign of the difference between `a` and `b`.

## fn `combination(n: int, i: int, k: int)->Sequence<int>`

Returns a combination of elements from the list `[1..n]`. Where `n` is the number of elements in the list, `k` is the number of elements in the combination, and `i` is the index of the combination to return. Returns a strictly increasing sequence of `k` integers that are all between `0` and `n-1` inclusive.

## fn `combination_with_replacement(n: int, i: int, k: int) -> Sequence<int>`

Returns a combination of elements from the list `[1..n]` with replacement. Where `n` is the number of elements in the list, `k` is the number of elements in the combination, and `i` is the index of the combination to return. Returns a monotonically increasing sequence of `k` integers that are all between `0` and `n-1` inclusive.

## fn `digits(x: int, b: int?)->Sequence<int>`

Returns the digits of `x` in a given base. The Optional argument `b` specifies the base to use, defaults to 10 The digits are returned in little-endian order.

```xray
let x = 123456789;

assert(digits(x) == [9,8,7,6,5,4,3,2,1])
```

## fn `div(a: int, b: int)->float`

Returns the quotient of `a` and `b`. Returns an error if `b` is zero.

## fn `div_ceil(a: int, b: int)->int`

Returns the ceiling of the quotient of `a` and `b`. Returns an error if `b` is zero.

## fn `div_floor(a: int, b: int)->int`

Returns the floor of the quotient of `a` and `b`. Returns an error if `b` is zero.

## fn `eq(a: int, b: int)->bool`

Returns whether `a` and `b` are equal.

## fn `factorial(x: int, i: int?)->int`

Returns the factorial of `x`. The Optional argument `i` specifies the step size to use, defaults to 1.

## fn `floor_root(x: int, r: int?)->int`

Returns the largest integer less than or equal to the root of `x`. The Optional argument `r` specifies the root to take. If it is not specified, the default is 2.

## fn `format(x: int, f: str)->int`

Returns a string representation of `x`, formatted according to a format string `f`. See [formatting](../lang/std_conventions.md#formatting) for details on formatting string format. Accepted modes:
* default: `int` is formatted as a decimal number.
* `b` or `B`: `int` is formatted as a binary number.
* `o` or `O`: `int` is formatted as an octal number.
* `x` or `X`: `int` is formatted as a hexadecimal number.

If alt mode is enabled, the number is preceded by the digit `0` and the mode character. So `format(10, "#b")` returns `"0b1010"`, and `format(10, "#B")` returns `"0B1010"`.

## fn `gcd(a: int, b: int)->int`

Returns the greatest common divisor of `a` and `b`. The result is always positive. `gcd(0, 0) == 0`.

## fn `harmonic_mean(a: int, b: int)->float`

Returns the harmonic mean of `a` and `b`. The harmonic mean of \\(x\\) and \\(y\\) is defined as \\(\frac{2xy}{x + y}\\).

## fn `hash(x: int)->int`

Returns a hash of `x`.

## fn `lcm(a: int, b: int)->int`

Returns the least common multiple of `a` and `b`. The result is always positive. `lcm(0,0)` is an error value.

## fn `mod(a: int, b: int)->int`

Returns the remainder of `a` and `b`. Returns an error if `b` is zero. XRay uses floored division, so the result will always have same sign as `b`.

## fn `mul(a: int, b: int)->int`

Returns the product of `a` and `b`.

## fn `multinom(k: Sequence<int>)->int`

Returns the multinomial coefficient of a list of numbers `k`. Equivalent to

\\[
    \frac{(k_1+k_2+...+k_m)!}{k_1!k_2!...k_m!}
\\]

Where \\(k_i\\) is the \\(i\\)th element of `k`.

## fn `neg(x: int)->int`

Returns the negation of `x`.

## fn `permutation(n: int, i: int) -> Sequence<int>`

A convenience function for [`permutation`](#permutation3), where the length of the permutation is equal to the length of the list `n`.

## fn `permutation(n: int, i: int, k: int) -> Sequence<int>` {#permutation3}

Returns a permutation of elements from the list `[1..n]`. Where `n` is the number of elements in the list, `k` is the number of elements in the permutation, and `i` is the index of the permutation to return. Returns a sequence of `k` unique integers that are all between `0` and `n-1` inclusive.

## fn `pow(a: int, b: int)->int`

Returns the power of `a` and `b`. Returns an error if `b` is negative, or if `a` and `b` are zero.

## fn `product(g: Generator<int>)->int`

Returns the product of the numbers in `g`.

## fn `product(s: Sequence<int>)->int`

Returns the product of the numbers in `s`.

## fn `sample_standard_variation(s: Sequence<int>)->float`

Returns the sample standard variation of the numbers in `s`.

## fn `sample_variance(s: Sequence<int>)->float`

Returns the sample variance of the numbers in `s`.

## fn `sign(x: int)->int`

Returns the sign of `x`. Returns `-1` if `x` is negative, `0` if `x` is zero, and `1` if `x` is positive.

## fn `sub(a: int, b: int)->int`

Returns the difference of `a` and `b`.

## fn `sum(g: Generator<int>)->int`

Returns the sum of the numbers in `g`.

## fn `sum(s: Sequence<int>)->int`

Returns the sum of the numbers in `s`.

## fn `to_float(x: int)->float`

Returns the floating point representation of `x`. Will return an error if `x` is too large to be represented as a float.

## fn `to_str(x: int)->str`

Returns a string representation of `x`. Equivalent to `format(x, "")`.
