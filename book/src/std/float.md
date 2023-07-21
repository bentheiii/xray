# float

## type `float`

A 64 floating-point number. A float is guaranteed to never be a special IEEE value (NaN or Infinity).

## let `e: float`

The base of the natural logarithm. Approximately `2.7182`.

## let `pi: float`

The ratio of a circle's circumference to its diameter. Approximately `3.1415`.

## fn `abs(x: float)->float`

Returns the absolute value of `x`.

## fn `acos(x: float)->float`

Returns the arccosine of `x`, in radians.

## fn `acosh(x: float)->float`

Returns the hyperbolic arccosine of `x`.

## fn `add(a: float, b: float)->float`

Returns the sum of `a` and `b`.

## fn `add(a: float, b: int)->float`

Convenience function for adding a `float` and an `int`.

## fn `add(a: int, b: float)->float`

Convenience function for adding an `int` and a `float`.

## fn `asin(x: float)->float`

Returns the arcsine of `x`, in radians.

## fn `asinh(x: float)->float`

Returns the hyperbolic arcsine of `x`.

## fn `atan(x: float)->float` {#atan1}

Returns the arctangent of `x`, in radians.

## fn `atan(y: float, x: float)->float`

Returns the arctangent of a ratio, in radians. Equivalent to the [`atan`](#atan1) of `y/x` (unless `x` is zero, in which case the result is `pi/2`).

## fn `atanh(x: float)->float`

Returns the hyperbolic arctangent of `x`.

## fn `cbrt(x: float)->float`

Returns the cube root of `x`.

## fn `ceil(x: float)->int`

Returns the smallest integer greater than or equal to `x`.

## fn `cmp(a: float, b: float)->int`

Compares `a` and `b`.

## fn `cos(x: float)->float`

Returns the cosine of `x`, in radians.

## fn `cosh(x: float)->float`

Returns the hyperbolic cosine of `x`.

## fn `covariance(pairs: Generator<(float, float)>) -> float`

Returns the covariance of a generator of pairs of numbers `pairs`. The covariance of \\(N\\) pairs \\((x_1, y_1), (x_2, y_2), \ldots, (x_N, y_N)\\) is defined as:

\\[
\frac{\sum_{i=1}^N (x_i - \bar{x})(y_i - \bar{y})}{N}
\\]

where \\(\bar{x}\\) and \\(\bar{y}\\) are the means of the \\(x\\) and \\(y\\) values, respectively.

## fn `div(a: float, b: float)->float`

Returns the quotient of `a` and `b`. If `b` is zero, returns an error.

## fn `div(a: float, b: int)->float`

Convenience function for dividing a `float` by an `int`.

## fn `div(a: int, b: float)->float`

Convenience function for dividing an `int` by a `float`.

## fn `eq(a: float, b: float)->bool`

Returns `true` if `a` and `b` are equal, and `false` otherwise.

```admonish note
Due to the nature of floating-point numbers, this function may return `false` even if the two numbers are very close to each other. For example, `eq(0.1 + 0.2, 0.3)` returns `false`. To check whether two number are approximatly equal, use [`is_close`](#is_close) instead.
```

## fn `erf(x: float)->float`

Returns the gauss error function of `x`.

## fn `erfc(x: float)->float`

Returns the complementary error function of `x`.

## fn `expm1(x: float)->float`

Returns \\(e^x - 1\\), where \\(e\\) is the base of the natural logarithm. Is more accurate than `exp(x) - 1` for small values of \\(x\\).

## fn `floor(x: float)->int`

Returns the largest integer less than or equal to `x`.

## fn `format(x: float, f: str)->str`

Returns a string representation of `x`, formatted according to a format string `f`. See [formatting](../lang/std_conventions.md#formatting) for details on formatting string format. Accepted modes:
* `f`: fixed-point notation. This is the default.
* `e`: scientific notation.
* `E`: scientific notation, with an uppercase `E` in the exponent.
* `%`: multiply by 100 and format as a percentage (as though with `f` mode).

## fn `gamma(x: float)->float`

Returns the gamma function of `x`.

## fn `gammaln(x: float)->float`

Returns the natural logarithm of the gamma function of `x`.

## fn `harmonic_mean(a: float, b: float)->float`

Returns the harmonic mean of `a` and `b`. The harmonic mean of \\(x\\) and \\(y\\) is defined as \\(\frac{2xy}{x + y}\\).

## fn `is_close(a: float, b: float, rtol: float?, atol: float?) -> bool` {#is_close}

Returns `true` if `a` and `b` are close to each other, and `false` otherwise. `rtol` is the relative tolerance, and `atol` is the absolute tolerance. Both `rtol` and `atol` are optional, and default to `1e-6`. Returns `true` if:

\\[
\left|a - b\right| \leq \max (\mathrm{atol}, \mathrm{rtol} \cdot \max(\left|a\right|, \left|b\right|))
\\]

## fn `log(x: float, base: float)->float`

Returns the logarithm of `x` in a given base `base`.

## fn `log1p(x: float)->float`

Returns the natural logarithm of \\(1 + x\\). Is more accurate than `log(1 + x)` for small values of \\(x\\).

## fn `ln(x: float)->float`

Returns the natural logarithm of `x`.

## fn `mod(a: float, b: float)->float`

Returns the remainder of dividing `a` and `b`. If the `b` is zero, returns an error. XRay uses floored division, so the result will always have same sign as `b`.

## fn `mul(a: float, b: float)->float`

Returns the product of `a` and `b`.

## fn `mul(a: float, b: int)->float`

Convenience function for multiplying a `float` by an `int`.

## fn `mul(a: int, b: float)->float`

Convenience function for multiplying an `int` by a `float`.

## fn `neg(x: float)->float`

Returns the negation of `x`.

## fn `pearson_correlation(s: Sequence<(float, float)>) -> float`

Returns the Pearson correlation coefficient of a sequence of pairs of numbers `s`. The Pearson correlation coefficient of \\(N\\) pairs \\((x_1, y_1), (x_2, y_2), \ldots, (x_N, y_N)\\) is defined as:

\\[
\frac{\sum_{i=1}^N (x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum_{i=1}^N (x_i - \bar{x})^2 \sum_{i=1}^N (y_i - \bar{y})^2}}
\\]

where \\(\bar{x}\\) and \\(\bar{y}\\) are the means of the \\(x\\) and \\(y\\) values, respectively.

## fn `pow(x: float, p: float)->float`

Returns `x` raised to power `p`.

## fn `pow(x: float, p: int)->float`

Convenience function for raising a `float` to an `int` power.

## fn `pow(x: int, p: float)->float`

Convenience function for raising an `int` to a `float` power.

## fn `product(items: Generator<float>)->float`

Returns the product of the numbers in `items`.

## fn `product(items: Sequence<float>)->float`

Returns the product of the numbers in `items`.

## fn `sign(x: float)->float`

Returns the sign of `x`. Returns `1.0` if `x` is positive, `-1.0` if `x` is negative, and `0.0` if `x` is zero.

## fn `sin(x: float)->float`

Returns the sine of `x`, in radians.

## fn `sinh(x: float)->float`

Returns the hyperbolic sine of `x`.

## fn `sqrt(x: float)->float`

Returns the square root of `x`.

## fn `sub(a: float, b: float)->float`

Returns the difference of `a` and `b`.

## fn `sub(a: float, b: int)->float`

Convenience function for subtracting an `int` from a `float`.

## fn `sub(a: int, b: float)->float`

Convenience function for subtracting a `float` from an `int`.

## fn `sum(items: Generator<float>, initial: int?)->float`

Returns the sum of the numbers in `items`. Uses [Kahan summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm) to reduce numerical error.


## fn `sum(items: Sequence<int>, initial: int?)->float`

Returns the sum of the numbers in `items`. Uses [Kahan summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm) to reduce numerical error.

## fn `tan(x: float)->float`

Returns the tangent of `x`, in radians.

## fn `tanh(x: float)->float`

Returns the hyperbolic tangent of `x`.

## fn `to_str(x: float)->str`

Returns a string representation of `x`.

## fn `trunc(x: float)->int`

Returns `x` rounded towards zero.