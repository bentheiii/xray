# Complex

## struct `Complex(r: float, i: float)`

A complex number, consisting of a real part `r` and an imaginary part `i`.

## fn `abs(x: Complex) -> float`

Returns the absolute value of `x`.

## fn `add(x: Complex, y: Complex) -> Complex`

Returns the sum of `x` and `y`.

## fn `add(x: Complex, y: float) -> Complex`

A convenience function for adding a real number to a complex number.

## fn `add(x: Complex, y: int) -> Complex`

A convenience function for adding a real number to a complex number.

## fn `add(x: float, y: Complex) -> Complex`

A convenience function for adding a real number to a complex number.

## fn `add(x: int, y: Complex) -> Complex`

A convenience function for adding a real number to a complex number.

## fn `arg(x: Complex) -> float`

Returns the argument of `x`, in radians.

## fn `complex(r: float) -> Complex`

A convenience function for creating a complex number with no imaginary part and real part `r`.

## fn `complex(r: int) -> Complex`

A convenience function for creating a complex number with no imaginary part and real part `r`.

## fn `complex_from_polar(r: float, theta: float) -> Complex`

A convenience function for creating a complex number from polar coordinates, with radius `r` and angle `theta` in radians.

## fn `conjugate(x: Complex) -> Complex`

Returns the complex conjugate of `x`.

## fn `div(x: Complex, y: Complex) -> Complex`

Returns the quotient of `x` and `y`.

## fn `div(x: Complex, y: float) -> Complex`

A convenience function for dividing a complex number and a real number.

## fn `div(x: Complex, y: int) -> Complex`

A convenience function for dividing a complex number and a real number.

## fn `div(x: float, y: Complex) -> Complex`

A convenience function for dividing a complex number and a real number.

## fn `div(x: int, y: Complex) -> Complex`

A convenience function for dividing a complex number and a real number.

## fn `eq(x: Complex, y: Complex) -> bool`

Returns whether `x` and `y` are equal.

```admonish note
Since floating point numbers are not exact, this function can produce unexpected results. To check if two complex numbers are close, use the [`is_close`](#is_close) function.
```

## fn `is_close(x: Complex, y: Complex, rel_tol: float?, abs_tol:float?) -> bool`  {#is_close}

Returns whether `x` and `y` are close, within a relative tolerance of `rel_tol` and an absolute tolerance of `abs_tol`. Both tolerances default to `1e-6`, and are checked against the absolute value of the difference between `x` and `y`. returns `true` if:

\\[
\left|x - y\right| \leq \max (\mathrm{atol}, \mathrm{rtol} \cdot \max(\left|x\right|, \left|y\right|))
\\]

## fn `ln(x: Complex) -> Complex`

Returns the natural logarithm of `x`, with branch cut along the negative real axis.

## fn `mul(x: Complex, y: Complex) -> Complex`

Returns the product of `x` and `y`.

## fn `mul(x: Complex, y: float) -> Complex`

A convenience function for multiplying a complex number and a real number.

## fn `mul(x: Complex, y: int) -> Complex`

A convenience function for multiplying a complex number and a real number.

## fn `mul(x: float, y: Complex) -> Complex`

A convenience function for multiplying a complex number and a real number.

## fn `mul(x: int, y: Complex) -> Complex`

A convenience function for multiplying a complex number and a real number.

## fn `neg(x: Complex) -> Complex`

Returns the negation of `x`.

## fn `pow(x: Complex, y: Complex) -> Complex`

Returns `x` raised to the power of `y`.

## fn `pow(x: Complex, y: float) -> Complex`

A convenience function for raising a complex number to a real power.

## fn `pow(x: Complex, y: int) -> Complex`

A convenience function for raising a complex number to a real power.

## fn `pow(x: float, y: Complex) -> Complex`

A convenience function for raising a real number to a complex power.

## fn `sub(x: Complex, y: Complex) -> Complex`

Returns the difference of `x` and `y`.

## fn `sub(x: Complex, y: float) -> Complex`

A convenience function for subtracting a real number and a complex number.

## fn `sub(x: Complex, y: int) -> Complex`

A convenience function for subtracting a real number and a complex number.

## fn `sub(x: float, y: Complex) -> Complex`

A convenience function for subtracting a real number and a complex number.

## fn `sub(x: int, y: Complex) -> Complex`

A convenience function for subtracting a real number and a complex number.