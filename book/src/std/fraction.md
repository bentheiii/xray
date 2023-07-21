# Fraction

## struct `Fraction(n: int, d: int)`

A rational number. Do not construct directly, instead use the [`fraction` function](#fraction2).

## fn `abs(x: Fraction) -> Fraction`

Returns the absolute value of `x`.

## fn `add(x: Fraction, y: Fraction) -> Fraction`

Returns the sum of `x` and `y`.

## fn `ceil(x: Fraction) -> int`

Returns the ceiling of `x`.

## fn `cmp(x: Fraction, y: Fraction) -> int`

Compares fractions `x` and `y`.

## fn `div(x: Fraction, y: Fraction) -> Fraction`

Returns the quotient of `x` and `y`.

## fn `eq(x: Fraction, y: Fraction) -> bool`

Returns `true` if `x` and `y` are equal.

## fn `floor(x: Fraction) -> int`

Returns the floor of `x`.

## fn `fraction(n: int) -> Fraction`

Constructs a fraction with numerator `n` and denominator `1`.

## fn `fraction(n: int, d: int) -> Fraction`  {#fraction2}

Constructs a fraction with numerator `n` and denominator `d`.

## fn `fraction(x: float) -> Fraction`

Constructs a fraction from a real number `x`. Note that this function is not exact, and may return a fraction that is not equal to `x`.

## fn `hash(x: Fraction) -> int`

Returns a hash of `x`.

## fn `mod(x: Fraction, y: Fraction) -> Fraction`

Returns the remainder of `x` divided by `y`.

## fn `mul(x: Fraction, y: Fraction) -> Fraction`

Returns the product of `x` and `y`.

## fn `neg(x: Fraction) -> Fraction`

Returns the negation of `x`.

## fn `pow(x: Fraction, y: int) -> Fraction`

Returns `x` raised to the power of `y`.

## fn `sign(x: Fraction) -> int`

Returns the sign of `x`.

## fn `sub(x: Fraction, y: Fraction) -> Fraction`

Returns the difference of `x` and `y`.

## fn `trunc(x: Fraction) -> int`

Returns the truncation of `x`, that is, `x` rounded towards zero.