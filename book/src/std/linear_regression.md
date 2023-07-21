# LinearRegression

## struct `LinearRegression(slope: float, intercept: float)`

A linear regression model.

## fn `eq(x: LinearRegression, y: LinearRegression) -> bool`

Returns `true` if `x` and `y` are equal.

## fn `linear_regression_leasty_square(x: Generator<float>, y: Generator<float>) -> LinearRegression`

Returns a linear regression model that fits the data in `x` and `y` using the least squares method. If `x` and `y` are not the same length, the shorter length is used.

## fn `predict(x: LinearRegression, x: float) -> float`

Returns the predicted value of `y` given `x`.