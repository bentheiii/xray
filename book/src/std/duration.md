# Duration

## struct `Duration(seconds: float)`

A duration of time. A negative duration represents a backwards offset in time.

## fn `add(x: Duration, y: Duration) -> Duration`

Returns the sum of `x` and `y`.

## fn `days(x: Duration) -> float`

Returns the number of days in `x`.

## fn `days(x: float) -> Duration`

Returns a duration of `x` days.

## fn `div(x: Duration, y: float) -> Duration`

Returns A duration of `x` divided by `y`.

## fn `div(x: Duration, y: int) -> Duration`

A convenience function for dividing a duration by an integer.

## fn `div(x: Duration, y: Duration) -> float`

Returns the quotient of `x` and `y`.

## fn `hours(x: Duration) -> float`

Returns the number of hours in `x`.

## fn `hours(x: float) -> Duration`

Returns a duration of `x` hours.

## fn `minutes(x: Duration) -> float`

Returns the number of minutes in `x`.

## fn `minutes(x: float) -> Duration`

Returns a duration of `x` minutes.

## fn `mul(x: Duration, y: float) -> Duration`

Returns A duration of `x` multiplied by `y`.

## fn `mul(x: Duration, y: int) -> Duration`

A convenience function for multiplying a duration by an integer.

## fn `mul(x: float, y: Duration) -> Duration`

A convenience function for multiplying a duration by a real number.

## fn `mul(x: int, y: Duration) -> Duration`

A convenience function for multiplying a duration by an integer.

## fn `seconds(x: Duration) -> float`

Returns the number of seconds in `x`.

## fn `seconds(x: float) -> Duration`

Returns a duration of `x` seconds.

## fn `sleep(x: Duration) -> ()`
Requires `SLEEP` permission.

Stops execution for `x` seconds.

## fn `sleep<T>(x: Duration, value: T) -> T`
Requires `SLEEP` permission.

Stops execution for `x` seconds, then returns `value`.

## fn `years(x: Duration) -> float`

Returns the number of years in `x`.

## fn `years(x: float) -> Duration`

Returns a duration of `x` years.