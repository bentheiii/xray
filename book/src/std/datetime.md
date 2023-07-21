# Datetime

## struct `Datetime(date: Date, hours: int, minutes: int, seconds: float)`

A datetime in the Gregorian calendar.

## fn `add(x: Datetime, y: Duration) -> Datetime`

Returns the datetime `y` after `x`. If `y` is negative, returns the datetime `y` before `x`.

## fn `add(x: Duration, y: Datetime) -> Datetime`

A convenience function for `add(y, x)`.

## fn `cmp(x: Datetime, y: Datetime) -> int`

Compares datetimes `x` and `y`.

## fn `datetime(unix: float) -> Datetime`

Returns the datetime corresponding to the unix timestamp `unix`.

## fn `eq(x: Datetime, y: Datetime) -> bool`

Returns `true` if `x` and `y` are equal.

## fn `format(x: Datetime, format: str) -> str`

Formats `x` according to `format`. The format string can be any string, it will have the following character combinations replaced:

* `%Y` - the year, as a four-digit number
* `%y` - the year, as a two-digit number
* `%m` - the month, as a two-digit number
* `%d` - the day, as a two-digit number
* `%B` - the full name of the month
* `%b` - the abbreviated name of the month
* `%A` - the full name of the weekday
* `%a` - the abbreviated name of the weekday
* `%w` - the weekday, as a one-digit number (where sunday is 0)
* `%H` - the hour, as a two-digit number
* `%M` - the minute, as a two-digit number
* `%S` - the second, as a two-digit number
* `%f` - the microsecond, as a six-digit number
* `%P` - `AM` or `PM`
* `%p` - `am` or `pm`
* `%I` - the hour, as a two-digit number (12-hour clock)
* `%%` - the character `%`

## fn `now() -> Datetime`
Requires `NOW` permission.

Returns the current datetime.

## fn `sub(x: Datetime, y: Datetime) -> Duration`

Returns the duration between `x` and `y`. If `x` is before `y`, the duration will be negative.

## fn `to_str(x: Datetime) -> str`

Returns the string representation of `x`.

## fn `unix(x: Datetime) -> float`

Returns the unix timestamp corresponding to `x`.