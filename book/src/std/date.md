# Date

## struct `Date(year: int, month: int, day: int)`

A date in the Gregorian calendar.

## fn `add(x: Date, y: Duration) -> Date`

Returns the date `y` days after `x`. If `y` is negative, returns the date `y` days before `x`. If `y` has parts smaller than a day, they are ignored.

## fn `date(jd: int) -> Date`

Returns the date corresponding to the Julian day `jd`.

## fn `cmp(x: Date, y: Date) -> int`

Compares dates `x` and `y`.

## fn `eq(x: Date, y: Date) -> bool`

Returns `true` if `x` and `y` are equal.

## fn `format(x: Date, format: str) -> str`

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
* `%%` - the character `%`

## fn `julian_day(x: Date) -> int`

Returns the Julian day corresponding to `x`.

## fn `sub(x: Date, y: Date) -> Duration`

Returns the duration between `x` and `y`. If `x` is before `y`, the duration will be negative.

## fn `to_str(x: Date) -> str`

Returns the string representation of `x`.

## fn `weekday(x: Date) -> int`

Returns the weekday of `x`, as a one-digit number (where monday is 0).