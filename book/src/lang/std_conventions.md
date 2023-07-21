# The Standard Library Conventions
The following conventions are used throughout the standard library. Users are encouraged to follow these conventions in their own code.
## Naming Conventions
All variable and function names are written in `snake_case`. All `struct`, `union`, type names, as well as `type` aliases are written in `PascalCase`. All `union` variants and `struct` fields are written in `snake_case`. All primitive types (such as `int`, `float`, `bool`, etc.) are written in `lowercase`. All generic parameters are written in `UPPERCASE`.

## Privacy
XRay has no notion of private or public members. Instead, the convention is to prefix private members and functions with two underscores and the name of the owner of the private member. For instance, all members of the standard library beginning with `__std_` are private to the standard library. This convention is also used to ensure a value is not overloaded.

Likewise, any member, variable, or function beginning with an underscore (`_`) should not be used by users. These are reserved for internal use by the developer of these items, and are not guaranteed to behave predictably.

## Comparisons
Some standard library functions make use of comparison functions, that accept two values and return an integer. If the integer is positive, the first value is greater than the second. If the integer is negative, the first value is less than the second. If the integer is zero, the two values are equal.

## Hash
Some standard library functions make use of hash functions, that accept a value and return an integer. The hash function should return the same value for two values that are equal according to the comparison function. The hash of any value should always be between zero and 2^64-1 (inclusive)

## Formatting

Formatting specifier strings in the standard library unified under the following format, they are all of the form:

`[[fill]align][sign][#][0][width][grouping][.precision][mode]`

Where:
* `align` specifies the designed padding alignment. Options:
    * `>` - right aligned. This is the default unless "zero-padded" is specified.
    * `=` - padded after the sign for numeric types. This is the default if "zero-padded" is specified.
    * `<` - left aligned
    * `^` - centered
* `fill` specifies the character used for padding. Default is a space if "zero-padded" is not specified, and `0` if it is.
* `sign` specifies the sign to use for numeric types. Options:
    * `-` - only show the sign if the number is negative. This is the default.
    * `+` - always show the sign
    * ` ` (space) - only show the sign if the number is negative, but add a space if the number is positive.
* `#` specifies the alternate form. Each type, as well as formatting mode, can support its own alternative form.
* `0` specifies "zero-padded". Sets the fill character to `0` and the alignment to `=`.
* `width` specifies the minimum width of the formatted string. If the formatted string is shorter than this, it will be padded with the fill character. If the formatted string is longer than this, it will not be truncated.
* `grouping` specifies the grouping of digits. This is only supported for numeric types. Options:
    * `_` - use an underscore to separate groups of digits.
    * `,` - use a comma to separate groups of digits.
* `precision` specifies the number of digits to show after the decimal point. This is only supported for numeric types.
* `mode` specifies the formatting mode. Each type can support its own formatting modes.


Note that an empty string is a valid format string, and will simply return the string representation of the number. For all types implement `format` and `to_str`, it is guaranteed that `format(x, "") == to_str(x)`.