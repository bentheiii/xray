# tuple

## fn `and<T>(a: (), b: T)->T`

Returns `b`. This is useful for chaining together functions that return a nil value.

## dyn fn `cmp(a: (T0, T1, ..., Tn), b: (U0, U1, ..., Un))->int`
Where `cmp(Ti, Ui) -> int` for all `i`.

Returns a lexigraphical comparison of `a` and `b`. The first element of each tuple is compared, and if they are not equal, the result is returned. Otherwise, the second element is compared, and so on. If all elements are equal, `0` is returned.

## dyn fn `eq(a: (T0, T1, ..., Tn), b: (U0, U1, ..., Un))->bool`
Where `eq(Ti, Ui) -> bool` for all `i`.

Returns whether `a` and `b` are equal. Each element is compared with the corresponding element in the other tuple. If all elements are equal, `true` is returned.

## dyn fn `hash(x: (T0, T1, ..., Tn))->int`
Where `hash(Ti) -> int` for all `i`.

Returns a hash of `x`.

## dyn fn `to_str(x: (T0, T1, ..., Tn))->str`
Where `to_str(Ti) -> str` for all `i`.

Returns a string representation of `x`. Each element is converted to a string and concatenated together, separated by commas and surrounded by parentheses.