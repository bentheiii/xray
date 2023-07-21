# errors

## fn `error(msg: str)->?`

Returns an error with error message `msg`.

## fn `get_error<T>(x: T)->Optional<str>`

Returns the error message of `x`, if it is an error, otherwise returns `none`.

## fn `is_error<T>(x: T)->bool`

Returns whether `x` is an error.

## fn `if_error<T>(x: T, if_error: T)->T`

Returns `x` if it is not an error, otherwise returns `if_error`. This function is short-circuiting for `is_error`, and will only evaluate it if `x` is an error.

## fn `if_error<T>(x: T, msg: str, if_error: T)->T`

Returns `x` if it is not an error, if it is an error, and the error message contains `msg`, will return the `if_error`. If `x` is an error and the error message does not contain `msg`, will return the error. This function is short-circuiting for `if_error`, and will only evaluate it if needed.

```xray
let val1 = error("I am a teapot");
let val2 = error("I am a toaster");
let val3 = 300;

assert(
    if_error(val1, "teapot", 100) == 100  // we caught the error
)
&& assert(is_error(
    if_error(val2, "teapot", 100)  // we didn't catch the error
))
&& assert(
    if_error(val3, "teapot", 100) == 300  // no error to catch
)
```