# Runtime Errors
Some operations in the language can cause runtime errors. These errors are values that can be used and stored in variables as normal. Every error contains a string value that describes the error. The error value is a special type that can be used in place of any other type. For example:
```xray errors
let x = 1 / 0;  // division by zero
```
For nearly all functions, calling them with an error value as an argument will return the error instead of evaluating the function. Likewise, attempting to construct a `struct` or `union` with error values will result in an error. The only exceptions to this rule are:
* [short-circuiting operators](./functions.md#short-circuiting-functions): if the error value is not used in the function flow, the error will not be used.
* [error handling functions](#error-handling-functions)

## Error Handling Functions
Special functions are the [`is_error`](../std/generic.md#is_error), [`if_error`](../std/generic.md#if_error), and [`get_error`](../std/generic.md#get_error) functions. These functions allow you to handle errors in a more controlled manner.
```xray
let x = 1 / 0;
assert(is_error(x))
&& assert(if_error(x, 6.5) == 6.5)
&& assert(get_error(x) == "division by zero")
```