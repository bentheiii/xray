# Functions
XRay supports code reuse and nesting with functions. Functions can be declared with the `fn` keyword.
```xray
fn triple_minus_seven(x: int)->int{
    ...
}
/* we have now defined a function called "triple_minus_seven" that accepts one integer
   parameter and returns an integer value */
```

The body of a function can include any number of declarations (including variables, other functions, and even new types), and must end with the value that should be returned.

```xray
fn triple_minus_seven(x: int)->int{
    let triple = x*3;
    x - 7  // this value will be returned to the caller
}
```

Functions can be called by placing parenthesis after their name.

```xray
fn triple_minus_seven(x: int)->int{
    let triple = x*3;
    x - 7  // this value will be returned to the caller
}

let triple_5_minus_seven = triple_minus_seven(5);
/* here we call the function we defined */

assert(triple_5_minus_seven == 8)  // here call built-in function "assert"
```

Note that we can only call functions after they are declared.

```xray does_not_compile
/* this will fail to compile, because the function is not yet declared */
let triple_5_minus_seven = triple_minus_seven(5);

fn triple_minus_seven(x: int)->int{
    let triple = x*3;
    x - 7  // this value will be returned to the caller
}
```
## Optional Parameters
Parameters can be made optional by giving them a default value. Optional parameters must be placed after all non-optional parameters.

```xray
fn triple_minus_y(x: int, y: int ?= 7)->int{
    let triple = x*3;
    x - y  // this value will be returned to the caller
}

/* calling without specifying y will use the default value */
assert(triple_minus_y(5) == 8)
&& assert(triple_minus_y(5, 2) == 13)
```

Note that the default values are singletons, their expressions are only evaluated once when the function is declared.

```xray
fn triple_minus_y(x: int, y: int ?= uniform_distribution(1,6).random())->int{
    /* the default value of y will be the same random roll for all
       calls to this function */
    let triple = x*3;
    x - y  // this value will be returned to the caller
}

assert(triple_minus_y(5) == triple_minus_y(5))
```
## Methods
Any function can also be called as a method with the dot (`.`) operator. The instance of the type will be passed as the first parameter to the function. Therefore, the invocation `x.f(y)` is equivalent to `f(x, y)`.

```xray
fn triple_minus_y(x: int, y: int)->int{
    let triple = x*3;
    x - y
} 

let x = 5;
assert(x.triple_minus_y(2) == 13)
```

This extends to all functions with any number of parameters. For example, the `if` function can also be used as a method on its first parameter, the condition.

```xray
let x = 5;

let y = (x > 3).if(
    x - 3,  // will be returned if x > 3
    x + 3   // will be returned if x <= 3
);
assert(y == 2)
```
## Operators
XRay supports operators simply as aliases of functions. For example, the `+` operator is just an alias for the `add` function, so `a + b` results in the same compiled program as `add(a,b)` (or indeed, `a.add(b)`). With this, users can override operators for new or existing types simply by defining the corresponding function.

```xray
/* substracting strings is not supported by the build-in library, but users
   can define it. */
fn sub(a: str, b: str)->str{
    (a.to_int() - b.to_int()).to_str()
    // note that this implementation will fail if the strings are not valid integers
}

assert("5" - "3" == "2")
```

XRay supports the following binary operators (and resolves them in the following order):
* `**`: `pow`
* `*`: `mul`
* `/`: `div`
* `%`: `mod`
* `+`: `add`
* `-`: `sub`
* `|`: `bit_or`
* `&`: `bit_and`
* `^`: `bit_xor`
* `<=`: `le`
* `<`: `lt`
* `>=`: `ge`
* `>`: `gt`
* `==`: `eq`
* `!=`: `ne`
* `&&`: `and`
* `||`: `or`

XRay supports the following unary operators:
* `!`: `not`
* `-`: `neg`
* `+`: `pos`

XRay also supports the indexing operator (`[]`) as an alias for the `get` function. For example, `a[b]` is equivalent to `get(a, b)`. This allows users to override the indexing operator for new or existing types simply by defining the corresponding function.

## Short-Circuiting Functions
User functions, as well as most built-in functions, as not short circuiting. This means that all parameters are evaluated before the function is called, and if any of the parameters produce an error, the function being called will also produce an error. For example:

```xray errors
fn triple_minus_seven(x: int, unused: int)->int{
    let triple = x*3;
    x - 7  // this value will be returned to the caller
}

/* even though the second parameter is unused, it will still be evaluated,
   and will produce an error that will prevent the function from being called */
let triple_5_minus_seven = triple_minus_seven(5, floor(1/0)); // ERROR
```

However, some built-in functions are short-circuiting. For example, the `and` function will only evaluate its second parameter if the first parameter is true. This allows us to write code like this:

```xray
fn is_even(x: int)->bool{
    x % 2 == 0
}

let x = is_even(5) && is_even(floor(1/0));
/* the second call to is_even will not be evaluated, because the first call
   returned false, and the && function is short-circuiting */
assert(x == false)
```

In general, users should assume that functions are not short-circuiting, unless the documentation explicitly states otherwise.

## Recursion
XRay supports recursion, this allows for functions to call themselves. For example, the following function will calculate the sum of the first `n` square numbers:

```xray
fn sum_of_squares(n: int)->int{
    (x == 0).if(
        0,  // base case
        n*n + sum_of_squares(n - 1)  // recursive case
    )
}

assert(sum_of_squares(5) == 55)
```
### Tail-Call Optimization
XRay supports tail-call optimization. This means that if a function returns the result of a recursive call, the compiler will optimize the code so that the stack frame of the first function is reused for the second function. This allows for recursion to be used without the risk of stack overflow.

```xray
fn sum_of_squares(n: int, ret: int ?= 0)->int{
    (x == 0).if(
        ret,  // base case
        sum_of_squares(n - 1, ret + n*n)  // tail-recursive case
    )
}

assert(sum_of_squares(5) == 55)
```

## Closures
Functions can be defined inside other functions. These functions can access the variables of the function they are defined in, and can be returned from the function they are defined in. For example, the following function returns a closure that adds `x` to its parameter:

```xray
fn adder(x: int)->fn(int)->int{
    fn add(y: int)->int{
        x + y
    }
    add
}