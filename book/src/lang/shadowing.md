# Overloads, Shadowing, and Forward Functions
## Overloads
When multiple functions of the same name are declared, these functions are aggregated into an overloaded function. When an overloaded function is called, the compiler will attempt to find the best match for the arguments passed to the function call. If no match is found, the compiler will throw an error.

```xray
fn foo(x: int) -> int {
    // overload 1
    x + 1
}

fn foo(x: float) -> float {
    // overload 2
    x + 1.0
}

let x = foo(1);  // overload 1 is called
let y = foo(1.0);  // overload 2 is called
```

### Overload Resolution

When an overloaded function is called with a set of arguments, the compiler will search the overloads in the following order:
* First, all the non-generic overloads are considered. If any of these overloads match the arguments, the search is complete and the matching overload is selected.
* If no non-generic overloads match, all the [generic](./lambda_functions.md#generic-functions) overloads are considered. If any of these overloads match the arguments, the search is complete and the matching overload is selected.
* If no generic overloads match, all the [dynamic function](./dyn_functions.md) overloads are considered. If any of these overloads match the arguments, the search is complete and the matching overload is selected.
* If no dynamic overloads match, the search fails and the compiler throws an error.

### Specific Overload Selection

An overloaded function cannot be used as a value, except by calling it.

```xray does_not_compile
fn square(x: int) -> int {
    x*x
}

fn square(x: float) -> float {
    x*x
}

let squares = range(10).map(
    square  // this will fail to compile, because foo is overloaded
);
```

Instead, we can specify a specific overload to use by using brackets after the function name, and specifying the types of the arguments.

```xray
fn square(x: int) -> int {
    x*x
}

fn square(x: float) -> float {
    x*x
}

let squares = range(10).map(
    square{int}  // this will use the int overload
);
```

### Overload Conflict
In some cases, two overloads can both match a call, in these cases the compiler will throw an error.

```xray does_not_compile
fn inc(x: int, a: int ?= 1) -> int {
    // overload 1
    x + a
}

fn inc(x: int, a: float ?= 1.0) -> float {
    // overload 2
    x.to_float() + a
}

let x = inc(1, 1);  // will call overload 1
let y = inc(1, 1.0);  // will call overload 2
let z = inc(1);  // this will fail to compile, because both overloads match
```
## Variable Shadowing

Shadowing is the process of declaring a new variable with the same name as an existing variable. This new variable will shadow the existing variable, and the existing variable will no longer be accessible.

```xray
let x = 1;
let x = 2;  // this shadows the previous x
```

Shadowing can be used to change the type of a variable.

```xray
let x = 1;
let x = 2.0;  // this shadows the previous x
```

## Forward Functions

Functions cannot be use until they are declared. The exception to this rule is when a function is declared with the `forward` keyword. A forward function is a function that is declared without a body, and is used to declare a function before it is defined.

```xray
forward fn foo(x: int) -> int;

fn main() {
    foo(1)  // this will compile, even though foo is not defined yet
}

fn foo(x: int) -> int {
    x + 1
}

main()  // this will also compile
```

Note that any function that uses a forward function before it is defined is also a forward function, and cannot be called until all its forward dependencies are fulfilled.


```xray does_not_compile
forward fn foo(x: int) -> int;

fn main() {
    // since main uses foo, it cannot be called until foo is defined
    foo(1)
}

main()  // this will fail to compile, because main is a forward function

fn foo(x: int) -> int {
    x + 1
}
```