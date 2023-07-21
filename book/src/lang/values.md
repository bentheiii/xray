# Values and Variables
The most primitive part of an XRay program is a value. Each value has a type, and a value that corresponds to that type. For example, the literal 42 produces a value of type `int` and value `42`.
```xray
42 // this will produce the value 42 of type int
```
Values can also be created by calling functions and operators, or by specifying already stored variables.
```xray
21.1 + 23.1                  // will result in the value 44.2 of type float
"banana".starts_with("nana") // will result in value false of type bool
pi  // a variable in the standard library, will result in a value of type float

// a function in a standard library, will result in a variable of type callable
factorial  
```
All values in XRay are:
* Immutable: once created, a value cannot be changed.
* Reference Counted: values are automatically freed when they are no longer used anywhere, and can be used and copied cheaply.

## Variables
Values can be stored in variables with the `let` keyword.

```xray
/* The variable a will be assigned the value 3, its type is inferred to be "int" */
let a = 3;
/* a type can be explicitly stated. Compilation will fail if the
   variable type and the value type mismatch */
let b: str = "hello world!";
let c = b * a;  // "hello world!hello world!hello world!"
```