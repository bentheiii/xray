# Dynamic Functions
Dynamic functions are functions provided by the standard library, that utilize compile-time reflection to provide dynamic functionality. These functions are not available in the language itself, but are available in the standard library. All the dynamic functions in the standard library are documented in the [standard library reference](../std/intro.md). For example, consider the non-dynamic `min` function:
```xray
fn min<T>(g: Generator<T>, lt: (T,T)->(bool)) -> T {
    ...
}
```

Since XRay does not know how to compare two arbitrary types, the `lt` function must be provided by the caller. However, `min` also has a dynamic version, that looks up the `lt` function at compile time:
```xray
let arr = [5, 9, 11, 3, 2, 7];
let m = arr.to_generator().min();  // dynamic invocation
assert(m == 2)
```

```admonish warning
Dynamic functions demand that the type of the argument be known at compile time. Therefore, they cannot be used with generic types.
```

## Dynamic specialization

Some dynamic functions can accept types as parameters and specialize themselves based on the type. Calling dynamic specialization is done with angle brackets between the function name and parameters. For example, the dynamic `mapping` function requires a type as a parameter, and automatically uses the `hash` and `eq` functions for that type:
```xray
let m = mapping<int>();  // dynamic specialization, results in a Mapping<int, Unknown>
let m1 = m.set(5, "five");
```