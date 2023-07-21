# struct

## dyn fn `members<T>(x: T) -> (T0, T1, ..., Tn)`
Where `T` is a struct type.

Returns a tuple of the members of `x`, in the order they were defined.

```xray
struct Person(
    name: str,
    age: int,
    height: float,
    weight: float,
)

let p = Person("bob", 20, 1.8, 70.0);

let as_tuple = members(p);

assert(as_tuple == ("bob", 20, 1.8, 70.0))
```