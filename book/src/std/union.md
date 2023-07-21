# union

## dyn fn `members<T>(x: T) -> (Optional<T0>, Optional<T1>, ..., Optional<Tn>)`
Where `T` is a union type.

Returns a tuple of the variants of `x`, where exactly one member is ever not `none`.

```xray
union Label(
    Tag: str,
    ID: int,
    anonymous: (),
)

let lb = Label::ID(15);

let as_tuple = members(p);

assert(as_tuple == (none(), some(15), none()))
```