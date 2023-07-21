# Optional

## type `Optional<T>`

A type that may or may not contain a value of type `T`. An optional that does not contain a value is usually denoted as `None` and can be created with the [`none()` function](#none), while an optional that does contain a value is usually denoted as `Some(value)` and can be created with the [`some(value)` function](#some).

## fn `and<T>(x: Optional<T>, y: Optional<T>) -> Optional<T>`

Returns `y` if `x` is not `None`, otherwise returns `None`. This function is short-circuiting for `y`, and does not evaluate `y` if `x` is `None`.

## dyn fn `eq<T0, T1>(a: Optional<T0>, b: Optional<T1>) -> bool`
Where `eq(T0, T1)->bool`

Returns `true` if `a` and `b` are both `None`, or if `a` and `b` are both `Some` and `a.value()` is equal to `b.value()`.


## fn `has_value<T>(x: Optional<T>) -> bool`

Returns whether `x` has some value.

## dyn fn `hash<T>(x: Optional<T>) -> int`
Where `hash(T)->int`

Returns the hash of the optional value `x`.

## fn `map<T,U>(x: Optional<T>, func: (T)->(U)) -> Optional<U>`

Returns `None` if `x` is `None`, otherwise returns `some(func(x.value()))`. This function is short-circuiting for `func`, and does not evaluate `func` if `x` is `None`.

## fn `map_or<T,U>(x: Optional<T>, func: (T)->(T), default: T) -> T`

Returns `default` if `x` is `None`, otherwise returns `func(x.value())`. This function is short-circuiting for `func`, and `default` and will only evaluate one of them.

## fn `none() -> Optional<?>` {#none}

Returns `None`, an optional without a value.

## fn `or<T>(x: Optional<T>, default: Optional<T>) -> Optional<T>`

Returns `default` if `x` is `None`, otherwise returns `x`. This function is short-circuiting for `default`, and does not evaluate `default` if `x` is `Some`.

## fn `or<T>(x: Optional<T>, default: T) -> T`

Returns `default` if `x` is `None`, otherwise returns `x.value()`. This function is short-circuiting for `default`, and does not evaluate `default` if `x` is `Some`.

## fn `some<T>(x: T) -> Optional<T>` {#some}

Returns `Some(x)`, an optional with the value `x`.

## dyn fn `to_str<T>(x: Optional<T>) -> str`
Where `to_str(T)->str`

Returns a string representation of the optional value `x`.

## fn `value<T>(x: Optional<T>, msg: str?) -> T`

Returns the value of `x` if `x` is `Some`, otherwise returns an error, optionally with the error message `msg`.