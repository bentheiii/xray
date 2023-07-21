# JSON

## union `JSON(number: float, bool: bool, string: str, null: (), array: Sequence<JSON>, object: Mapping<str, JSON>)`

An arbitrary JSON value.

## fn `eq(x: JSON, y: JSON) -> bool`

Returns `true` if `x` and `y` are equal.

## fn `json(x: float) -> JSON`

Returns a JSON value representing `x`.

## fn `json(x: int) -> JSON`

Returns a JSON value representing `x`.

## fn `json(x: bool) -> JSON`

Returns a JSON value representing `x`.

## fn `json(x: str) -> JSON`

Returns a JSON value representing `x`.

## fn `json(x: ()) -> JSON`

Returns a JSON value representing `x` (null).

## fn `json(x: Sequence<JSON>) -> JSON`

Returns a JSON value representing `x`.

## fn `json(x: Mapping<str, JSON>) -> JSON`

Returns a JSON value representing `x`.

## dyn fn `json<T>(x: Sequence<T>) -> JSON`
Where `json(T)->JSON`

Returns a JSON value representing `x`.

## dyn fn `json<T>(x: Mapping<str, T>) -> JSON`
Where `json(T)->JSON`

Returns a JSON value representing `x`.

## dyn fn `json<T>(x: Optional<T>) -> JSON`
Where `json(T)->JSON`

Returns a JSON value representing `x`, where a `None` value is represented as `null`.

## fn `json_deserialize(x: str) -> JSON`

Deserializes a JSON value from `x`, if `x` is not valid JSON, returns an error.

## fn `serialize(x: JSON) -> str`

Serializes a JSON value to a string.