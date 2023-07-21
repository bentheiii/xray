# Set

## type `Set<T>`

An unordered hash-map of items of type `T`. Each mapping has a hash and equality function associated with it, which are used to determine whether two items are equal and to compute the hash of an item.

## fn `add<T>(x: Set<T>, item: T) -> Set<T>`

Returns a new set with `item` added, with the same hash and equality functions as `x`. If `item` is already present, returns `x`.

## fn `bit_and<T>(x: Set<T>, y: Set<T>) -> Set<T>`

Returns a new set with the items that are present in both `x` and `y`, with the same hash and equality functions as `x`.

## fn `bit_or<T>(x: Set<T>, y: Set<T>) -> Set<T>`

Returns a new set with the items that are present in either `x` or `y`, with the same hash and equality functions as `x`.

## fn `bit_xor<T>(x: Set<T>, y: Set<T>) -> Set<T>`

Returns a new set with the items that are present in either `x` or `y`, but not both, with the same hash and equality functions as `x`.

## fn `clear<T>(x: Set<T>) -> Set<T>`

Returns a new set with no items, with the same hash and equality functions as `x`.

## fn `contains<T>(x: Set<T>, item: T) -> bool`

Returns whether `x` contains `item`.

## fn `discard<T>(x: Set<T>, item: T) -> Set<T>`

Returns a new set with `item` removed, with the same hash and equality functions as `x`. If `item` is not present, returns `x`.

## fn `eq<T>(x: Set<T>, y: Set<T>) -> bool`

Returns whether `x` and `y` contain the same items.

~~~admonish warning
Checking for equality for two sets that have different hash or equality functions can produce unexpected results.
```xray
let s1 = set<int>().add(3);
let s2 = set((x: int)->{x%3}, (a: int, b: int)->{a%3==b%3}).add(0);

assert(s1 == s2)
&& assert(s2 != s1)
```
Therefore, it is highly recommended to use the same hash and equality functions for sets that are expected to be compared.
~~~

## fn `ge<T>(x: Set<T>, y: Set<T>) -> bool`

Returns whether `x` contains all the items in `y`.

## fn `gt<T>(x: Set<T>, y: Set<T>) -> bool`

Returns whether `x` contains all the items in `y`, and `x` and `y` are not equal.

## fn `hash<T>(x: Set<T>) -> int`

Returns the hash of `x`.

## fn `is_disjoint<T>(x: Set<T>, y: Set<T>) -> bool`

Returns whether `x` and `y` have no items in common.

## fn `le<T>(x: Set<T>, y: Set<T>) -> bool`

Returns whether `y` contains all the items in `x`.

## fn `len<T>(x: Set<T>) -> int`

Returns the number of items in `x`.

## fn `lt<T>(x: Set<T>, y: Set<T>) -> bool`

Returns whether `y` contains all the items in `x`, and `x` and `y` are not equal.

## fn `remove<T>(x: Set<T>, item: T) -> Set<T>`

Returns a new set with `item` removed, with the same hash and equality functions as `x`. If `item` is not present, returns an error.

## fn `set<T>(hash_: (T)->(int), eq_: (T,T)->(bool)) -> Set<T>` {#set2}

Returns a new set with the given hash and equality functions.

## dyn fn `set<T>()->Set<T>`
Where `hash(T)->int`, `eq(T,T)->bool`

A dynamic variant of [`set`](#set2), using the default hash and equality functions for `T`.

## fn `sub<T>(x: Set<T>, y: Set<T>) -> Set<T>`

Returns a new set with the items that are present in `x` but not `y`, with the same hash and equality functions as `x`.

## fn `to_array<T>(x: Set<T>) -> Sequence<T>`

Returns a sequence containing the items in `x`, in an arbitrary order.

## fn `to_generator<T>(x: Set<T>) -> Generator<T>`

Returns a generator that yields the items in `x`, in an arbitrary order.

## fn `update<T>(x: Set<T>, items: Generator<T>) -> Set<T>`

Returns a new set with the items in `items` added, with the same hash and equality functions as `x`.

## fn `update<T>(x: Set<T>, items: Sequence<T>) -> Set<T>`

Returns a new set with the items in `items` added, with the same hash and equality functions as `x`.