# Mapping

## type `Mapping<K,V>`

An unordered hash-map of keys of type `K` and values of type `V`. Each mapping has a hash and equality function associated with it, which are used to determine whether two keys are equal and to compute the hash of a key.

## fn `clear<K>(x: Mapping<K,?>) -> Mapping<K,?>`

Returns a new mapping with no keys, with the same hash and equality functions `x`.

## fn `contains<K,V>(x: Mapping<K,V>, k: K) -> bool`

Returns whether `x` contains `k`.

## fn `counter_mode<K>(m: Mapping<K,int>) -> (Sequence<K>, int)`

Returns the keys with the highest value in `x`, and the value itself.

## fn `discard<K,V>(x: Mapping<K,V>, k: K) -> Mapping<K,V>`

Returns a new mapping with `k` removed, with the same hash and equality functions as `x`. If `k` is not present, returns `x`.

## dyn fn `eq<K, V>(a: Mapping<K,V>, b: Mapping<K,V>) -> bool`
Where `eq(V, V)->bool`.

Returns whether `a` and `b` are equal. Two mappings are considered equal if they have exactly the same keys, and the values for each key are equal for both mappings.

~~~admonish warning
Checking for equality for two mappings that have different hash or equality functions can produce unexpected results.
```xray
let m1 = mapping<int>().set(3, 4);
let m2 = mapping((x: int)->{x%3}, (a: int, b: int)->{a%3==b%3}).set(0, 4);

assert(m1 == m2)
&& assert(m2 != m1)
```
Therefore, it is highly recommended to use the same hash and equality functions for mappings that are expected to be compared.
~~~

## fn `get<K,V>(x: Mapping<K,V>, k: K) -> V`

Returns the value associated with `k` in `x`. If `k` is not present in `x`, returns an error.

## fn `get<K,V>(x: Mapping<K,V>, k: K, default: V) -> V`

Returns the value associated with `k` in `x`. If `k` is not present in `x`, returns `default`. This function is short-circuiting for `default`, and will only evaluate it if `k` is not present in `x`.

## dyn fn `hash<K, V>(x: Mapping<K,V>) -> int`
Where `hash(V)->int`.

Returns the hash of `x`.

## fn `keys<K,V>(x: Mapping<K,V>) -> Generator<K>`

Returns a generator of all the keys in `x`.

## fn `len<K>(x: Mapping<K,?>) -> int`

Returns the number of keys in `x`.

## fn `lookup<K,V>(x: Mapping<K,V>, k: K) -> Optional<V>`

Returns the value associated with `k` in `x`. If `k` is not present in `x`, returns `none`.

## fn `map_values<K,V,W>(x: Mapping<K,V>, func: (V)->(W)) -> Mapping<K,W>`

Returns a new mapping with the same hash and equality functions as `x`, where each value is mapped using `func`.

## fn `mapping<K>(hash_: (K)->(int), eq_: (K,K)->(bool)) -> Mapping<K,?>`  {#mapping2}

Returns a new mapping with the given `hash_` and `eq_` functions.

## dyn fn `mapping<K>() -> Mapping<K,?>`
Where `hash(K)->int`, `eq(K,K)->bool`.

A dynamic variant of [mapping](#mapping2), using inferred hash and equality functions.

## fn `pop<K,V>(x: Mapping<K,V>, k: K) -> Mapping<K,V>`

Returns a new mapping with `k` removed, with the same hash and equality functions as `x`. If `k` is not present, returns an error.

## fn `set<K,V>(x: Mapping<K,V>, k: K, v: V) -> Mapping<K,V>`

Returns a new mapping with `k` set to `v`, with the same hash and equality functions as `x`. If `k` is already present, it is overwritten.

## fn `set_default<K,V>(x: Mapping<K,V>, k: K, v: V) -> Mapping<K,V>`

Returns a new mapping with `k` set to `v`, with the same hash and equality functions as `x`. If `k` is already present, it is not overwritten.

## fn `to_generator<K,V>(x: Mapping<K,V>) -> Generator<(K,V)>`

Returns a generator of all the key-value pairs in `x`.

## fn `update<K,V>(x: Mapping<K,V>, items: Generator<(K,V)>) -> Mapping<K,V>`

Returns a new mapping with the key-value pairs in `items` added, with the same hash and equality functions as `x`. If a key is already present, it is overwritten.

## fn `update<K,V>(x: Mapping<K,V>, y: Mapping<K,V>) -> Mapping<K,V>`

Returns a new mapping with the key-value pairs in `y` added, with the same hash and equality functions as `x`. If a key is already present, it is overwritten.

## fn `update_counter<K>(x: Mapping<K,int>, keys: Generator<K>) -> Mapping<K,int>`

Returns a new mapping with the keys in `keys` added to counter `x`, with the same hash and equality functions as `x`. If a key is already present, its value is incremented.

## fn `update_from_keys<K,V>(x: Mapping<K,V>, keys: Generator<K>, on_vacant: (K)->(V), on_occupied: (K,V)->(V)) -> Mapping<K,V>`  {#update_from_keys_gen}

Returns a new mapping with the keys in `keys` added to `x`, with the same hash and equality functions as `x`. If a key is already present, its value is updated using `on_occupied`. If a key is not present, its value is updated using `on_vacant`.

## fn `update_from_keys<K,V>(x: Mapping<K,V>, keys: Sequence<K>, on_vacant: (K)->(V), on_occupied: (K,V)->(V)) -> Mapping<K,V>`

A variant of [update_from_keys](#update_from_keys_gen) that takes a sequence of keys instead of a generator.

## fn `values<K,V>(x: Mapping<K,V>) -> Generator<V>`

Returns a generator of all the values in `x`.