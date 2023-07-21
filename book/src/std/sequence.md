# Sequence

## type `Sequence<T>`

A linear sequence of items of type `T`. The items are ordered, and may contain duplicates.

~~~admonish note title="Sequence Implementation"
While a sequence can be an "array" (a contiguous block of memory), it can also be a lazy sequence, or a multiple chained sequences, or many other internal representations. The only requirement is that the items are ordered, and can be accessed by index in constant time. XRay makes no promises about which internal representation is used for each operation. In order to force a sequence to be an array, use [`to_array`](#to_array).

```xray
let s0 = [1, 2, 3];  // s0 is an array
let s1 = s0.map((x: int)->{x*2});  // s1 is a lazy sequence, no new
                                   // data is allocated
let s2 = range(1, 10000);  // s2 is a lazy range of numbers, no new 
                           // data is allocated
let s3 = s1 + s2;  // s3 is a chained sequence, no new data is allocated
let s4 = s3.to_array();  // data will be allocated for s4, since we force 
                         // it to be an array
let s5 = s3.push(1);  // this will force the entire sequence into an array
```
~~~

```admonish info title="infinite sequences"
Some sequences can be infinite. For example, [`count`](#count) can be used to create an infinite sequence of numbers. Infinite sequences can be used in many places where a finite sequence is expected, but not all. For example, [`len`](#len) will return an error if given an infinite sequence.
```

```admonish warning title="immutable sequences"
In other languages, some sequence operations are more efficient than others. In XRay, since sequences are immutable, all operations are equally efficient. For example, [`push`](#push) is just as efficient as [`rpush`](#rpush) or [`insert`](#insert). For a way to aggregate values more efficiently, see the [Stack type](./stack.md).
```

```admonish note title="indices"
XRay sequences are indexed starting at 0. This means that the first element of a sequence is at index 0, the second element is at index 1, and so on. This is different from some other languages, where the first element is at index 1.

In all places whare an index can be provided, a negative index can also be provided. A negative index is interpreted as an index from the end of the sequence. That is, `-1` is the last element of the sequence, `-2` is the second to last element, and so on.
```

## fn `add<T>(x: Sequence<T>, y: Sequence<T>) -> Sequence<T>`

Returns a new sequence that is the concatenation of `x` and `y`.

## fn `add<T>(x: Sequence<T>, y: Stack<T>) -> Sequence<T>`

Returns a new sequence that is the concatenation of `x` and `y`, `y` is inserted in reverse insertion order.

```xray
let seq = [1,2,3];
let stack = stack().push(4).push(5).push(6);

let seq2 = add(seq, stack);

assert(seq2 == [1,2,3,6,5,4])
```

## fn `add_rev<T>(x: Sequence<T>, y: Stack<T>) -> Sequence<T>`

Returns a new sequence that is the concatenation of `x` and `y`, `y` is inserted in insertion order.

```xray
let seq = [1,2,3];
let stack = stack().push(4).push(5).push(6);

let seq2 = add_rev(seq, stack);

assert(seq2 == [1,2,3,4,5,6])
```

## fn `aggregate<T, U>(x: Sequence<T>, initial: U, f: (U, T)->(U)) -> Generator<U>`

Returns a generator that yields aggregated values of `x`. The first value yielded is `initial`, and each subsequent value is the result of calling `f` with the previous value and the next item in `x`.

## fn `aggregate<T>(x: Sequence<T>, f: (T, T)->(T)) -> Generator<T>`

Returns a generator that yields aggregated values of `x`. The first value yielded is the first item in `x`, and each subsequent value is the result of calling `f` with the previous value and the next item in `x`.

## fn `all<T>(x: Sequence<T>, pred: (T)->(bool)) -> bool`

Returns whether all items in `x` satisfy `pred`.

## fn `any<T>(x: Sequence<T>, pred: (T)->(bool)) -> bool`

Returns whether any item in `x` satisfies `pred`.

## fn `binary_search<T>(x: Sequence<T>, cmp_: (T)->(int)) -> Optional<int>`

For an `x` and `cmp_` where `cmp_(x[i])` is monotonically increasing returns the index of an item in `x` where `cmp_(x[i]) == 0`. If no such item exists, returns `None`.

## fn `bisect<T>(x: Sequence<T>, left_predicate: (T)->(bool)) -> int`

For an `x` and `left_predicate` where `left_predicate(x[i]) == (i < M)` for some `M`, returns `M`. That is, assuming that the sequence begins with a sequence of items that satisfy `left_predicate`, followed by a sequence of items that do not satisfy `left_predicate`, returns the number of items that satisfy `left_predicate`.

## dyn fn `cmp<T0, T1>(Sequence<T0>, Sequence<T1>)->int`
Where `cmp(T0, T1)->int`

Compares `x` and `y` lexicographically. Returns an integer less than, equal to, or greater than zero if `x` is lexicographically less than, equal to, or greater than `y`.

## fn `combination<T>(x: Sequence<T>, i: int, k: int) -> Sequence<T>`

Returns the `i`th combination of `x` of length `k`. Will return a subsequence of `x` of length `k`.

## fn `combinations<T>(x: Sequence<T>, k: int) -> Sequence<Sequence<T>>`

Returns all combinations of `x` of length `k`. The combinations will be subsequences of `x` of length `k`.

## fn `combination_with_replacement<T>(x: Sequence<T>, i: int, k: int) -> Sequence<T>`

Returns the `i`th combination with replacement of `x` of length `k`. Will return a subsequence (with replacement) of `x` of length `k`.

## fn `combinations_with_replacement<T>(x: Sequence<T>, k: int) -> Sequence<Sequence<T>>`

Returns all combinations with replacement of `x` of length `k`. The combinations will be subsequences (with replacement) of `x` of length `k`.

## fn `contains<T, U>(x: Sequence<T>, item: U, eq_: (T, U)->(bool)) -> bool`  {#contains3}

Returns whether `x` contains `item` according to `eq_`.

## dyn fn `contains<T, U>(x: Sequence<T>, item: U)->R`
Where `eq(T, U)->bool`, `contains(Sequence<T>, U, (T, U)->bool)->R`

A dynamic variant of [`contains`](#contains3), using the inferred equality operator.

## fn `count()->Sequence<int>`

Returns an infinite sequence of integers starting at 0 and increasing by 1.

## fn `count<T>(x: Sequence<T>, pred: (T)->(bool)) -> int`

Returns the number of items in `x` that satisfy `pred`.

## fn `count< T, U>(x: Sequence<T>, item: U, eq_: (T, U)->(bool)) -> int`  {#count3}

Returns the number of items in `x` that are equal to `item` according to `eq_`.

## dyn fn `count<T, U>(x: Sequence<T>, item: U)->R`
Where `eq(T, U)->bool`, `count(Sequence<T>, U, (T, U)->bool)->R`

A dynamic variant of [`count`](#count3), using the inferred equality operator.

## fn `enumerate<T>(x: Sequence<T>, start: int?, step: int?) -> Sequence<(int, T)>`

Returns a sequence of pairs of the form `(i, x[i])` for each item in `x`.

## dyn fn `eq<T0, T1>(Sequence<T0>, Sequence<T1>)->bool`
Where `eq(T0, T1)->bool`

Returns whether `x` and `y` are equal, both in length and in value, pair-wise.

## fn `filter<T>(x: Sequence<T>, pred: (T) -> (bool)) -> Generator<T>`

Returns a generator that yields the items in `x` that satisfy `pred`.

## fn `first<T>(x: Sequence<T>, func: (T) -> (bool)) -> Optional<T>`

Returns the first item in `x` that satisfies `func`, or `None` if no such item exists.

## dyn fn `geo_mean<T>(x: Sequence<T>)->R`
Where `product(Sequence<T>)->P`, `len(Sequence<T>)->L`, `div(1, L)->D`, `pow(P, D)->R`

Returns the geometric mean of `x`. 

## fn `get<T>(x: Sequence<T>, idx: int) -> T`

Returns the `idx`th item in `x`.

## dyn fn `harmonic_mean<T>(x: Sequence<T>)->R`
Where `div(int, T)->D`, `map(Sequence<T>, (T)->(D))->M`, `mean(M)->A`, `div(int, A)->R`

Returns the harmonic mean of the elements of `x`. The harmonic mean of a sequence \\(x_1, x_2, \ldots, x_n\\) is defined as:

\\[
\frac{n}{\frac{1}{x_1} + \frac{1}{x_2} + \ldots + \frac{1}{x_n}}
\\]

## dyn fn `hash<T>(Sequence<T>)->int`
Where `hash(T)->int`

Returns a hash of `x`.

## fn `is_infinite<T>(x: Sequence<T>) -> bool`

Returns whether `x` is an infinite sequence.

## fn `insert<T>(x: Sequence<T>, idx: int, item: T) -> Sequence<T>` {#insert}

Returns a copy of `x` with `item` inserted at index `idx`.

## fn `last<T>(x: Sequence<T>, func: (T) -> (bool)) -> Optional<T>`

Returns the last item in `x` that satisfies `func`, or `None` if no such item exists.

## fn `len<T>(x: Sequence<T>) -> int`

Returns the length of `x`. If the sequence is infinite, returns an error.

## fn `map<T,U>(x: Sequence<T>, f: (T)->(U)) -> Sequence<U>`

Returns a sequence of elements of the form `f(x[i])` for each item in `x`.

## fn `max<T>(x: Sequence<T>, lt_: (T, T)->(bool)) -> T`  {#max2}

Returns the maximum item in `x` according to `lt_`.

## dyn fn `max<T>(x: Sequence<T>)->R`

A dynamic variant of [`max`](#max2), using the inferred less-than operator.

## dyn fn `mean<T>(x: Sequence<T>)->R`
Where `sum(Sequence<T>)->S`, `len(Sequence<T>)->L`, `div(S, L)->R`

Returns the arithmetic mean of `x`.

## fn `median<T>(x: Sequence<T>, cmp_: (T, T)->(int)) -> T`  {#median2}

Returns the median of `x` according to `cmp_`.

## dyn fn `median<T>(x: Sequence<T>)->R`
Where `cmp(T, T)->int`, `median(Sequence<T>, (T, T)->int)->R`

A dynamic variant of [`median`](#median2), using the inferred comparison operator.

## fn `min<T>(x: Sequence<T>, lt_: (T, T)->(bool)) -> T`  {#min2}

Returns the minimum item in `x` according to `lt_`.

## dyn fn `min<T>(x: Sequence<T>)->R`

A dynamic variant of [`min`](#min2), using the inferred less-than operator.

## fn `mul<T>(x: Sequence<T>, n: int) -> Sequence<T>`

Returns a sequence containing `n` copies of `x`.

## fn `n_largest<T>(x: Sequence<T>, n: int, cmp_: (T, T)->(int)) -> Sequence<T>`  {#n_largest3}

Returns the `n` largest items in `x` according to `cmp_`.

## dyn fn `n_largest<T>(x: Sequence<T>, n: int)->R`
Where `cmp(T, T)->int`, `n_largest(Sequence<T>, int, (T, T)->int)->R`

A dynamic variant of [`n_largest`](#n_largest3), using the inferred comparison operator.

## fn `n_smallest<T>(x: Sequence<T>, n: int, cmp_: (T, T)->(int)) -> Sequence<T>`  {#n_smallest3}

Returns the `n` smallest items in `x` according to `cmp_`.

## dyn fn `n_smallest<T>(x: Sequence<T>, n: int)->R`
Where `cmp(T, T)->int`, `n_smallest(Sequence<T>, int, (T, T)->int)->R`

A dynamic variant of [`n_smallest`](#n_smallest3), using the inferred comparison operator.

## fn `nth<T>(x: Sequence<T>, n: int, pred: (T)->(bool)) -> Optional<T>`

Returns the `n`th item in `x` that satisfies `pred`, or `None` if no such item exists. If `n` is negative, returns the `n`th last item in `x` that satisfies `pred`.

## fn `nth_largest<T>(x: Sequence<T>, n: int, cmp_: (T, T)->(int)) -> T`  {#nth_largest3}

Returns the `n`th largest item in `x` according to `cmp_`.

## dyn fn `nth_largest<T>(x: Sequence<T>, n: int)->R`
Where `cmp(T, T)->int`, `nth_largest(Sequence<T>, int, (T, T)->int)->R`

A dynamic variant of [`nth_largest`](#nth_largest3), using the inferred comparison operator.

## fn `nth_smallest<T>(x: Sequence<T>, n: int, cmp_: (T, T)->(int)) -> T`  {#nth_smallest3}

Returns the `n`th smallest item in `x` according to `cmp_`.

## dyn fn `nth_smallest<T>(x: Sequence<T>, n: int)->T`
Where `cmp(T, T)->int`, `nth_smallest(Sequence<T>, int, (T, T)->int)->R`

A dynamic variant of [`nth_smallest`](#nth_smallest3), using the inferred comparison operator.

## fn `permutation<T>(x: Sequence<T>, i: int, k: int?) -> Sequence<T>`

Returns the `i`th permutation of `x` of length `k`. `k` defaults to `x.len()`.

## fn `permutations<T>(x: Sequence<T>, k: int?) -> Sequence<Sequence<T>>`

Returns a sequence of all permutations of `x` of length `k`. `k` defaults to `x.len()`.

## dyn fn `product<T, U>(x: Sequence<T>, initial: U)->R`
Where `mul(U, T)->U`, `reduce(Sequence<T>, U, (U, T)->U)->R`

Returns the product of `x` with `initial` as the initial value.

## fn `pop<T>(x: Sequence<T>, idx: int) -> Sequence<T>`

Returns a sequence containing all items in `x` except the item at `idx`.

## fn `push<T>(x: Sequence<T>, item: T) -> Sequence<T>`

Returns a sequence containing all items in `x` and `item`, at the last index.

## fn `random_choices<T>(x: Sequence<T>, k: int) -> Sequence<T>`
Requires `RANDOM` permissions.

Returns a sequence of `k` random items from `x`, with repetition.

## fn `random_choices<T>(x: Sequence<T>, k: int, weights: Sequence<float>) -> Sequence<T>`
Requires `RANDOM` permissions.

Returns a sequence of `k` random items from `x`, with repetition, with the probability of each item being chosen proportional to its weight. If `x` and `weights` are not the same length, an error is returned.

## fn `range(end: int)->Sequence<int>`

Returns a sequence of integers from `0` to `end` (exclusive). The resulting sequence has length `end`.

## fn `range(start: int, end: int, step: int?)->Sequence<int>`

Returns a sequence of integers from `start` to `end` (exclusive), with step `step`. If `step` is not provided, it defaults to `1`. If `step` is negative, the sequence is decreasing. If `start` is greater than `end` and `step` is positive, the sequence is empty. If `start` is less than `end` and `step` is negative, the sequence is empty.

## fn `rank_avg<T>(x: Sequence<T>, item: T, cmp_: (T, T)->(int)) -> float`  {#rank_avg3}

Returns the rank of `item` in `x` according to `cmp_`. If `item` appears in `x` multiple times, returns the average rank of the occurrences. If `item` is not in `x`, returns an error.

## dyn fn `rank_avg<T>(x: Sequence<T>, item: T)->R`
Where `cmp(T, T)->int`, `rank_avg(Sequence<T>, T, (T, T)->int)->R`

A dynamic variant of [`rank_avg`](#rank_avg3), using the inferred comparison operator.

## fn `rank_eq<T>(x: Sequence<T>, item: T, cmp_: (T, T)->(int)) -> int`  {#rank_eq3}

Returns the rank of `item` in `x` according to `cmp_`. If `item` appears in `x` multiple times, returns the rank of the first occurrence. If `item` is not in `x`, returns an error.

## dyn fn `rank_eq<T>(x: Sequence<T>, item: T)->R`
Where `cmp(T, T)->int`, `rank_eq(Sequence<T>, T, (T, T)->int)->R`

A dynamic variant of [`rank_eq`](#rank_eq3), using the inferred comparison operator.

## fn `rank_sorted_avg<T, U>(x: Sequence<T>, item: U, cmp_: (T, U)->(int)) -> float`  {#rank_sorted_avg3}

Returns the average rank of `item` in `x` according to `cmp_`, like [`rank_avg`](#rank_avg3), but assumes `x` is sorted according to `cmp_`, and is more efficient.

## dyn fn `rank_sorted_avg<T>(x: Sequence<T>, item: T)->R`
Where `cmp(T, T)->int`, `rank_sorted_avg(Sequence<T>, T, (T, T)->int)->R`

A dynamic variant of [`rank_sorted_avg`](#rank_sorted_avg3), using the inferred comparison operator.

## fn `rank_sorted_eq<T, U>(x: Sequence<T>, item: U, cmp_: (T, U)->(int)) -> int`  {#rank_sorted_eq3}

Returns the rank of `item` in `x` according to `cmp_`, like [`rank_eq`](#rank_eq3), but assumes `x` is sorted according to `cmp_`, and is more efficient.

## dyn fn `rank_sorted_eq<T, U>(x: Sequence<T>, item: U) -> R`

A dynamic variant of [`rank_sorted_eq`](#rank_sorted_eq3), using the inferred comparison operator.

## fn `reduce<T, U>(x: Sequence<T>, initial: U, f: (U, T)->(U)) -> U`

Returns the result of applying `f` to each item in `x`, starting with `initial` as the initial value.

## fn `reduce<T>(x: Sequence<T>, f: (T, T)->(T)) -> T`

Returns the result of applying `f` to each item in `x`, starting with the first item as the initial value.

## fn `repeat<T>(x: Sequence<T>) -> Sequence<T>`

Returns a sequence of all items in `x`, repeated infinitely.

## fn `repeat<T>(x: Sequence<T>, n: int) -> Sequence<T>`

Returns a sequence of all items in `x`, repeated `n` times.

## fn `reverse<T>(x: Sequence<T>) -> Sequence<T>`

Returns a sequence of all items in `x`, in reverse order.

## fn `rpush<T>(x: Sequence<T>, item: T) -> Sequence<T>`

Returns a sequence containing all items in `x` and `item`, at the first index.

## fn `sample<T>(x: Sequence<T>, n: int) -> Sequence<T>`
Requires `RANDOM` permissions.

Returns a sequence of `n` random items from `x`, without repetition. If `n` is greater than the length of `x`, an error is returned.

## fn `sample<T>(x: Sequence<T>, n: int, counts: Sequence<int>) -> Sequence<T>`
Requires `RANDOM` permissions.

Returns a sequence of `n` random items from `x`, where `x[i]` is considered to exist `counts[i]` times to be selected. For example, `sample([1,2,3], 4, [3,8,1])` might result in `[1,1,1,2]`, or `[2,2,2,2]`, but not in `[3,3,2,2]`. If `x` and `counts` are not the same length, an error is returned.

## fn `set<T>(x: Sequence<T>, idx: int, item: T) -> Sequence<T>`

Returns a sequence containing all items in `x`, with `item` at index `idx`.

## fn `shuffle<T>(x: Sequence<T>) -> Sequence<T>`
Requires `RANDOM` permissions.

Returns a sequence containing all items in `x`, in random order.

## fn `skip<T>(x: Sequence<T>, n: int) -> Sequence<T>`

Returns a sequence containing all items in `x`, except the first `n` items.

## fn `skip_until<T>(x: Sequence<T>, pred: (T)->(bool)) -> Sequence<T>`

Returns a sequence containing all items in `x`, except the first items until `pred` returns `true`.

## fn `sort<T>(x: Sequence<T>, cmp: (T,T)->(int)) -> Sequence<T>`  {#sort2}

Returns a sequence containing all items in `x`, sorted according to `cmp`.

## dyn fn `sort<T>(x: Sequence<T>)->R`
Where `cmp(T, T)->int`, `sort(Sequence<T>, (T, T)->(int))->r`

A dynamic variant of [`sort`](#sort2), using the inferred comparison operator.

## fn `sort_reverse<T>(x: Sequence<T>, cmp: (T,T)->(int)) -> Sequence<T>`  {#sort_reverse2}

Returns a sequence containing all items in `x`, sorted in reverse to `cmp`.

## dyn fn `sort_reverse<T>(x: Sequence<T>)->R`
Where `cmp(T, T)->int`, `sort(Sequence<T>, (T, T)->(int))->r`

A dynamic variant of [`sort`](#sort_reverse2), using the inferred comparison operator.

## dyn fn `sum<T, U>(x: Sequence<T>, initial: U)->R`
Where `add(U, T)->U`, `reduce(Sequence<T>, U, (U, T)->U)->R`

Returns the sum of all items in `x`, starting with `initial` as the initial value.

## fn `swap<T>(x: Sequence<T>, idx1: int, idx2: int) -> Sequence<T>`

Returns a sequence containing all items in `x`, with the items at `idx1` and `idx2` swapped.

## fn `take<T>(x: Sequence<T>, n: int) -> Sequence<T>`

Returns a sequence containing the first `n` items in `x`.

## fn `take_while<T>(x: Sequence<T>, pred: (T)->(bool)) -> Sequence<T>`

Returns a sequence containing the first items in `x` until `pred` returns `false`.

## fn `to_array<T>(x: Sequence<T>) -> Sequence<T>`

Returns a sequence containing all items in `x`, as an array.

## fn `to_generator<T>(x: Sequence<T>) -> Generator<T>`

Returns a generator that yields all items in `x`.

## fn `to_stack<T>(x: Sequence<T>) -> Stack<T>`

Returns a stack containing all items in `x`, where `x[0]` will be at the bottom of the stack.

## dyn fn `to_str<T>(x: Sequence<T>)->str`
Where `to_str(T)->str`

Returns a string representation of `x`. The string representation of a sequence is the string representation of each item, separated by commas and surrounded by square brackets.

## dyn fn `unzip<T0, T1, ... Tk>(x: Sequence<(T0, T1, ... Tk)>) -> (Sequence<T0>, Sequence<T1>, ... Sequence<Tk>)`

Returns a tuple of sequences, where the first sequence contains the first item of each tuple in `x`, the second sequence contains the second item of each tuple in `x`, and so on.

## dyn fn `zip<T0, T1, ... Tk>(x0: Sequence<T0>, x1: Sequence<T1>, ... xk: Sequence<Tk>) -> Sequence<(T0, T1, ... Tk)>`

Returns a sequence of tuples, where the first tuple contains the first item of each sequence, the second tuple contains the second item of each sequence, and so on. The length of the returned sequence is the length of the shortest input sequence.