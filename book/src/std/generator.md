# Generator

## type `Generator<T>`

An iteration over a data set.

## fn `add<T>(a: Generator<T>, b: Generator<T>)->Generator<T>`

Chains `a` and `b` together. Does not allocate.

## fn `aggregate<T, U>(x: Generator<T>, initial: U, func: (U, T)->U) -> Generator<U>`

Creates a new generator that applies `func` to the previously evaluated value, and the next element of `x`, for each element, starting with the given initial value `initial`. The returned generator will always begin with `initial`, even if the original generator is empty.

```xray
let orig = [1,2,3,4,5].to_generator();

let agg = orig.aggregate(10, bit_xor{int, int});

assert(agg.to_array() == [10, 11, 9, 10, 16, 11])
```

## fn `aggregate<T>(x: Generator<T>, func: (T, T)->T) -> Generator<T>`

Creates a new generator that applies `func` to the previously evaluated value, and the next element of `x`, for each element.

## fn `all<T>(x: Generator<T>, pred: (T)->bool)->bool`

Returns true if the given predicate `pred` returns true for all elements of `x`.

## fn `any<T>(x: Generator<T>, pred: (T)->bool)->bool`

Returns true if the given predicate `pred` returns true for any element of `x`.

## fn `contains<T, U>(x: Generator<T>, needle: U, eq_: (T, U)->bool) -> bool`  {#contains3}

Returns true if `needle` is contained in `x`, according to the given equality function `eq_`.

## dyn fn `contains<T, U>(x: Generator<T>, needle: U)->...`
Where `eq(T,U)->bool`


A dynamic variant of [`contains`](#contains3) that uses the default equality function.

## fn `count<T>(x: Generator<T>, pred: (T)->(bool))->int`

Returns the number of elements in `x` for which the given predicate `pred` returns true.

## fn `count<T, U>(x: Generator<T>, needle: U, eq_: (T, U)->bool) -> bool`  {#count3}

Returns the number of elements in `x` that are equal to `needle`, according to the given equality function `eq`.

## dyn fn `count<T, U>(x: Generator<T>, neddle: U)->...`
Where `eq(T,U)->bool`

A dynamic variant of [`count`](#count3) that uses the default equality function.

## fn `distinct<T>(x: Generator<T>, hash_ (T)->(int), eq_: (T, T)->(bool)) -> Generator<T>`  {#distinct3}

Returns a generator that only includes at most one occurrence of each element of `x`. The given hash and equality functions `hash_` and `eq_` are used to determine if two elements are equal.

## dyn fn `distinct<T>(x: Generator<T>) -> ...`
Where `hash(T)->int`, `eq(T,T)->bool`

A dynamic variant of [`distinct`](#distinct3) that uses the default hash and equality functions.

## fn `enumerate<T>(x: Generator<T>, start: int?, stride: int?) -> Generator<(int, T)>`

Returns a generator that yields the index and value of each element of `x`. The first element of the tuple is the index, and the second is the value. Optionally, the starting index `start` and step size `stride` can be specified.

## fn `filter<T>(x: Generator<T>, pred: (T)->bool) -> Generator<T>`

Returns a generator that yields only the elements of `x` for which `pred` returns `true`.

## fn `first<T>(x: Generator<T>, pred: (T)->(bool)) -> Optional<T>`

Returns the first element of `x` for which `pred` returns `true`. If no such element exists, returns `none`.

## fn `flatten<T>(x: Generator<Generator<T>>)->Generator<T>`

Returns a generator that yields the elements of each generator in `x`, in order.

```xray
let g = [
    [1,2,3].to_generator();,
    [4,5,6].to_generator();,
    [7,8,9].to_generator();
].to_generator();

assert(g.flatten().to_array() == [1,2,3,4,5,6,7,8,9])
```

## fn `flatten<T>(x: Sequence<Generator<T>>) -> Generator<T>`

Returns a generator that yields the elements of each generator in `x`, in order.

## dyn fn `geo_mean<T>(x: Generator<T>)->P`
Where `mul(T, T)->M`, `aggregate(Generator<T>, (T, T)->(M))->A`, `Enumerate(Generator<A>, int)->E`, `last(E)->(int, T)`, `pow(T, float)->P`

Returns the geometric mean of the elements of `x`.

## fn `get<T>(x: Generator<T>, idx: int)->T`

Returns the element of `x` at the index `idx`.

## fn `group<T>(x: Generator<T>, eq_: (T, T)->bool) -> Generator<Sequence<T>>`  {#group2}

Returns a generator that yields sequences of elements of `x` that are equal to each other, according to the given equality function `eq_`.

```xray
let g = [3,6,9,2,1,3,1,7].to_generator();
// group by modulo 3
let grouped = g.group((i:int, j:int)->{i%3 == j%3});
assert(grouped.to_array() == [[3,6,9],[2],[1],[3],[1,7]])
```

## dyn fn `group<T>(x: Generator<T>)->...`
Where `eq(T,T)->bool`


A dynamic variant of [`group`](#group2) that uses the default equality function.

## dyn fn `harmonic_mean<T>(x: Generator<T>)->R`
Where `div(int, T)->D`, `map(Generator<T>, (T)->(D))->M`, `mean(M)->A`, `div(int, A)->R`

Returns the harmonic mean of the elements of `x`. The harmonic mean of a sequence \\(x_1, x_2, \ldots, x_n\\) is defined as:

\\[
\frac{n}{\frac{1}{x_1} + \frac{1}{x_2} + \ldots + \frac{1}{x_n}}
\\]

## fn `last<T>(x: Generator<T>)->T`

Returns the last element of `x`. If `x` is empty, returns an error.

## fn `len<T>(x: Generator<T>)->int`

Returns the number of elements in `x`.

## fn `map<T, U>(x: Generator<T>, func: (T)->(U)) -> Generator<U>`

Returns a generator that yields the result of applying the given function `func` to each element of `x`.

## fn `max<T>(x: Generator<T>, lt_: (T, T)->bool)->T`  {#max2}

Returns the maximum element of `x`, according to the given "less-than" function `lt_`. If `x` is empty, returns an error.

## dyn fn `max<T>(x: Generator<T>)->R`
Where `lt(T,T)->bool`, `max(Generator<T>, (T, T)->bool)->R`

A dynamic variant of [`max`](#max2) that uses the default "less-than" function.

## dyn fn `mean<T>(x: Generator<T>)->R`
Where `add(T, T)->A`, `aggregate(Generator<T>, (T, T)->(A))->M`, `enumerate(M, int)->E`, `last(E)->(int, T)`, `div(T, int)->R`

Returns the mean of the elements of `x`.

## fn `min<T>(x: Generator<T>, lt_: (T, T)->bool)->T`  {#min2}

Returns the minimum element of `x`, according to the given "less-than" function `lt_`. If `x` is empty, returns an error.

## dyn fn `min<T>(Generator<T>)->R`
Where `lt(T,T)->bool`, `min(Generator<T>, (T, T)->bool)->R`

A dynamic variant of [`min`](#min2) that uses the default "less-than" function.

## fn `nth<T>(x: Generator<T>, idx: int, pred: (T)->(bool)) -> Optional<T>`

Returns the `idx`th element of `x` for which the given predicate `pred` returns `true`. If no such element exists, returns `none`.

## dyn fn `product<T0, T1, ..., Tn>(a: Generator<T0>, b: Generator<T1>, ..., z: Generator<Tn>) -> Generator<(T0, T1, ..., Tn)>`

Returns a generator that yields the cartesian product of the given generators.

```xray
let g = [3,6,9].to_generator();
let g2 = "ABCA".chars().to_generator();

assert(g.product(g2).to_array() == [(3,'A'),(3,'B'),(3,'C'),(3,'A'),(6,'A'),(6,'B'),(6,'C'),(6,'A'),(9,'A'),(9,'B'),(9,'C'),(9,'A')])
```

## dyn fn `product<T, U>(x: Generator<T>, U)->R`
Where `mul(U, T)->U`, `reduce(Generator<T>, U, (U, T)->U)->R`

Returns the product of the elements of `x`, given an initial value.

## fn `reduce<T, U>(x: Generator<T>, initial: U, func: (U, T)->U) -> U`

Returns the result of applying the given function `func` to each element of `x`, starting with the given `initial` value.

## fn `reduce<T>(x: Generator<T>, func: (T, T)->T) -> T`

Returns the result of applying the given function `func` to each element of `x`.

## fn `repeat<T>(x: Generator<T>)->Generator<T>`

Returns a generator that yields the elements of `x` indefinitely.

## fn `repeat<T>(x: Generator<T>, n: int)->Generator<T>`

Returns a generator that yields the elements of `x`, `n` times.

## fn `skip<T>(x: Generator<T>, skip_count: int)->Generator<T>`

Returns a generator that skips the first `skip_count` of elements of `x`.

## fn `skip_until<T>(x: Generator<T>, pred: (T)->(bool))->Generator<T>`

Returns a generator that skips elements of `x` until the given predicate `pred` returns `true`. The first element for which `pred` returns `true` will be the first element of the returned generator.

```xray
let a = [1,2,4,5,3,1,7,8].to_generator();
let b = a.skip_until((x: int) -> {x >= 5});

assert(b.to_array() == [5,3,1,7,8])
```

## fn `successors<T>(initial: T, succ: (T)->(T)) -> Generator<T>`

Returns an infinite generator that yields the result of applying `succ` to the given `initial` value, and then applying `succ` to the result, and so on. The first element of the generator will be `initial`.

```xray
let g = successors(1, (x: int) -> {x*2});
assert(g.take(5).to_array() == [1,2,4,8,16])
```

## fn `successors_until<T>(initial: T, succ: (T)->(Optional<T>)) -> Generator<T>`

Returns an infinite generator that yields the result of applying `succ` to the given `initial` value, and then applying `succ` to the result, and so on, until `succ` returns `none`. The first element of the generator will be `initial`.

## dyn fn `sum<T, U>(x: Generator<T>, U)->R`
Where `add(U, T)->U`, `reduce(Generator<T>, U, (U, T)->U)->R`

Returns the sum of the elements of the generator, given an initial value.

## fn `take<T>(x: Generator<T>, len_: int) -> Generator<T>`

Returns a generator that yields the first `len_` elements of `x`.

## fn `take_while<T>(x: Generator<T>, pred: (T)->(bool)) -> Generator<T>`

Returns a generator that yields elements of `x` until the given predicate `pred` returns false. The first element for which `pred` returns false will not be included in the returned generator.

```xray
let a = [1,2,4,5,3,1,7,8].to_generator();
let b = a.take_while((x: int) -> {x < 5});

assert(b.to_array() == [1,2,4])
```

## fn `to_array<T>(x: Generator<T>) -> Sequence<T>`

Returns a sequence containing the elements of `x`.

## dyn fn `unzip<T0, T1, ..., Tn>(x: Generator<(T0, T1, ..., Tn)>) -> (Generator<T0>, Generator<T1>, ..., Generator<Tn>)`

Returns a tuple of generators, each of which yields the elements of the corresponding tuple element of `x`.

## fn `windows<T>(x: Generator<T>, width: int) -> Generator<Sequence<T>>`

Returns a generator that yields sliding slices (or windows) of size `width` over the elements of `x`.

```xray
let a = [1,2,3,4,5].to_generator();
let b = a.windows(3);

assert(
    b.to_array() == [
        [1,2,3],
        [2,3,4],
        [3,4,5]
    ]
)
```

## fn `with_count<T>(x: Generator<T>, hash_: (T)->(int), eq_: (T, T)->(bool)) -> Generator<(T, int)>`  {#with_count3}

Returns a generator that yields the elements of `x`, along with a count of the number of times that element has been previously yielded. The given `hash_` and `eq_` functions are used to determine if two elements are equal.

## dyn fn `with_count<T>(x: Generator<T>) -> R`
Where `hash(T)->int`, `eq(T, T)->bool`, `with_count<T>(Generator<T>, (T)->int, (T, T)->bool)->R`

A dynamic variant of [`with_count`](#with_count3) that uses the default hash and equality functions.

## dyn fn `zip<T0, T1, ..., Tn>(a: Generator<T0>, b: Generator<T1>, ..., z: Generator<Tn>) -> Generator<(T0, T1, ..., Tn)>`

Returns a generator that yields tuples of the elements of the given generators. The length of the returned generator will be the length of the shortest generator.