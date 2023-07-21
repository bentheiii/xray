# Matrix

## struct `Matrix<T>(...)`

A matrix is a two dimensional array of values of type `T`.

```admonish note
Like Sequences, matrices can be implemented lazily, which can force the evaluation of the matrix to be deferred until it is needed. This can be useful for large matrices, but might be undesirable for small matrices. To force a matric to be evaluated, use the [`manifest`](#manifest) function.
```

## fn `add<T0, T1, U>(x: Matrix<T0>, y: Matrix<T1>, add_: (T0, T1)->(U)) -> Matrix<U>`  {#add3}

Adds two matrices `x` and `y` element-wise, using `add_` to add the elements.

## dyn fn `add<T0, T1, U>(m1: Matrix<T0>, m2: Matrix<T1>) -> Matrix<U>`
Where `add(T0, T1) -> U`.

A dynamic variant of [`add`](#add3) that infers the add function.

## fn `columns<T>(m: Matrix<T>) -> int`

Returns the number of columns in the matrix `m`.

## fn `get<T>(m: Matrix<T>, row: int, col: int) -> T`

Gets the element of `m` at row `row` and column `col`.

## fn `get<T>(m: Matrix<T>, c: (int, int)) -> T`

Gets the element of `m` at row `c::item0` and column `c::item1`.

## fn `manifest<T>(m: Matrix<T>) -> Matrix<T>`  {#manifest}

Forces the evaluation of the matrix `m` to a non-lazy matrix and returns the result.

## fn `matrix<T>(rows: int, cols: int, data: Sequence<T>) -> Matrix<T>`

Creates a matrix with `rows` rows and `cols` columns, using the elements of `data` to fill the matrix, row-first. If `data` does not have exactly `rows * cols` elements, an error is returned.

## fn `matrix<T>(rows: int, cols: int, func:(int, int)->(T)) -> Matrix<T>`

Creates a matrix with `rows` rows and `cols` columns, using `func` to fill the matrix lazily.

## fn `matrix<T>(rows: int, cols: int, func:((int, int))->(T)) -> Matrix<T>`

Creates a matrix with `rows` rows and `cols` columns, using `func` to fill the matrix lazily.

## fn `matrix<T>(data: Sequence<Sequence<T>>) -> Matrix<T>`

Creates a matrix from a sequence of sequences `data`. If the sequences are not all the same length, or if `data` is empty, an error is returned.

## fn `rows<T>(m: Matrix<T>) -> int`

Returns the number of rows in the matrix `m`.

## fn `to_sequence<T>(m: Matrix<T>) -> Sequence<Sequence<T>>`

Converts the matrix `m` to a sequence of sequences.