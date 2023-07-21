# Lambda And Generic Functions
## Lambda Functions
Lambda functions are anonymous functions that can be used as values. They are created with the `->` symbol between a list of arguments and a function body.
```xray
let sq = (a: int) -> {a * a};

let x = sq(2);  

assert(x == 4)
```

They are most useful in functional programming.

```xray
fn get_first_eric(people: Sequence<str>)->Optional<str> {
    people.first(
        (person: str) -> {person.starts_with("Eric")}
    )
}
```
## Generic Functions
Just like types, functions can be generic. Generic functions are functions that can be parameterized with other types. Generic functions are created with the `<>` symbol between a list of generic parameters and a function body.

```xray
fn apply_twice<T>(f: (T) -> (T), x: T) -> T {
    f(f(x))
}

let x = apply_twice((a: int) -> {a * a}, 2);

assert(x == 16)
```