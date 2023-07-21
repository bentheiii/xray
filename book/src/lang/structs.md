# Structs
Structs are a way for users to create their own types. They are similar to classes in other languages, in that they group associated data together.

## Defining Structs
Structs are defined with the `struct` keyword, followed by the name of the struct, and the list of fields between braces. Each field is defined with its name, followed by a colon and the type of the field.

```xray
// This defines a struct named Point, with two fields, x and y, both of type int.
struct Point (
    x: int,
    y: int,
)
```

## Creating Structs
Structs can be created by calling the struct name as if it were a function, and passing the values for each field as arguments.

```xray
struct Point (
 x: int,
 y: int,
)

let p = Point(1, 2);
```
## Accessing Struct Fields
Struct fields can be accessed with the access operator (`::`), followed by the name of the field.

```xray
struct Point (
 x: int,
 y: int,
)

let p = Point(1, 2);

let x = p::x;
assert(x==1)
```
## Generic Structs
Structs can be generic, and can have generic fields. This can be done with angle brackets after the struct's name.

```xray
struct Point<T> (
 x: T,
 y: T,
 z: Optional<T>,
)

let p = Point(1, 2, none());  // inferred to be Point<int>
```
## Recursive Structs
Structs can be recursive, meaning that they can contain fields of their own type.

```xray
struct Node (
 value: int,
 next: Optional<Node>,
)

let n = Node(1, some(Node(2, none())));
```