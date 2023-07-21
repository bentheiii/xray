# Unions
Unions are another for users to define their own types. They are similar to unions in other languages, in that they can have multiple variants, each with their own data. Each variant must have exactly one value.

## Defining Unions
Unions are defined with the `union` keyword, followed by the name of the union, and the list of variants between braces. Each variant is defined with its name, followed by a colon and the type of the variant.

```xray
/* This defines a union named Label, with two variants, either "named" which contains
   a string, or "id", which contains an int. */
union Label (
    named: str,
    id: int,
)
```

## Creating Unions
Unions can be created by calling the union name with the access operator followed by the variant name as if it were a function, and passing the values for each variant as arguments.

```xray
union Label (
    named: str,
    id: int,
)

let l0 = Label::named("hello");
let l1 = Label::id(42);
```

## Accessing Union Variants

Union variants can be accessed with the conditional access operator (`?:`), followed by the name of the variant. This will return an `Optional` value, which will be `none()` if the variant is not the one being accessed, or `some(<Variant data>)` if it is.

```xray
union Label (
    named: str,
    id: int,
)

let l0 = Label::named("hello");

assert(l0?:named == some("hello"))
&& assert(l0?:id == none())
```

Additionally, variant data can be accessed with the asserting access operator (`!:`), followed by the name of the variant. This will return the data of the variant, or throw an error if the variant is not the one being accessed.

```xray
union Label (
    named: str,
    id: int,
)

let l0 = Label::named("hello");

assert(l0!:named == "hello")
```

### Case-By-Case Handling of Unions

The conditional access operator can be chained to handle each case of a union.

```xray
union Label (
    named: str,
    id: int,
)

fn label_is_known(label: Label) -> bool {
    fn name_is_known(name: str) -> bool {
        // we only know the label names "main" and "hello"
        name == "hello" || name == "main"
    }

    fn id_is_known(id: int) -> bool {
        // we only know the label ids 0 and 1
        id == 0 || id == 1
    }

    label?:named.map(name_is_known)
    || label!:id.map(id_is_known)
}
```
## Generic Unions
Unions can be generic, and can have generic variants. This can be done with angle brackets after the union's name.

```xray
union Label<T> (
    named: str,
    id: T,
)

let l0 = Label::named("hello");  // inferred to be Label<Unknown>
let l1 = Label::id(42);          // inferred to be Label<int>
```

## Recursive Unions

Unions can be recursive, meaning that they can contain variants of their own type.

```xray
union Node (
    last: (),
    next: Node,
)

let n0 = Node::last();
let n1 = Node::next(n0);
```