# Collections
Collections are data structures that store multiple values. All standard library collections cannot store errors, attempting to insert an error into a collection will return an error.

```xray
let arr = [];

let result = arr.push(1/0);

assert(is_error(result))
```

Note that while collections cannot store errors, lazy collections can produce errors when evaluated.

```xray
let orig = [0,1,2,3];
let mapped = orig.map((x: int)->{1/x}); // [1/0, 1, 1/2, 1/3]

assert(is_error(mapped[0]))
```