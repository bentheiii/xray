# Stack

## type `Stack<T>`

A first-in, last-out stack of elements of type `T`. The stack is implemented as a singly-linked list, so accessing the last added element (the "head") is fast, but accessing the rest of the stack is slow.

## dyn fn `eq<T0, T1>(x: Stack<T0>, y: Stack<T1>) -> bool`
Where `eq(T0, T1)->bool`

Returns whether the stacks are equal, element-wise.

## dyn fn `hash<T>(x: Stack<T>) -> int`
Where `hash(T)->int`

Returns a hash of the stack.

## fn `head<T>(x: Stack<T>) -> T`

Returns the last added element of the stack.

## fn `len<T>(x: Stack<T>) -> int`

Returns the number of elements in the stack.

## fn `push<T>(x: Stack<T>, value: T) -> Stack<T>`

Returns a new stack with the given value added.

## fn `stack() -> Stack<?>`

Returns an empty stack.

## fn `tail<T>(x: Stack<T>) -> Stack<T>`

Returns a new stack with the head of the stack removed. If `x` is empty, returns an error.

## fn `to_array<T>(x: Stack<T>) -> Sequence<T>`

Returns an array containing the elements of the stack, in reverse order of insertion. So index `0` of the returned array will be the last element added to the stack, and index `-1` will be the first element added to the stack.

## fn `to_array_reversed<T>(x: Stack<T>) -> Sequence<T>`

Returns an array containing the elements of the stack, in order of insertion. So index `0` of the returned array will be the first element added to the stack, and index `-1` will be the last element added to the stack.