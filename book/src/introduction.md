# Introduction

XRay is a functional, strongly-typed, statically-typed, stateless programming language. Its strong suit is that it presents the runner of the code a lot of power with regard to what the program is capable of doing, thus making it ideal for services that need their users to describe some complex behavior, while not necceraly trusting them.

## Philosophy
### Library First
XRay is supposed to be embedded within larger programs, so it is developed as a library first, with its executable nature being mostly for prototyping and testing. XRay's nature as an embedded-first language makes it ideal for customizations on the runner's side. [Read more about XRay's interoperability](interoperability_intro.md).
### Runtime Limitation
The runner of the code can impose many different kinds of limits and permissions on the users, and XRay continuously monitors the running program to ensure that the limitations are kept. Limits range from maximum memory the program may allocate to maximum stack depth. 
``` admonish warning
XRay is very early in development, therefore while all limits are expected to be enforced at all time, holes and breaches may well exist.
```

[Read more about XRay's runtime limitation](runtime_limitations.md).

### Deterministic Compilation
In contrast with runtime execution, XRay's compilation process strives to b deterministic, resulting in the same runnable program, regardless of the runner's or user's configuration. Thus, compilation should be safe and uniform, without complex configurations or non-linear complexity.
### Easy to Learn
```admonish quote title="Guiding Principle"
If it's a no-brainer in excel, it should be a no-brainer in XRay.
```
XRay is expected to be used by users who don't necessarily work with code every day. Thus, it should be easy to pick up, even if the internals would be unknown.

### Static Checking
User errors should be caught early and reported simply to the user. For this reason, XRay is strongly and statically typed, and compiles deterministically.
#### Runtime Errors
```admonish quote title=""
Never underestimate a fool with a deadline
```

Errors are still possible during runtime, XRay believes that these errors should interoperable and handle-able by users, but they should not be ignored accidentally. [Read more about XRay's runtime error handling](runtime_errors.md).