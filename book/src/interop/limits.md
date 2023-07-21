# Runtime Limits

When running compiled XRay programs, the runner can specify runtime limits. These are a set of limits that the program cannot exceed. Almost all of these limits are disabled by default, and can be enabled by the runner.

```admonish warning
It is highly recommended for production runners to enable all runtime limits to some sensible value.
```

## Runtime Violation  {#violations}

Exceeding any of the runtime limits will result in a runtime violation. This is a special error that is not catchable by the program. Instead, it will be caught by the runner, and returned to the interop layer as an error.

## Limits
Currently supported runtime limits:

### Size Limit
This is the maximum total amount of allocated XRay objects (in memory). This includes all objects, including functions and references. When setting this limit, keep in mind that this limit also includes items allocated by the XRay standard library, which currently amounts to approximately 40,000 bytes of overhead.

### Depth Limit
This is the maximum depth of the call stack. This includes all functions, including those in the XRay standard library. Note that this limit does not apply to tail-call recursions.

### Recursion Limit
This is the maximum number of times a function can call itself recursively with tail-call optimization.

### UD Call Limit
This is the maximum number of times a user function can be called. This includes all functions, including those in the XRay standard library.

The counter towards this limit is global, and not per-function. It can be reset by interop.

### Max Search Limit
This is a limit over the maximum number of iterations inside of native functions that perform searches. This includes all functions, including those in the XRay standard library.

### Time Limit
This is the maximum time a program can run for. This includes all functions, including those in the XRay standard library. Note that the "timer" begins counting when the limit object itself is created, and not when the program starts running. The timer can be reset by interop. The timeout is checked at the beginning of each user function call.

### Permissions
Some operations should not always be allowed. These special operations require "permissions" that can be enabled or disabled in the limits, permissions are expanded on in the [next section](./permissions.md).