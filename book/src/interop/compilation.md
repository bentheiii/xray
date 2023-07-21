# Compliation

XRay code is single-pass compiled in-memory into a "compilation scope".

```admonish note title="Compile to File"
Since XRay is single-pass, compilation is very fast. Also, some of XRay code safety is only enforced at compile-time. For these reasons, serializing compilation scopes is not supported, and not recommended. It is better to handle raw XRay code, and compile it as needed on the machine that intends to run it.
```

Compiling code into an existing compilation scope is done with the `feed_file` method. This method can fail, but it will never run user code.