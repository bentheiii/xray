# String Literals
String literals are enclosed in either double quotes (`"`) or single quotes (`'`).
```xray
let x = "Hello, world!";
let y = 'Hello, world!';
// string literals can include quotes of the other kind without escaping
let z = "Hello, 'world'!";
let w = 'Hello, "world"!'
```

String literal can begin with any number of `#` characters, which are ignored. The number of `#` characters at the beginning of the string literal is the number of `#` characters that must be present at the end of the string literal.
```xray
let x = #"Hello, world!"#;
let y = ##"Hello, world!"##;
let z = ###"Hello, "world!""###;
```
## Escape Sequences
String literals can contain escape sequences, which are sequences of characters that are interpreted as a single character. Escape sequences are of the form:
* `\n` - newline
* `\r` - carriage return
* `\t` - tab
* `\\` - backslash
* `\"` - double quote
* `\'` - single quote
* `\0` - null character
* `\u{xxxxxx}` - hexadecimal character code (between 1 and 6 hexadecimal digits)

```xray
let table = "a\tb\nc\t\\d";  // a    b
                             // c    \d
```
## Formatted Strings
Formatted strings begin with `f` and can contain expressions enclosed in curly braces (`{}`), curly braces can be escaped by doubling them (`{{}}`).
```xray
let x = 1;
let y = 2;
let z = f"{x} + {y} = {x + y}";  // "1 + 2 = 3"
```
Formatted string creates an array of all the string parts with the expressions after calling `to_str` on each expression, and then calls `join` on the array. This means that the expressions can be any type that implements `to_str`.
Therefore, the above expression `f"{x} + {y} = {x + y}"` is equivalent to `[to_str(x), " + ", to_str(y), " = ", to_str(x + y)].join()`.

expressions in formatted strings can be followed by a colon (`:`) and a format specifier. This will call the `format` function on the expression with the format specifier as the argument, instead of `to_str`. So the expression `f"x is {x:04}"` is equivalent to `["x is ", format(x, "04")].join()`.
## Raw Strings
Raw strings begin with `r` and are string literals that do not interpret escape sequences.
```xray
let x = r"\n";  // "\\n"
```