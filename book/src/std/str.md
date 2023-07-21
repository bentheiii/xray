# str

## type `str`

A utf-8 encoded string.

## fn `add(a: str, b: str)->str`

Concatenates `a` and `b`.

## fn `chars(x: str)->Sequence<str>`

Returns a sequence of the characters in `x`.

## fn `cmp(a: str, b: str)->int`

Returns a lexicographical comparison of `a` and `b`. Each character is compared by its unicode code point.

## fn `code_point(x: str)->int`

Returns the unicode code point of the character `x`. If `x` does not contain exactly one character, an error is returned.

## fn `contains(haystack: str, needle: str, start_index: int?)->bool`

Returns whether `haystack` contains `needle`. If `start_index` is specified, the search will begin at that index.

## fn `ends_with(x: str, suffix: str)->bool`

Returns whether `x` ends with suffix.

## fn `eq(a: str, b: str)->bool`

Returns whether `a` and `b` are equal. Each character is compared by its unicode code point.

## fn `find(haystack: str, needle: str, start_index: int?)->Optional<int>`

Returns the index of the first occurrence of `needle` in `haystack`. If `start_index` is specified, the search will begin at that index. If `needle` is not found, `none` is returned.

## fn `format(x: str, f: str)->str`

Returns a formatted string representation of `x`, formatted according to a format string `f`. See [formatting](../lang/std_conventions.md#formatting) for details on formatting string format. No modes are accepted.

## fn `format_replace(template: str, repl: (x: str)->(x: str))->str`

An advanced function for formatting objects. Replaces all occurrences of `%*` (`%` and any character) in `template` with the result of the function `repl`. The function is called with the character after the `%` as its argument.

```xray
fn repl(s: str)->str{
    if(s == "n", "bob",
    if(s == "t", "joe",
    "%"))
}

let template = "hello %n, my name is %t";

let result = format_replace(template, repl);

assert(result == "hello bob, my name is joe")
```

## fn `get(x: str, idx: int)->str`

Returns the character at index `idx` of `x`. If the index is out of bounds, an error is returned.

## fn `hash(x: str)->int`

Returns a hash of `x`.

## fn `is_lower(x: str)->bool`

Returns whether all characters in `x` are lower case, as based on the Unicode property `Lowercase`.

## fn `is_upper(x: str)->bool`

Returns whether all characters in `x` are lower case, as based on the Unicode property `Uppercase`.

## fn `is_whitespace(x: str)->bool`

Returns whether all characters in `x` are whitespace, as based on the Unicode property `White_Space`.

## fn `join(g: Generator<str>, sep: str?)->str`

Concatenates all strings in a generator `g`, optionally separated by separator `sep`.

## fn `join(s: Sequence<str>, sep: str?)->str`

Concatenates all strings in a sequence `s`, optionally separated by separator `sep`.

## fn `len(x: str)->int`

Returns the number of characters in `x`.

## fn `lower(x: str)->str`

Returns a copy of `x` with all characters lowercased.

## fn `lstrip(x: str, pred: (x: str)->(bool))->str`  {#lstrip2}

Returns a copy of `x` with all leading characters removed for which `pred` returns true.

```xray
let s = "banana";
let lstripped = lstrip(s, (c: str)->{c == "a" || c == "b"});
assert(lstripped == "nana")
```

## fn `lstrip(x: str)->str`

A variant of [`lstrip`]{#lstrip2} that removes all whitespace.

## fn `mul(x: str, i: int)->str`

Returns a string that is the concatenation of `x` with itself `i` of times.

## fn `partition(x: str, sep: str)->(str, str)`

Returns a tuple of `x` before the first occurrence of `sep`, and the string after `sep`. If `sep` is not found, `x` and an empty string is returned.

```xray
let s = "i am a teapot short and stout";

let partitioned_found = partition(s, "teapot");
let partitioned_not_found = partition(s, "toaster");

assert(partitioned_found == ("i am a ", " short and stout"))
&& assert(partitioned_not_found == ("i am a teapot short and stout", ""))
```

## fn `remove_prefix(x: str, prefix: str)->str`

If `x` starts with `prefix`, returns a copy of `x` with the `prefix`. Otherwise, returns a copy of `x`.

## fn `remove_suffix(x: str, suffix: str)->str`

If `x` ends with `suffix`, returns a copy of `x` with the `suffix`. Otherwise, returns a copy of `x`.

## fn `replace(x: str, needle: str, repl: str)->str`

Returns a copy of `x` with all occurrences of `needle` replaced with `repl`.

## fn `replace(x: str, needle: str, repl: str, n: int)->str`

Returns a copy of `x` with the first `n` occurrences of `needle` replaced with `repl`.

## fn `reverse(x: str)->str`

Returns a copy of `x` with the characters in reverse order.

## fn `rfind(haystack: str, needle: str, start_index: int?)->Optional<int>`

Returns the index of the last occurrence of `needle` in `haystack`. If `start_index` is specified, the search will begin at that index. If `needle` is not found, `none` is returned.

## fn `rpartition(x: str, sep: str)->(str, str)`

Returns a tuple of `x` before the last occurrence of `sep`, and the string after `sep`. If `sep` is not found, an empty string and `x` is returned.

```xray
let s = "i am a teapot short and stout";

let partitioned_found = rpartition(s, "a");
let partitioned_not_found = rpartition(s, "toaster");

assert(partitioned_found == ("i am a teapot short ", "nd stout"))
&& assert(partitioned_not_found == ("", "i am a teapot short and stout"))
```

## fn `rsplit(x: str, sep: str, n: int)->Sequence<str>`

Returns a sequence of strings, split by `sep`, starting from the end of the string. The sequence will contain at most `n` strings.

## fn `rstrip(x: str, pred: (x: str)->(bool))->str` {#rstrip2}

Returns a copy of the string with all trailing characters removed for which the predicate returns true.

## fn `rstrip(x: str)->str`

A variant of [`rstrip`]{#rstrip2} that removes all whitespace.

## fn `split(x: str, sep: str)->Generator<str>`

Returns a generator of strings, split by `sep`.

## fn `split(x: str, sep: str, n: int)->Generator<str>`

Returns a generator of strings, split by `sep`. The generator will contain at most `n` strings.

## fn `starts_with(x: str, prefix: str)->bool`

Returns whether `x` starts with `prefix`.

## fn `strip(x: str, pred: (x: str)->(bool))->str`  {#strip2}

Returns a copy of `x` with all leading and trailing characters removed for which the predicate `pred` returns true.

## fn `strip(x: str)->str`

A variant of [`strip`]{#strip2} that removes all whitespace.

## fn `substring(x: str, start_idx: int, end_idx: int)->str`

Returns a copy of `x` from `start_idx` (inclusive) to `end_idx` (exclusive).

## fn `substring(x: str, start_idx: int, end_idx: Optional<int>?)->str`

Returns a copy of `x` from `start_idx` (inclusive) to `end_idx` (exclusive). If `end_idx` is not specified, the string will be sliced to the end.

## fn `to_int(x: str)->int`

Returns the integer value of `x`. If `x` is not a valid integer, an error is returned.

## fn `to_str(x: str)->str`

Returns `x` unaltered.

## fn `upper(x: str)->str`

Returns a copy of `x` with all characters uppercased.