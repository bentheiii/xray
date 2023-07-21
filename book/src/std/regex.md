# Regex

## type `Regex`

A regular expression, built from a pattern.

## struct `Match(...)`

A match of a regular expression over a specific string.

## fn `get(m: Match, i: int) -> str`

Get a capture group from a match by its index.

## fn `get(m: Match, i: str) -> str`

Get a capture group from a match by its name.

## fn `match(r: Regex, s: str, i: int?) -> Optional<Match>`

Match a regular expression `r` against a string `s` at index `i` (defaults to 0). If the match fails, returns `None`.

## fn `regex(s: str) -> Regex`
Requires `REGEX` permission.

Returns a regular expression built from a string `s`. If the string is not a valid regular expression, returns an error.

## fn `search(r: Regex, s: str, i: int?, j: Optional<int>?) -> Optional<Match>`

Search a regular expression `r` against a string `s`, starting at index `i` (defaults to 0) up to index `j` (defaults to the end of the string). If the match fails, returns `None`. Note that `j` is the exclusive upper limit of where a match can start.

## fn `search(r: Regex, s: str, i: int, j: int) -> Optional<Match>`

Search a regular expression `r` against a string `s`, starting at index `i` up to index `j`. If the match fails, returns `None`. Note that `j` is the exclusive upper limit of where a match can start.