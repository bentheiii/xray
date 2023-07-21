# Permissions

```admonish note
In nearly all cases, permissions are enabled by default.
```

Current built-in permissions are:

## NOW
This permission allows the program to fetch the current time and date. It is enabled by default.

## PRINT
This permission allows the program to print values to the standard output. It is enabled by default.

## PRINT_DEBUG
This permission allows the program to print the internal value representations to the standard output It is enabled by default.

## RANDOM
This permission allows the program to generate random numbers and values. It is enabled by default.

## REGEX
This permission allows the program to use regular expressions. It is disabled by default.

```admonish note title="Why are regular expressions disabled by default?"
The current implementation of regular expressions uses a hybrid model, where matches are found with a linear-complexity model, and then the matches are checked with a backtracking model. This is done to allow for more expressive regular expressions, but it might have some edge cases where the program can take a long time to run. This is why the REGEX permission is disabled by default.
```

## SLEEP
This permission allows the program to halt its own execution for a set amount of time. It is disabled by default.