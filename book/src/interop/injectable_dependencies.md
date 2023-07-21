# Injectable Dependencies

The runtime of the evaluation scope is generic, and can be injected with providers that change the way side-effect behave. Current supported providers are:

## Writers
Functions like `display` and `debug` can print to the "standard output". However, it is the runner's choice as to what the standard output is. The `Writer` trait allows the runner to inject a writer that can be used by the evaluation scope to print to.

## Random Number Generators
Any random number generator can be used by the evaluation scope. This allows the runner to inject a random number generator that can be used by the evaluation scope to generate random numbers.

## Time
The evaluation scope can use the current time and date. The runner can inject a time provider that can be used by the evaluation scope to get the current time and date.