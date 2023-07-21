# Discrete Distribution

## type `DiscreteDistribution`

A discrete distribution over the integers.

## fn `binomial_distribution(n: int, p: float) -> DiscreteDistribution`

Returns a binomial distribution with `n` trials and `p` probability of success.

## fn `cdf(dist: DiscreteDistribution, x: int) -> float`

Returns the cumulative distribution function of `dist` at `x`. The returned value will always be between `0` and `1`.

## fn `custom_distribution(probs: Sequence<(int, float)>) -> DiscreteDistribution`

Returns a custom discrete distribution. The distribution is defined by a list of pairs `(x, p)`, where `x` is an integer and `p` is the probability of `x`. If the sum of the probabilities is not `1`, the probabilities will be normalized.

## fn `geometric_distribution(p: float) -> DiscreteDistribution`

Returns a geometric distribution with probability `p`.

## fn `hypergeometric_distribution(N: int, k: int, n: int) -> DiscreteDistribution`

Returns a hypergeometric distribution with `N` total items, `k` of which are of one type, and `n` of which are selected.

## fn `mean(dist: DiscreteDistribution) -> float`

Returns the mean (expected value) of `dist`.

## fn `negative_binomial_distribution(r: float, p: float) -> DiscreteDistribution`

Returns a negative binomial distribution with `r` successes and `p` probability of success.

## fn `pmf(dist: DiscreteDistribution, x: int) -> float`

Returns the probability mass function of `dist` at `x`.

## fn `poisson_distribution(lambda: float) -> DiscreteDistribution`

Returns a Poisson distribution with rate `lambda`.

## fn `quantile(dist: DiscreteDistribution, p: float) -> int`

Returns the inverse-cdf of `dist` at `p`. That is, returns a value `x` such that `cdf(dist, x) ~ p`.

## fn `random(dist: DiscreteDistribution) -> int`
Requires `RANDOM` permission.

Returns a random number from `dist`.

## fn `sample(dist: DiscreteDistribution, n: int) -> Sequence<int>`
Requires `RANDOM` permission.

Returns `n` random numbers from `dist`.

## fn `sample_distribution(s: Sequence<int>) -> DiscreteDistribution`

Returns a discrete distribution from a sample. The distribution is defined by a list of integers, where each integer is the number of times that value appears in the sample.

## fn `skewness(dist: DiscreteDistribution) -> float`

Returns the skewness of `dist`. If the distribution has no skew, an error is returned.

## fn `std_dev(dist: DiscreteDistribution) -> float`

Returns the standard deviation of `dist`.

## fn `uniform_distribution(a: int, b: int) -> DiscreteDistribution`

Returns a uniform distribution between `a` and `b` (inclusive).

## fn `variance(dist: DiscreteDistribution) -> float`

Returns the variance of `dist`.

## fn `z_score(dist: DiscreteDistribution, x: int) -> float`

Returns the z-score of `x` in `dist`. That is, returns `(x - mean(dist)) / std_dev(dist)`.