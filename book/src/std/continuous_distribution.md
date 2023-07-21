# Continuous Distribution

## type `ContinuousDistribution`

A continuous distribution over the real numbers.

## fn `beta_distribution(alpha: float, beta: float) -> ContinuousDistribution`

Returns a beta distribution with `alpha` and `beta` shape parameters.

## fn `cdf(dist: ContinuousDistribution, x: float) -> float`

Returns the cumulative distribution function of `dist` at `x`. The returned value will always be between `0` and `1`.

## fn `chisq_distribution(nu: float) -> ContinuousDistribution`

Returns a chi-squared distribution with `nu` degrees of freedom.

## fn `exponential_distribution(lambda: float) -> ContinuousDistribution`

Returns an exponential distribution with rate `lambda`.

## fn `fisher_snedecor_distribution(d1: float, d2: float) -> ContinuousDistribution`

Returns a Fisher-Snedecor distribution with `d1` and `d2` degrees of freedom.

## fn `gamma_distribution(shape: float, scale: float) -> ContinuousDistribution`

Returns a gamma distribution with `shape` and `scale` parameters.

## fn `lognormal_distribution(mean: float, stddev: float) -> ContinuousDistribution`

Returns a lognormal distribution with `mean` and `stddev` parameters.

## fn `mean(dist: ContinuousDistribution) -> float`

Returns the mean (expected value) of `dist`.

## fn `normal_distribution(mean: float, stddev: float) -> ContinuousDistribution`

Returns a normal distribution with `mean` and `stddev` parameters.

## fn `quantile(dist: ContinuousDistribution, p: float) -> float`

Returns the inverse-cdf of `dist` at `p`. That is, returns a value `x` such that `cdf(dist, x) == p`.

## fn `random() -> float`
Requires `RANDOM` permission.

Returns a random number between `0` and `1`, uniformly distributed.

## fn `random(dist: ContinuousDistribution) -> float`
Requires `RANDOM` permission.

Returns a random number from `dist`.

## fn `rectangular_distribution(a: float, b: float) -> ContinuousDistribution`

Returns a rectangular distribution (also called the continuous uniform distribution) between `a` and `b`.

## fn `sample(dist: ContinuousDistribution, n: int) -> Seqeunce<float>`
Requires `RANDOM` permission.

Returns `n` random numbers from `dist`.

## fn `skewness(dist: ContinuousDistribution) -> float`

Returns the skewness of `dist`. If the distribution has no skew, an error is returned.

## fn `standard_uniform_distribution() -> ContinuousDistribution`

Returns a standard uniform distribution (also called the continuous uniform distribution) between `0` and `1`.

## fn `std_dev(dist: ContinuousDistribution) -> float`

Returns the standard deviation of `dist`.  If the distribution has no standard deviation, an error is returned.

## fn `students_t_distribution(freedom: float, loc: float?, scale: float?) -> ContinuousDistribution`

Returns a Student's t distribution with `freedom` degrees of freedom. The `loc` and `scale` parameters can be used to shift and scale the distribution, and default to `0` and `1` respectively.

## fn `triangular_distribution(a: float, b: float, c: float?) -> ContinuousDistribution`

Returns a triangular distribution between `a` and `b`. If `c` is specified, the distribution will be symmetric about `c`. Otherwise, the distribution will be symmetric about the midpoint between `a` and `b`.

## fn `variance(dist: ContinuousDistribution) -> float`

Returns the variance of `dist`. If the distribution has no variance, an error is returned.

## fn `weibull_distribution(a: float, b: float) -> ContinuousDistribution`

Returns a Weibull distribution with `a` and `b` parameters.

## fn `z_score(dist: ContinuousDistribution, x: float) -> float`

Returns the z-score of `x` in `dist`. This value is equal to:

\\[
    \frac{x - mu}{sigma}
\\]

Where `mu` is the mean of `dist` and `sigma` is the standard deviation of `dist`.