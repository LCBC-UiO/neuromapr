# Permutation test for any metric between brain maps

Computes a user-specified metric between two vectors and tests
significance using either spatially-constrained null surrogates or
simple random permutation.

## Usage

``` r
permtest_metric(
  x,
  y,
  metric_func = stats::cor,
  n_perm = 1000L,
  seed = NULL,
  null_method = NULL,
  distmat = NULL,
  coords = NULL,
  parcellation = NULL,
  ...
)
```

## Arguments

- x, y:

  Numeric vectors.

- metric_func:

  Function taking `(x, y)` and returning a scalar.

- n_perm:

  Integer number of permutations.

- seed:

  Optional integer seed for reproducibility.

- null_method:

  Optional null model method passed to
  [`generate_nulls()`](https://lcbc-uio.github.io/neuromapr/reference/generate_nulls.md).
  If `NULL`, uses simple random permutation.

- distmat:

  Distance matrix (passed to
  [`generate_nulls()`](https://lcbc-uio.github.io/neuromapr/reference/generate_nulls.md)
  if needed).

- coords:

  Coordinate list (passed to
  [`generate_nulls()`](https://lcbc-uio.github.io/neuromapr/reference/generate_nulls.md)
  if needed).

- parcellation:

  Integer vector (passed to
  [`generate_nulls()`](https://lcbc-uio.github.io/neuromapr/reference/generate_nulls.md)
  if needed).

- ...:

  Additional arguments passed to
  [`generate_nulls()`](https://lcbc-uio.github.io/neuromapr/reference/generate_nulls.md).

## Value

List with `$observed`, `$null_values`, `$p_value`, and `$n_perm`.

## References

Markello RD et al. (2022) Nature Methods 19:1472-1480.
doi:10.1038/s41592-022-01625-w
