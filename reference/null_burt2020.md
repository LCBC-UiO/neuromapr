# Variogram-matching null model

Generates spatially-constrained surrogate brain maps by matching the
empirical variogram of the original data through smoothed random
permutations.

## Usage

``` r
null_burt2020(
  data,
  distmat,
  n_perm = 1000L,
  seed = NULL,
  ns = 500L,
  nh = 25L,
  pv = 25,
  knn = 1000L,
  deltas = seq(0.1, 0.9, by = 0.1),
  kernel = c("exponential", "gaussian", "uniform"),
  resample = FALSE
)
```

## Arguments

- data:

  Numeric vector of brain map values.

- distmat:

  Distance matrix between parcels/vertices.

- n_perm:

  Integer number of null permutations to generate.

- seed:

  Optional integer seed for reproducibility.

- ns:

  Integer, subsample size for variogram computation.

- nh:

  Integer, number of distance bins for variogram.

- pv:

  Numeric, percentile cutoff for maximum distance in variogram.

- knn:

  Integer, number of nearest neighbors for smoothing.

- deltas:

  Numeric vector of smoothing levels (fractions of `knn`).

- kernel:

  Smoothing kernel function.

- resample:

  Logical. If `FALSE` (default, matches brainsmash Python), variogram
  subsample indices are computed once and reused across all
  permutations. If `TRUE`, a fresh subsample is drawn each time.

## Value

A
[null_distribution](https://lcbc-uio.github.io/neuromapr/reference/null_distribution.md)
object.

## References

Burt JB et al. (2020) NeuroImage 220:117038.
doi:10.1016/j.neuroimage.2020.117038
