# Moran spectral randomization null model

Generates spatially-constrained surrogate brain maps using Moran's
eigenvector maps (MEMs) for spectral randomization.

## Usage

``` r
null_moran(
  data,
  distmat,
  n_perm = 1000L,
  seed = NULL,
  procedure = c("pair", "singleton"),
  kernel = c("inverse_distance", "exponential", "gaussian", "bisquare"),
  tol = 1e-06
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

- procedure:

  Character, either `"pair"` (default, random 2D rotations of
  near-degenerate eigenvector pairs, matches neuromaps Python) or
  `"singleton"` (random sign flips of individual eigenvectors).

- kernel:

  Weight matrix kernel: `"inverse_distance"` (default, matches neuromaps
  Python), `"exponential"`, `"gaussian"`, or `"bisquare"`.

- tol:

  Numeric tolerance for eigenvalue comparison.

## Value

A
[null_distribution](https://lcbc-uio.github.io/neuromapr/reference/null_distribution.md)
object.

## References

Wagner HH, Dray S (2015) Methods in Ecology and Evolution 6:1169-1178.
doi:10.1111/2041-210X.12407
