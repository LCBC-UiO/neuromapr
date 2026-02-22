# Cornblath spin test null model

Spin-based null model where each rotated vertex receives the label of
its nearest non-medial-wall original vertex, then parcels are reassigned
by majority vote among the resulting vertex labels.

## Usage

``` r
null_cornblath(
  data,
  coords,
  parcellation,
  n_perm = 1000L,
  seed = NULL,
  rotation = c("euler", "rodrigues")
)
```

## Arguments

- data:

  Numeric vector of brain map values.

- coords:

  List with `$lh` and `$rh` matrices of spherical coordinates (n x 3
  each).

- parcellation:

  Integer vector of parcel labels for all vertices. `0` and `NA`
  indicate medial wall.

- n_perm:

  Integer number of null permutations to generate.

- seed:

  Optional integer seed for reproducibility.

- rotation:

  Rotation generation method: `"euler"` (ZYZ Euler angles, default,
  matches neuromaps Python) or `"rodrigues"` (Rodrigues axis-angle
  formula).

## Value

A
[null_distribution](https://lcbc-uio.github.io/neuromapr/reference/null_distribution.md)
object.

## References

Cornblath EJ et al. (2020) Communications Biology 3:590.
doi:10.1038/s42003-020-01296-5

## Examples

``` r
coords <- list(lh = matrix(rnorm(30), 10, 3), rh = matrix(rnorm(30), 10, 3))
parcellation <- c(rep(1L, 5), rep(2L, 5), rep(3L, 5), rep(4L, 5))
data <- c(1.0, 2.0, 3.0, 4.0)
nd <- null_cornblath(data, coords, parcellation, n_perm = 10L, seed = 1L)
```
