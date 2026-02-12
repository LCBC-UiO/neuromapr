# Compare brain maps with spatial null model significance testing

Computes the correlation between two brain maps and optionally tests
significance using a spatial null model to account for spatial
autocorrelation.

## Usage

``` r
compare_maps(
  x,
  y,
  method = c("pearson", "spearman"),
  null_method = NULL,
  n_perm = 1000L,
  nulls = NULL,
  distmat = NULL,
  coords = NULL,
  seed = NULL,
  na.rm = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x, y:

  Numeric vectors of brain map values, or file paths to GIFTI/NIfTI
  files.

- method:

  Correlation method: `"pearson"` or `"spearman"`.

- null_method:

  Optional null model method for empirical p-values. One of
  `"burt2020"`, `"moran"`, `"spin_vasa"`, `"spin_hungarian"`,
  `"alexander_bloch"`, `"baum"`, `"cornblath"`, `"burt2018"`, or `NULL`
  for parametric only.

- n_perm:

  Integer number of null permutations.

- nulls:

  Pre-computed
  [null_distribution](https://lcbc-uio.github.io/neuromapr/reference/null_distribution.md)
  object for `x`.

- distmat:

  Distance matrix (passed to null model if needed).

- coords:

  Coordinate list (passed to spin null models if needed).

- seed:

  Optional integer seed for reproducibility.

- na.rm:

  Logical, remove NA values before computing correlation.

- verbose:

  Logical, print progress messages.

- ...:

  Additional arguments passed to
  [`generate_nulls()`](https://lcbc-uio.github.io/neuromapr/reference/generate_nulls.md).

## Value

A `neuromaps_enhanced_comparison` object (inherits
`neuromaps_comparison`) with additional fields `p_null`, `null_method`,
`null_r`, and `n_perm`.

## References

Markello RD et al. (2022) Nature Methods 19:1472-1480.
doi:10.1038/s41592-022-01625-w
