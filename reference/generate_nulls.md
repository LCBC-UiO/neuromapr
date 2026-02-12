# Generate null distributions for brain map data

Dispatches to the appropriate null model method for generating
spatially-constrained surrogate brain maps.

## Usage

``` r
generate_nulls(
  data,
  method = c("burt2020", "moran", "spin_vasa", "spin_hungarian", "alexander_bloch",
    "baum", "cornblath", "burt2018"),
  n_perm = 1000L,
  distmat = NULL,
  coords = NULL,
  parcellation = NULL,
  seed = NULL,
  ...
)
```

## Arguments

- data:

  Numeric vector of brain map values.

- method:

  Character string specifying the null model method.

- n_perm:

  Integer number of null permutations to generate.

- distmat:

  Distance matrix (required for `"burt2020"`, `"burt2018"`, and
  `"moran"`).

- coords:

  List with `$lh` and `$rh` coordinate matrices (required for spin
  methods).

- parcellation:

  Integer vector of parcel labels (required for `"baum"` and
  `"cornblath"`).

- seed:

  Optional integer seed for reproducibility.

- ...:

  Additional arguments passed to the specific null method (e.g.
  `rotation` for spin methods, `kernel` for moran/burt2020).

## Value

A
[null_distribution](https://lcbc-uio.github.io/neuromapr/reference/null_distribution.md)
object.

## References

Markello RD et al. (2022) Nature Methods 19:1472-1480.
doi:10.1038/s41592-022-01625-w
