# Baum spin test null model

Spin-based null model with maximum-overlap parcel reassignment. After
rotating vertex coordinates, each original parcel is assigned the value
of the rotated parcel with the most vertex overlap.

## Usage

``` r
null_baum(
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

Baum GL et al. (2020) PNAS 117:21854-21861. doi:10.1073/pnas.2005518117
