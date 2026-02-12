# Alexander-Bloch spin test null model

Original vertex-level spin test: rotates coordinates and assigns each
rotated vertex the value of its nearest original vertex (no optimal
matching).

## Usage

``` r
null_alexander_bloch(
  data,
  coords,
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

Alexander-Bloch AF et al. (2018) NeuroImage 175:111-120.
doi:10.1016/j.neuroimage.2018.04.023
