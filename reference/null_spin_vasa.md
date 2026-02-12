# Spin-test null models for brain maps

Generate spatially-constrained null distributions using spin-based
permutation of spherical coordinates.

## Usage

``` r
null_spin_vasa(
  data,
  coords,
  n_perm = 1000L,
  seed = NULL,
  rotation = c("euler", "rodrigues")
)

null_spin_hungarian(
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

Vasa F et al. (2018) Cerebral Cortex 28:3293-3303.
doi:10.1093/cercor/bhx195

Markello RD, Misic B (2021) NeuroImage 236:118052.
doi:10.1016/j.neuroimage.2021.118052
