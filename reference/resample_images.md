# Resample brain maps for comparison

Aligns two brain maps into the same coordinate space and density before
comparison. Supports multiple strategies for choosing the target space.

## Usage

``` r
resample_images(
  src,
  trg,
  src_space = c("fsaverage", "fsLR"),
  trg_space = c("fsaverage", "fsLR"),
  strategy = c("downsample_only", "transform_to_src", "transform_to_trg",
    "transform_to_alt"),
  alt_space = NULL,
  alt_density = NULL,
  hemisphere = c("left", "right"),
  area_surf_current = NULL,
  area_surf_new = NULL,
  wb_path = NULL,
  verbose = TRUE
)
```

## Arguments

- src:

  Character, file path to the source GIFTI.

- trg:

  Character, file path to the target GIFTI.

- src_space:

  Source coordinate space (`"fsaverage"` or `"fsLR"`).

- trg_space:

  Target coordinate space (`"fsaverage"` or `"fsLR"`).

- strategy:

  Resampling strategy. One of `"downsample_only"`, `"transform_to_src"`,
  `"transform_to_trg"`, or `"transform_to_alt"`.

- alt_space:

  Alternative space for `"transform_to_alt"` strategy.

- alt_density:

  Alternative density for `"transform_to_alt"` strategy.

- hemisphere:

  Which hemispheres: `"left"`, `"right"`, or both.

- area_surf_current:

  Path to a current-resolution area-correction surface (e.g.
  midthickness). Passed to
  [`transform_to_space()`](https://lcbc-uio.github.io/neuromapr/reference/transform_to_space.md).

- area_surf_new:

  Path to a target-resolution area-correction surface. Passed to
  [`transform_to_space()`](https://lcbc-uio.github.io/neuromapr/reference/transform_to_space.md).

- wb_path:

  Path to `wb_command` executable.

- verbose:

  Logical, print progress messages.

## Value

List with `$src` and `$trg` file paths in the aligned space.

## References

Markello RD et al. (2022) Nature Methods 19:1472-1480.
doi:10.1038/s41592-022-01625-w
