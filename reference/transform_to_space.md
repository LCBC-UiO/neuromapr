# Transform brain maps between coordinate spaces

Resamples GIFTI surface files between `fsaverage` and `fsLR` coordinate
spaces using Connectome Workbench via `ciftiTools`.

## Usage

``` r
transform_to_space(
  paths,
  target_space = c("fsLR", "fsaverage"),
  target_density = "32k",
  hemisphere = c("left", "right"),
  method = c("adaptive", "barycentric"),
  area_surf_current = NULL,
  area_surf_new = NULL,
  wb_path = NULL,
  verbose = TRUE
)
```

## Arguments

- paths:

  Character vector of GIFTI file paths to transform.

- target_space:

  Target coordinate space: `"fsLR"` or `"fsaverage"`.

- target_density:

  Target mesh density (e.g., `"32k"`, `"164k"`).

- hemisphere:

  Which hemispheres to transform: `"left"`, `"right"`, or both.

- method:

  Resampling method: `"adaptive"` (default) or `"barycentric"`.

- area_surf_current:

  Path to the current-resolution area-correction surface (e.g.
  midthickness). Recommended when `method = "adaptive"` for proper
  vertex-area correction (matches neuromaps Python behaviour).

- area_surf_new:

  Path to the target-resolution area-correction surface. If `NULL` and
  `area_surf_current` is provided, ciftiTools resamples it
  barycentrically.

- wb_path:

  Path to `wb_command` executable. If `NULL`, auto-detected via
  `ciftiTools`.

- verbose:

  Logical, print progress messages.

## Value

Character vector of transformed file paths.

## References

Robinson EC et al. (2014) NeuroImage 100:414-426.
doi:10.1016/j.neuroimage.2014.07.033

Robinson EC et al. (2018) NeuroImage 167:150-165.
doi:10.1016/j.neuroimage.2017.10.037
