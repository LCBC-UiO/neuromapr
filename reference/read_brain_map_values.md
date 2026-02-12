# Read vertex/voxel values from a brain map file

Reads GIFTI (`.func.gii`) or NIfTI (`.nii.gz`) files and returns the
values as a numeric vector. Used internally by
[`compare_maps()`](https://lcbc-uio.github.io/neuromapr/reference/compare_maps.md)
when file paths are passed instead of numeric vectors.

## Usage

``` r
read_brain_map_values(path)
```

## Arguments

- path:

  Path to a `.func.gii` (GIFTI) or `.nii.gz` (NIfTI) file.

## Value

A numeric vector of map values.
