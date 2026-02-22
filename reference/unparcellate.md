# Unparcellate brain map data

Inverse of
[`parcellate()`](https://lcbc-uio.github.io/neuromapr/reference/parcellate.md):
maps parcel-level values back to vertices.

## Usage

``` r
unparcellate(parcel_data, parcellation, fill = NA_real_)
```

## Arguments

- parcel_data:

  Named numeric vector of parcel values.

- parcellation:

  Integer vector of labels or file path to a GIFTI label file.

- fill:

  Value for medial wall vertices (default: `NA_real_`).

## Value

Numeric vector of vertex-level values.

## References

Markello RD et al. (2022) Nature Methods 19:1472-1480.
doi:10.1038/s41592-022-01625-w

## Examples

``` r
parcel_data <- c("1" = 10, "2" = 20)
labels <- c(1L, 1L, 2L, 2L, 0L)
unparcellate(parcel_data, labels)
#> [1] 10 10 20 20 NA
```
