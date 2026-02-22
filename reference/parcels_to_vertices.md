# Map parcel data back to vertices

Expands parcel-level values to a vertex-level vector using parcellation
labels.

## Usage

``` r
parcels_to_vertices(parcel_data, labels, fill = NA_real_)
```

## Arguments

- parcel_data:

  Named numeric vector of parcel values (names match labels).

- labels:

  Integer vector of parcel labels. `0` and `NA` are medial wall.

- fill:

  Value for medial wall vertices (default: `NA_real_`).

## Value

Numeric vector of `length(labels)`.

## References

Markello RD et al. (2022) Nature Methods 19:1472-1480.
doi:10.1038/s41592-022-01625-w

## Examples

``` r
parcel_data <- c("1" = 10, "2" = 20)
labels <- c(1L, 1L, 2L, 2L, 0L)
parcels_to_vertices(parcel_data, labels)
#> [1] 10 10 20 20 NA
```
