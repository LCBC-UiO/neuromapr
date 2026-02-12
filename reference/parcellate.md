# Parcellate brain map data

High-level wrapper that reads data and parcellation from file paths or
vectors, then aggregates vertices into parcels.

## Usage

``` r
parcellate(data, parcellation, summary_func = mean)
```

## Arguments

- data:

  Numeric vector or file path to a GIFTI/NIfTI brain map.

- parcellation:

  Integer vector of labels or file path to a GIFTI label file.

- summary_func:

  Function to summarise each parcel (default:
  [mean](https://rdrr.io/r/base/mean.html)).

## Value

Named numeric vector of parcel-level values.

## References

Markello RD et al. (2022) Nature Methods 19:1472-1480.
doi:10.1038/s41592-022-01625-w
