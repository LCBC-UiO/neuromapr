# Aggregate vertex data into parcels

Summarises vertex-level data by parcellation labels.

## Usage

``` r
vertices_to_parcels(data, labels, summary_func = mean)
```

## Arguments

- data:

  Numeric vector of vertex-level values.

- labels:

  Integer vector of parcel labels. `0` and `NA` are treated as medial
  wall and excluded.

- summary_func:

  Function to summarise each parcel (default:
  [mean](https://rdrr.io/r/base/mean.html)).

## Value

Named numeric vector of parcel-level values.

## References

Markello RD et al. (2022) Nature Methods 19:1472-1480.
doi:10.1038/s41592-022-01625-w
