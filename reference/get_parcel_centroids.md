# Compute parcel centroids

Finds the centroid of each parcel using one of three methods.

## Usage

``` r
get_parcel_centroids(
  vertices,
  labels,
  method = c("average", "surface", "geodesic"),
  faces = NULL
)
```

## Arguments

- vertices:

  Numeric matrix (n x 3) of vertex coordinates.

- labels:

  Integer vector of parcel labels. `0` and `NA` are medial wall.

- method:

  Centroid method: `"average"` (coordinate means), `"surface"` (vertex
  closest to the average centroid), or `"geodesic"` (vertex minimizing
  sum of geodesic distances within parcel).

- faces:

  Integer matrix (m x 3) of face indices. Required for `"geodesic"`
  method.

## Value

Numeric matrix (n_parcels x 3) with rownames set to parcel labels.

## References

Markello RD et al. (2022) Nature Methods 19:1472-1480.
doi:10.1038/s41592-022-01625-w

## Examples

``` r
vertices <- matrix(rnorm(30), ncol = 3)
labels <- c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 3L)
get_parcel_centroids(vertices, labels, method = "average")
#>         [,1]       [,2]       [,3]
#> 1 -0.3882299  0.1406222  0.4134827
#> 2  0.2158690 -0.6238162  0.1283738
#> 3  0.2143412 -0.6343495 -0.5199403
```
