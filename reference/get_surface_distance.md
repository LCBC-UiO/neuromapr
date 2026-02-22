# Compute geodesic distances on a surface mesh

Builds a graph from a triangular mesh and computes shortest-path
(Dijkstra) distances between vertices.

## Usage

``` r
get_surface_distance(vertices, faces, source_vertices = NULL)
```

## Arguments

- vertices:

  Numeric matrix (n x 3) of vertex coordinates.

- faces:

  Integer matrix (m x 3) of face indices (1-indexed).

- source_vertices:

  Optional integer vector of source vertex indices. If `NULL`, computes
  the full n x n distance matrix.

## Value

Numeric distance matrix. If `source_vertices` is provided, returns a
`length(source_vertices) x n` matrix; otherwise an `n x n` matrix.

## References

Markello RD et al. (2022) Nature Methods 19:1472-1480.
doi:10.1038/s41592-022-01625-w

## Examples

``` r
vertices <- matrix(
  c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1),
  nrow = 4, byrow = TRUE
)
faces <- matrix(c(1L, 2L, 3L, 2L, 3L, 4L), nrow = 2, byrow = TRUE)
get_surface_distance(vertices, faces)
#>          [,1]     [,2]     [,3]     [,4]
#> [1,] 0.000000 1.000000 1.000000 2.414214
#> [2,] 1.000000 0.000000 1.414214 1.414214
#> [3,] 1.000000 1.414214 0.000000 1.414214
#> [4,] 2.414214 1.414214 1.414214 0.000000
```
