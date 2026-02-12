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
