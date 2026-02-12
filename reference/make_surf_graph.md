# Build an igraph from a triangular surface mesh

Extracts unique edges from triangular faces, computes Euclidean edge
weights, and returns an igraph graph object suitable for geodesic
distance computation.

## Usage

``` r
make_surf_graph(vertices, faces)
```

## Arguments

- vertices:

  Numeric matrix (n x 3) of vertex coordinates.

- faces:

  Integer matrix (m x 3) of face indices (1-indexed).

## Value

An `igraph` graph object with weighted edges.

## References

Markello RD et al. (2022) Nature Methods 19:1472-1480.
doi:10.1038/s41592-022-01625-w
