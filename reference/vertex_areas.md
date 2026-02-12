# Compute per-vertex surface areas

Each triangle distributes one-third of its area to each of its three
vertices. Triangle area is computed via the cross-product formula.

## Usage

``` r
vertex_areas(vertices, faces)
```

## Arguments

- vertices:

  Numeric matrix (n x 3) of vertex coordinates.

- faces:

  Integer matrix (m x 3) of face indices (1-indexed).

## Value

Numeric vector of length `nrow(vertices)`.
