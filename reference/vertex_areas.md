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

## Examples

``` r
vertices <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3, byrow = TRUE)
faces <- matrix(c(1L, 2L, 3L), nrow = 1)
vertex_areas(vertices, faces)
#> [1] 0.1666667 0.1666667 0.1666667
```
