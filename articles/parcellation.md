# Working with Parcellated Data

Most brain imaging analyses eventually need to move between two
resolutions: vertex-level (one value per surface point, typically
32k–164k per hemisphere) and parcel-level (one value per brain region,
typically 50–400). A cortical thickness map is vertex-level. A table of
regional volumes from the Desikan-Killiany atlas is parcel-level.
Comparing the two requires converting one to match the other.

neuromapr provides tools for this conversion—aggregating vertices into
parcels, mapping parcel values back to vertices, and computing parcel
centroids on the cortical surface.

``` r
library(neuromapr)
```

## Vertices to parcels

Suppose you have a vertex-level brain map and a parcellation that
assigns each vertex to a region. The parcellation is an integer vector
where each vertex gets a label, with `0` or `NA` marking the medial wall
(vertices that belong to no region).

``` r
set.seed(42)
n_vertices <- 1000
vertex_data <- rnorm(n_vertices)

labels <- sample(
  c(0, 1:10), n_vertices,
  replace = TRUE, prob = c(0.1, rep(0.09, 10))
)

parcel_values <- vertices_to_parcels(vertex_data, labels)
parcel_values
#>            1            2            3            4            5            6 
#> -0.001235255  0.142132938 -0.055766033 -0.162060178  0.027356454 -0.019617141 
#>            7            8            9           10 
#> -0.114211335 -0.047980452 -0.108683119  0.018359997
```

Each parcel gets the mean of its constituent vertices. Medial wall
vertices (label 0) are excluded automatically.

The summary function is configurable:

``` r
parcel_sd <- vertices_to_parcels(
  vertex_data, labels, summary_func = sd
)
parcel_sd
#>         1         2         3         4         5         6         7         8 
#> 1.0683513 1.0550975 0.9958556 1.0257398 1.0019210 0.8117328 0.9765293 1.0108265 
#>         9        10 
#> 1.0698179 0.9825964
```

## Parcels back to vertices

The reverse operation paints parcel-level values back onto the vertex
mesh. Every vertex gets its parcel’s value; medial wall vertices get a
fill value.

``` r
vertex_filled <- parcels_to_vertices(parcel_values, labels)
head(vertex_filled, 20)
#>  [1] -0.001235255  0.027356454 -0.047980452  0.018359997  0.018359997
#>  [6] -0.108683119 -0.047980452  0.018359997  0.142132938 -0.162060178
#> [11] -0.114211335  0.142132938 -0.055766033 -0.047980452           NA
#> [16] -0.108683119 -0.114211335 -0.047980452 -0.108683119 -0.001235255
```

Vertices with label 0 receive `NA` by default. You can change the fill:

``` r
vertex_filled_zero <- parcels_to_vertices(
  parcel_values, labels, fill = 0
)
sum(vertex_filled_zero == 0)
#> [1] 95
```

## The high-level wrappers

[`parcellate()`](https://lcbc-uio.github.io/neuromapr/reference/parcellate.md)
and
[`unparcellate()`](https://lcbc-uio.github.io/neuromapr/reference/unparcellate.md)
add file-reading convenience on top of the low-level functions. If your
data lives on disk as GIFTI files, pass paths directly:

``` r
parcel_vals <- parcellate(
  "cortical_thickness.func.gii",
  "aparc.label.gii"
)
```

The function reads the data file, reads the parcellation labels,
validates that they match in length, and calls
[`vertices_to_parcels()`](https://lcbc-uio.github.io/neuromapr/reference/vertices_to_parcels.md)
underneath.

When working with vectors already in memory, the wrappers still
work—they skip the file-reading step:

``` r
parcellated <- parcellate(vertex_data, labels)
unparcellated <- unparcellate(parcellated, labels)

all.equal(
  parcels_to_vertices(parcel_values, labels),
  unparcellated
)
#> [1] TRUE
```

The roundtrip is not lossless—parcellation is lossy aggregation. But
`unparcellate(parcellate(x))` gives you parcel means broadcast back to
vertex positions, which is the expected behaviour.

## Parcel centroids

Some analyses need to know where each parcel lives in 3D space—for
example, to compute a parcel-level distance matrix.
[`get_parcel_centroids()`](https://lcbc-uio.github.io/neuromapr/reference/get_parcel_centroids.md)
offers three methods for finding the representative point of each
parcel.

``` r
vertices <- matrix(rnorm(n_vertices * 3), ncol = 3)
```

### Average centroid

Take the mean x, y, z coordinates of all vertices in the parcel. Fast,
but the resulting point may not lie on the actual cortical surface.

``` r
centroids_avg <- get_parcel_centroids(
  vertices, labels, method = "average"
)
head(centroids_avg)
#>          [,1]         [,2]         [,3]
#> 1  0.11396575  0.016773018 -0.065814398
#> 2  0.02388696 -0.035682772 -0.020913744
#> 3  0.07952778  0.101717196  0.091118296
#> 4 -0.08853466 -0.052836153 -0.058780127
#> 5  0.01771226  0.044216347  0.046963635
#> 6 -0.03578315 -0.004064188 -0.008321535
```

### Surface centroid

Find the vertex closest to the average centroid. The result is always an
actual vertex on the surface, which matters when centroids must respect
the cortical geometry.

``` r
centroids_surf <- get_parcel_centroids(
  vertices, labels, method = "surface"
)
head(centroids_surf)
#>          [,1]        [,2]       [,3]
#> 1 -0.08089878  0.26659246  0.1365766
#> 2 -0.12375518 -0.01086631  0.1036001
#> 3  0.38467702 -0.10467462 -0.1120929
#> 4 -0.58301662  0.13731157 -0.1416120
#> 5 -0.40958087  0.15919765 -0.1902452
#> 6 -0.47109369 -0.34759792  0.1752621
```

### Geodesic centroid

The most principled approach: find the vertex that minimizes the sum of
geodesic (shortest-path-on-surface) distances to all other vertices in
the parcel. This requires the face matrix of the surface mesh and the
**igraph** package.

``` r
centroids_geo <- get_parcel_centroids(
  vertices, labels,
  method = "geodesic",
  faces = faces
)
```

Geodesic centroids are computationally expensive—they compute pairwise
shortest paths within each parcel. For large parcellations on
high-resolution meshes, the surface centroid is a practical
approximation.

## Building a parcel-level distance matrix

A common use case: you have parcellated data and need a distance matrix
for null model generation. Parcel centroids get you there.

``` r
parcel_distmat <- as.matrix(dist(centroids_avg))
dim(parcel_distmat)
#> [1] 10 10
```

This distance matrix feeds directly into
[`generate_nulls()`](https://lcbc-uio.github.io/neuromapr/reference/generate_nulls.md):

``` r
parcel_map <- rnorm(nrow(centroids_avg))
nulls <- generate_nulls(
  parcel_map,
  method = "moran",
  distmat = parcel_distmat,
  n_perm = 100L,
  seed = 1
)
nulls
#> 
#> ── Null Distribution
#> • Method: moran
#> • Permutations: 100
#> • Observations: 10
```

## Parcellation and null models together

The parcel spin methods (`baum` and `cornblath`) combine parcellation
with spin-based null generation. They need both spherical coordinates
and the parcellation vector:

``` r
n_lh <- 680
n_rh <- 680
sphere_coords <- list(
  lh = matrix(rnorm(n_lh * 3), ncol = 3),
  rh = matrix(rnorm(n_rh * 3), ncol = 3)
)
parcellation <- rep(1:68, each = 20)
parcel_data <- rnorm(68)

nulls_baum <- null_baum(
  parcel_data, sphere_coords, parcellation,
  n_perm = 50L, seed = 1
)
nulls_baum
#> 
#> ── Null Distribution
#> • Method: baum
#> • Permutations: 50
#> • Observations: 68
```

The rotation happens at vertex resolution, but the output is
parcel-level: one surrogate value per parcel per permutation. This is
the right approach when your analysis variable is measured at the parcel
level but you want the null model to respect vertex-level spatial
structure.

## Practical considerations

**Parcellation size matters for null models.** With 30–50 parcels,
distance-based methods like `burt2020` and `moran` work well. With
hundreds of parcels (e.g., Schaefer 400), spin-based methods become more
attractive because they leverage the full vertex-level spatial
structure.

**Medial wall handling.** All parcellation functions treat label 0 and
`NA` as medial wall. The `cornblath` null model handles medial wall
vertices gracefully during spin-based reassignment. If your parcellation
has substantial medial wall coverage, prefer `cornblath` over `baum`.

**The summary function determines what you are testing.** Using `mean`
(the default) tests whether the average value in each region relates to
another variable. Using `sd` would test whether within-region
variability relates to it. Choose deliberately.

## References

Markello RD, Hansen JY, Liu Z-Q, et al. (2022). neuromaps: structural
and functional interpretation of brain maps. *Nature Methods*, 19,
1472–1480. <doi:10.1038/s41592-022-01625-w>
