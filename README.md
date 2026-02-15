
<!-- README.md is generated from README.Rmd. Please edit that file -->

# neuromapr <a href="https://lcbc-uio.github.io/neuromapr/"><img src="man/figures/logo.png" align="right" height="138" alt="neuromapr website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Brain maps are spatially autocorrelated — nearby regions have similar
values just because they are neighbours. Standard correlation tests
ignore this and produce inflated p-values. neuromapr generates
spatially-constrained surrogate maps that preserve autocorrelation
structure, so you can test whether the similarity between two brain maps
is more than a spatial artifact.

The package implements the framework described in [Markello et
al. (2022)](https://doi.org/10.1038/s41592-022-01625-w) and provides
eight null model methods, parcellation utilities, geodesic surface
distance, and coordinate-space transforms.

This package was co-developed with [Claude
Code](https://docs.anthropic.com/en/docs/agents-and-tools/claude-code/overview)
(Anthropic’s Claude Opus 4.6). While the test suite is thorough and
defaults are aligned with the
[neuromaps](https://netneurolab.github.io/neuromaps/) Python reference
implementation, discrepancies may still exist at this early stage. We
encourage users to cross-validate results against
[neuromaps](https://github.com/netneurolab/neuromaps) and to [report any
issues](https://github.com/lcbc-uio/neuromapr/issues).

## Installation

Install from the lcbc-uio
[r-universe](https://lcbc-uio.r-universe.dev/):

``` r
options(repos = c(
  lcbcuio = "https://lcbc-uio.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))

install.packages("neuromapr")
```

Or install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("lcbc-uio/neuromapr")
```

## Quick start

``` r
library(neuromapr)
```

Compare two brain maps with a spatial null model in three lines:

``` r
set.seed(42)
n <- 100
distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))

map_a <- rnorm(n)
map_b <- 0.4 * map_a + rnorm(n, sd = 0.8)

compare_maps(
  map_a,
  map_b,
  null_method = "burt2020",
  distmat = distmat,
  n_perm = 500L,
  seed = 1,
  verbose = FALSE
)
#> 
#> ── Brain Map Comparison
#> Method: pearson
#> r = 0.5302, p = 1.4e-08
#> n = 100
#> ────────────────────────────────────────────────────────────────────────────────
#> Null model: burt2020 (500 permutations)
#> p_null = 0.002
```

The `p_null` accounts for spatial autocorrelation. The parametric `p`
does not.

## Null model methods

neuromapr provides eight methods across three families:

| Family         | Method                      | Input required               |
|----------------|-----------------------------|------------------------------|
| Distance-based | `burt2020` (variogram)      | Distance matrix              |
|                | `burt2018` (SAR model)      | Distance matrix              |
|                | `moran` (spectral)          | Distance matrix              |
| Spin-based     | `alexander_bloch`           | Sphere coordinates           |
|                | `spin_vasa` (greedy)        | Sphere coordinates           |
|                | `spin_hungarian` (optimal)  | Sphere coordinates           |
| Parcel spin    | `baum` (max overlap)        | Sphere coords + parcellation |
|                | `cornblath` (majority vote) | Sphere coords + parcellation |

All methods are accessed through `generate_nulls()` or directly via
their dedicated functions (e.g., `null_burt2020()`, `null_moran()`).

## Key features

**Parcellation utilities** — aggregate vertex data into parcels, map
parcel values back to vertices, and compute parcel centroids using
average, surface, or geodesic methods.

``` r
vertex_data <- rnorm(1000)
labels <- rep(1:10, each = 100)

parcel_means <- vertices_to_parcels(vertex_data, labels)
head(parcel_means)
#>           1           2           3           4           5           6 
#>  0.11795689 -0.03426519 -0.05073563  0.04712785  0.06088535 -0.04200267
```

**Geodesic surface distance** — shortest-path distances along a
triangular mesh, for analyses where Euclidean distance through the brain
does not reflect cortical proximity.

``` r
vertices <- matrix(
  c(0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0),
  ncol = 3,
  byrow = TRUE
)
faces <- matrix(c(1, 2, 3, 2, 3, 4), ncol = 3, byrow = TRUE)

get_surface_distance(vertices, faces)
#>      [,1]     [,2]     [,3] [,4]
#> [1,]    0 1.000000 1.000000    2
#> [2,]    1 0.000000 1.414214    1
#> [3,]    1 1.414214 0.000000    1
#> [4,]    2 1.000000 1.000000    0
```

**Custom metric testing** — `permtest_metric()` runs a permutation test
with any metric function, optionally using spatial null surrogates:

``` r
result <- permtest_metric(
  map_a,
  map_b,
  metric_func = function(x, y) mean(abs(x - y)),
  n_perm = 200L,
  seed = 1
)
result$p_value
#> [1] 1
```

**Format conversions** — convert FreeSurfer `.annot` and morphometry
files to GIFTI with `annot_to_gifti()` and `fsmorph_to_gifti()`.

## Citation

If you use neuromapr, please cite the package and the neuromaps
framework it implements. You can get the package citation from R with:

``` r
citation("neuromapr")
```

Mowinckel A (2026). *neuromapr: Spatial Null Models and Transforms for
Brain Map Comparison*. R package version 0.2.0.

> Markello RD, Hansen JY, Liu Z-Q, et al. (2022). neuromaps: structural
> and functional interpretation of brain maps. *Nature Methods*, 19,
> 1472–1480. <doi:10.1038/s41592-022-01625-w>

Individual null model methods have their own citations documented in
each function’s help page.
