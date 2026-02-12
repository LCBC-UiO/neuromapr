# Download a neuromaps annotation

Download brain map annotation files from the neuromaps OSF repository.
Surface annotations return two files (left and right hemispheres),
volume annotations return one.

## Usage

``` r
fetch_neuromaps_annotation(
  source,
  desc,
  space,
  density = NULL,
  resolution = NULL,
  hemisphere = NULL,
  data_dir = neuromaps_cache_dir(),
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- source:

  Data source identifier (e.g. `"abagen"`, `"beliveau"`).

- desc:

  Map descriptor key (e.g. `"genepc1"`, `"feobv"`).

- space:

  Coordinate space (e.g. `"fsaverage"`, `"MNI152"`, `"fsLR"`).

- density:

  Surface vertex density (e.g. `"10k"`, `"164k"`). Mutually exclusive
  with `resolution`.

- resolution:

  Volume voxel resolution (e.g. `"1mm"`, `"2mm"`). Mutually exclusive
  with `density`.

- hemisphere:

  Hemisphere (`"L"` or `"R"`).

- data_dir:

  Directory for cached downloads. Defaults to `neuromaps_cache_dir()`.

- overwrite:

  Re-download even if cached file exists.

- verbose:

  Print progress messages.

## Value

Character vector of downloaded file path(s).

## Examples

``` r
if (FALSE) { # \dontrun{
fetch_neuromaps_annotation("abagen", "genepc1", "fsaverage", density = "10k")
} # }
```
