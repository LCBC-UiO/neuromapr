# List available neuromaps annotations

Query the neuromaps registry to see which brain map annotations are
available for download. Data is fetched from the
[neuromaps](https://github.com/netneurolab/neuromaps) project's GitHub
repository and cached for the session.

## Usage

``` r
neuromaps_available(
  source = NULL,
  desc = NULL,
  space = NULL,
  density = NULL,
  resolution = NULL,
  hemisphere = NULL,
  tags = NULL,
  format = NULL
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

- tags:

  Character vector of tags. All must match (AND logic).

- format:

  Filter by format (`"surface"` or `"volume"`).

## Value

A tibble of available annotations with columns: source, desc, space,
den, res, hemi, format, fname, full_desc, tags, N, age.

## Details

When used in `neuromaps_available()`, all parameters act as regex
filters. When used in
[`fetch_neuromaps_annotation()`](https://lcbc-uio.github.io/neuromapr/reference/fetch_neuromaps_annotation.md),
`source`, `desc`, and `space` are exact matches.

## Examples

``` r
if (FALSE) { # \dontrun{
neuromaps_available()
neuromaps_available(source = "beliveau")
neuromaps_available(tags = "pet")
} # }
```
