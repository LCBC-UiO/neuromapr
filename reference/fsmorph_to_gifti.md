# Convert FreeSurfer morphometry to GIFTI

Reads a FreeSurfer morphometry file (`.curv`, `.thickness`, `.sulc`) and
writes a GIFTI func file.

## Usage

``` r
fsmorph_to_gifti(morph_path, output_path = NULL)
```

## Arguments

- morph_path:

  Path to FreeSurfer morphometry file.

- output_path:

  Output GIFTI path. If `NULL`, replaces the extension with `.func.gii`.

## Value

The output file path (invisibly).

## References

Markello RD et al. (2022) Nature Methods 19:1472-1480.
doi:10.1038/s41592-022-01625-w
