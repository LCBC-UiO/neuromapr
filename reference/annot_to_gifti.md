# Convert FreeSurfer annotation to GIFTI

Reads a FreeSurfer `.annot` file and writes a GIFTI label file.

## Usage

``` r
annot_to_gifti(annot_path, output_path = NULL)
```

## Arguments

- annot_path:

  Path to FreeSurfer `.annot` file.

- output_path:

  Output GIFTI path. If `NULL`, replaces the extension with
  `.label.gii`.

## Value

The output file path (invisibly).

## References

Markello RD et al. (2022) Nature Methods 19:1472-1480.
doi:10.1038/s41592-022-01625-w
