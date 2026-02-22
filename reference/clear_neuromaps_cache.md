# Clear cached neuromaps registry data

Removes the session-level cache of the neuromaps annotation registry,
forcing a fresh download on the next call to
[`neuromaps_available()`](https://lcbc-uio.github.io/neuromapr/reference/neuromaps_available.md)
or
[`fetch_neuromaps_annotation()`](https://lcbc-uio.github.io/neuromapr/reference/fetch_neuromaps_annotation.md).

## Usage

``` r
clear_neuromaps_cache()
```

## Value

`NULL`, invisibly.

## Examples

``` r
clear_neuromaps_cache()
```
