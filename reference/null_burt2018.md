# Burt 2018 spatial autoregressive null model

Generates surrogate brain maps using a spatial autoregressive (SAR)
model. Estimates spatial autocorrelation and distance decay parameters,
then generates surrogates by solving the SAR equation and rank-matching
to the original data.

## Usage

``` r
null_burt2018(data, distmat, n_perm = 1000L, seed = NULL)
```

## Arguments

- data:

  Numeric vector of brain map values.

- distmat:

  Distance matrix between parcels/vertices.

- n_perm:

  Integer number of null permutations to generate.

- seed:

  Optional integer seed for reproducibility.

## Value

A
[null_distribution](https://lcbc-uio.github.io/neuromapr/reference/null_distribution.md)
object.

## References

Burt JB et al. (2018) Nature Neuroscience 21:1251-1259.
doi:10.1038/s41593-018-0195-0
