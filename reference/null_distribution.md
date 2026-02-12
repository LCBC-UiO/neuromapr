# Create a null distribution object

Create a null distribution object

## Usage

``` r
new_null_distribution(nulls, method, observed, params = list())
```

## Arguments

- nulls:

  Numeric matrix (n x n_perm) of surrogate values.

- method:

  Character string identifying the null model method.

- observed:

  Numeric vector of original data values.

- params:

  Named list of algorithm parameters.

## Value

A `null_distribution` object.
