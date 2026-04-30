# Create a null distribution object

Create a null distribution object

## Usage

``` r
new_null_distribution(nulls, method, observed, params = list())

# S3 method for class 'null_distribution'
print(x, ...)

# S3 method for class 'null_distribution'
summary(object, ...)

# S3 method for class 'null_distribution'
as.matrix(x, ...)

# S3 method for class 'null_distribution'
plot(x, parcel = 1L, ...)
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

- x:

  A `null_distribution` object.

- ...:

  Ignored.

- object:

  A `null_distribution` object.

- parcel:

  Integer index of the parcel to plot.

## Value

A `null_distribution` object.

## Examples

``` r
nulls <- matrix(rnorm(30), nrow = 3, ncol = 10)
nd <- new_null_distribution(nulls, "test", observed = c(1, 2, 3))
print(nd)
#> 
#> ── Null Distribution 
#> • Method: test
#> • Permutations: 10
#> • Observations: 3
summary(nd)
#> $method
#> [1] "test"
#> 
#> $n_perm
#> [1] 10
#> 
#> $n
#> [1] 3
#> 
#> $null_mean
#> [1]  0.13277450  0.29899078 -0.03344153
#> 
#> $null_sd
#> [1] 0.8773569 0.8580055 0.6861845
#> 
#> $observed
#> [1] 1 2 3
#> 
```
