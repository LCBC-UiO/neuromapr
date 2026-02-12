describe("new_null_distribution", {
  it("creates a valid null_distribution object", {
    nulls <- matrix(rnorm(100), nrow = 10, ncol = 10)
    observed <- rnorm(10)
    nd <- new_null_distribution(nulls, "burt2020", observed, list(nh = 25))

    expect_s3_class(nd, "null_distribution")
    expect_equal(nd$method, "burt2020")
    expect_equal(nd$n_perm, 10)
    expect_equal(nd$n, 10)
    expect_identical(nd$nulls, nulls)
    expect_identical(nd$observed, observed)
    expect_equal(nd$params, list(nh = 25))
  })
})

describe("validate_null_distribution", {
  it("passes for valid objects", {
    nulls <- matrix(rnorm(50), nrow = 5, ncol = 10)
    nd <- new_null_distribution(nulls, "moran", rnorm(5))
    expect_invisible(validate_null_distribution(nd))
  })

  it("errors for non-null_distribution", {
    expect_error(validate_null_distribution(list()), "null_distribution")
  })

  it("errors when observed length mismatches nulls rows", {
    nulls <- matrix(rnorm(50), nrow = 5, ncol = 10)
    nd <- new_null_distribution(nulls, "moran", rnorm(3))
    expect_error(validate_null_distribution(nd), "length")
  })
})

describe("print.null_distribution", {
  it("prints method, permutations, and observations", {
    nulls <- matrix(rnorm(50), nrow = 5, ncol = 10)
    nd <- new_null_distribution(nulls, "moran", rnorm(5))
    expect_snapshot(print(nd))
  })

  it("returns object invisibly", {
    nulls <- matrix(rnorm(50), nrow = 5, ncol = 10)
    nd <- new_null_distribution(nulls, "moran", rnorm(5))
    expect_invisible(print(nd))
  })
})

describe("summary.null_distribution", {
  it("returns expected summary fields", {
    nulls <- matrix(rnorm(50), nrow = 5, ncol = 10)
    nd <- new_null_distribution(nulls, "burt2020", rnorm(5))
    s <- summary(nd)

    expect_equal(s$method, "burt2020")
    expect_equal(s$n_perm, 10)
    expect_equal(s$n, 5)
    expect_length(s$null_mean, 5)
    expect_length(s$null_sd, 5)
  })
})

describe("as.matrix.null_distribution", {
  it("returns the nulls matrix", {
    nulls <- matrix(1:20, nrow = 4, ncol = 5)
    nd <- new_null_distribution(nulls, "moran", 1:4)
    expect_identical(as.matrix(nd), nulls)
  })
})

describe("plot.null_distribution", {
  it("returns a ggplot object when ggplot2 available", {
    skip_if_not_installed("ggplot2")
    nulls <- matrix(rnorm(100), nrow = 10, ncol = 10)
    nd <- new_null_distribution(nulls, "burt2020", rnorm(10))
    p <- plot(nd, parcel = 1)
    expect_s3_class(p, "gg")
  })
})
