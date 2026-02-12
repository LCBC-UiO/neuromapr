describe("null_burt2018", {
  it("returns null_distribution with correct dims", {
    set.seed(42)
    n <- 20
    data <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))

    result <- null_burt2018(data, distmat, n_perm = 5L, seed = 1)
    expect_s3_class(result, "null_distribution")
    expect_equal(result$method, "burt2018")
    expect_equal(nrow(result$nulls), 20)
    expect_equal(ncol(result$nulls), 5)
  })

  it("preserves the rank distribution of original data", {
    set.seed(42)
    n <- 20
    data <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))

    result <- null_burt2018(data, distmat, n_perm = 3L, seed = 1)
    for (i in seq_len(3)) {
      expect_equal(sort(result$nulls[, i]), sort(data))
    }
  })

  it("stores estimated parameters", {
    set.seed(42)
    n <- 20
    data <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))

    result <- null_burt2018(data, distmat, n_perm = 3L, seed = 1)
    expect_true("rho" %in% names(result$params))
    expect_true("d0" %in% names(result$params))
    expect_true(is.numeric(result$params$rho))
    expect_true(is.numeric(result$params$d0))
  })

  it("errors for distmat dimension mismatch", {
    expect_error(
      null_burt2018(1:5, diag(3), n_perm = 2L),
      "5 x 5"
    )
  })
})
