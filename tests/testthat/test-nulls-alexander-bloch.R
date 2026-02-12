describe("null_alexander_bloch", {
  it("returns null_distribution with correct dims", {
    set.seed(42)
    n_lh <- 10
    n_rh <- 10
    coords <- list(
      lh = matrix(rnorm(n_lh * 3), ncol = 3),
      rh = matrix(rnorm(n_rh * 3), ncol = 3)
    )
    data <- rnorm(n_lh + n_rh)

    result <- null_alexander_bloch(data, coords, n_perm = 5L, seed = 1)
    expect_s3_class(result, "null_distribution")
    expect_equal(result$method, "alexander_bloch")
    expect_equal(nrow(result$nulls), 20)
    expect_equal(ncol(result$nulls), 5)
  })

  it("errors for data length mismatch", {
    coords <- list(
      lh = matrix(rnorm(15), ncol = 3),
      rh = matrix(rnorm(15), ncol = 3)
    )
    expect_error(
      null_alexander_bloch(1:3, coords, n_perm = 2L),
      "must match"
    )
  })

  it("produces different values per permutation", {
    set.seed(42)
    n_lh <- 10
    n_rh <- 10
    coords <- list(
      lh = matrix(rnorm(n_lh * 3), ncol = 3),
      rh = matrix(rnorm(n_rh * 3), ncol = 3)
    )
    data <- rnorm(n_lh + n_rh)

    result <- null_alexander_bloch(data, coords, n_perm = 3L, seed = 1)
    expect_false(all(result$nulls[, 1] == result$nulls[, 2]))
  })
})
