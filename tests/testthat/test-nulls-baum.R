describe("null_baum", {
  it("returns null_distribution with correct dims", {
    set.seed(42)
    n_lh <- 10
    n_rh <- 10
    n <- n_lh + n_rh
    coords <- list(
      lh = matrix(rnorm(n_lh * 3), ncol = 3),
      rh = matrix(rnorm(n_rh * 3), ncol = 3)
    )
    parcellation <- rep(1:4, each = 5)
    data <- c(10, 20, 30, 40)

    result <- null_baum(data, coords, parcellation, n_perm = 5L, seed = 1)
    expect_s3_class(result, "null_distribution")
    expect_equal(result$method, "baum")
    expect_equal(nrow(result$nulls), 4)
    expect_equal(ncol(result$nulls), 5)
  })

  it("errors for wrong data length", {
    n_lh <- 5
    n_rh <- 5
    coords <- list(
      lh = matrix(rnorm(n_lh * 3), ncol = 3),
      rh = matrix(rnorm(n_rh * 3), ncol = 3)
    )
    parcellation <- rep(1:2, each = 5)
    expect_error(
      null_baum(1:5, coords, parcellation, n_perm = 2L),
      "must match"
    )
  })

  it("null values are drawn from original data", {
    set.seed(42)
    n_lh <- 10
    n_rh <- 10
    coords <- list(
      lh = matrix(rnorm(n_lh * 3), ncol = 3),
      rh = matrix(rnorm(n_rh * 3), ncol = 3)
    )
    parcellation <- rep(1:4, each = 5)
    data <- c(10, 20, 30, 40)

    result <- null_baum(data, coords, parcellation, n_perm = 5L, seed = 1)
    non_na <- result$nulls[!is.na(result$nulls)]
    expect_true(all(non_na %in% data))
  })
})
