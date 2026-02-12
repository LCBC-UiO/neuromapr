describe("permtest_metric", {
  it("returns correct structure with random permutation", {
    set.seed(42)
    x <- rnorm(50)
    y <- x + rnorm(50, sd = 0.5)

    result <- permtest_metric(x, y, n_perm = 99L, seed = 1)
    expect_true(is.list(result))
    expect_true("observed" %in% names(result))
    expect_true("null_values" %in% names(result))
    expect_true("p_value" %in% names(result))
    expect_true("n_perm" %in% names(result))
    expect_equal(length(result$null_values), 99)
    expect_equal(result$n_perm, 99L)
  })

  it("observed matches direct correlation", {
    set.seed(42)
    x <- rnorm(30)
    y <- rnorm(30)

    result <- permtest_metric(x, y, n_perm = 10L, seed = 1)
    expect_equal(result$observed, stats::cor(x, y))
  })

  it("accepts custom metric function", {
    set.seed(42)
    x <- rnorm(30)
    y <- rnorm(30)
    mae <- function(a, b) mean(abs(a - b))

    result <- permtest_metric(x, y, metric_func = mae, n_perm = 10L, seed = 1)
    expect_equal(result$observed, mae(x, y))
  })

  it("p_value is between 0 and 1", {
    set.seed(42)
    x <- rnorm(30)
    y <- rnorm(30)

    result <- permtest_metric(x, y, n_perm = 99L, seed = 1)
    expect_true(result$p_value >= 0 && result$p_value <= 1)
  })

  it("errors for length mismatch", {
    expect_error(
      permtest_metric(1:5, 1:3, n_perm = 10L),
      "same length"
    )
  })

  it("errors for non-numeric input", {
    expect_error(
      permtest_metric(letters[1:5], 1:5, n_perm = 10L),
      "numeric"
    )
  })

  it("works with spatial null method", {
    set.seed(42)
    n <- 20
    data_x <- rnorm(n)
    data_y <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))

    result <- permtest_metric(
      data_x, data_y,
      n_perm = 5L, seed = 1,
      null_method = "burt2018",
      distmat = distmat
    )
    expect_equal(length(result$null_values), 5)
    expect_true(is.numeric(result$p_value))
  })
})
