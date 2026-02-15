describe("compare_maps", {
  it("computes parametric correlation", {
    set.seed(42)
    x <- rnorm(50)
    y <- x + rnorm(50, sd = 0.5)
    result <- compare_maps(x, y, verbose = FALSE)

    expect_s3_class(result, "neuromaps_enhanced_comparison")
    expect_s3_class(result, "neuromaps_comparison")
    expect_true(result$r > 0)
    expect_true(result$p < 0.05)
    expect_equal(result$n, 50)
    expect_null(result$p_null)
    expect_null(result$null_method)
  })

  it("supports spearman method", {
    set.seed(42)
    x <- rnorm(50)
    y <- x + rnorm(50, sd = 0.5)
    result <- compare_maps(x, y, method = "spearman", verbose = FALSE)
    expect_equal(result$method, "spearman")
  })

  it("handles NA removal", {
    x <- c(1, 2, NA, 4, 5)
    y <- c(5, 4, 3, NA, 1)
    result <- compare_maps(x, y, na.rm = TRUE, verbose = FALSE)
    expect_equal(result$n, 3)
  })

  it("errors for length mismatch", {
    expect_error(compare_maps(1:5, 1:3, verbose = FALSE), "same length")
  })

  it("computes empirical p with moran null", {
    set.seed(42)
    n <- 20
    x <- rnorm(n)
    y <- x + rnorm(n, sd = 0.5)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    result <- compare_maps(
      x, y,
      null_method = "moran",
      n_perm = 10L,
      distmat = distmat,
      seed = 1,
      verbose = FALSE
    )

    expect_s3_class(result, "neuromaps_enhanced_comparison")
    expect_true(!is.null(result$p_null))
    expect_true(result$p_null >= 0 && result$p_null <= 1)
    expect_equal(result$null_method, "moran")
    expect_length(result$null_r, 10)
    expect_equal(result$n_perm, 10)
  })

  it("accepts pre-computed null distribution", {
    set.seed(42)
    n <- 20
    x <- rnorm(n)
    y <- x + rnorm(n, sd = 0.5)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    nd <- null_moran(x, distmat, n_perm = 10L, seed = 1)

    result <- compare_maps(x, y, nulls = nd, verbose = FALSE)
    expect_true(!is.null(result$p_null))
    expect_equal(result$null_method, "moran")
  })

  it("errors for mismatched pre-computed nulls", {
    set.seed(42)
    nd <- new_null_distribution(
      matrix(rnorm(50), nrow = 5, ncol = 10),
      "moran", rnorm(5)
    )
    expect_error(
      compare_maps(1:10, 1:10, nulls = nd, verbose = FALSE),
      "rows"
    )
  })
})

describe("compute_null_correlations", {
  it("computes correlation between each null column and y", {
    null_mat <- matrix(rnorm(30), nrow = 10, ncol = 3)
    y <- rnorm(10)
    result <- compute_null_correlations(null_mat, y, "pearson")
    expect_length(result, 3)
    expect_true(all(abs(result) <= 1))
  })
})

describe("print.neuromaps_enhanced_comparison", {
  it("prints without null model info when no nulls used", {
    result <- structure(
      list(r = 0.5, p = 0.001, p_null = NULL, method = "pearson", n = 100,
           null_method = NULL, null_r = NULL, n_perm = NULL),
      class = c("neuromaps_enhanced_comparison", "neuromaps_comparison")
    )
    expect_snapshot(res <- print(result))
    expect_identical(res, result)
  })

  it("prints null model info when nulls used", {
    result <- structure(
      list(r = 0.5, p = 0.001, p_null = 0.05, method = "pearson", n = 100,
           null_method = "burt2020", null_r = rnorm(100), n_perm = 100L),
      class = c("neuromaps_enhanced_comparison", "neuromaps_comparison")
    )
    expect_snapshot(print(result))
  })
})
