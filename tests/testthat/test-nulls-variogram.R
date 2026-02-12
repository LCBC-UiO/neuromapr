describe("compute_variogram", {
  it("returns data frame with distance and gamma columns", {
    set.seed(42)
    n <- 30
    data <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    vg <- compute_variogram(data, distmat, nh = 10, pv = 50)

    expect_s3_class(vg, "data.frame")
    expect_named(vg, c("distance", "gamma"))
    expect_true(all(vg$gamma >= 0))
    expect_true(all(vg$distance > 0))
  })

  it("subsamples when ns < n", {
    set.seed(42)
    n <- 50
    data <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    vg <- compute_variogram(data, distmat, nh = 10, pv = 50, ns = 20)
    expect_s3_class(vg, "data.frame")
  })
})

describe("smooth_surrogate", {
  it("smooths permuted values toward neighbors", {
    set.seed(42)
    n <- 20
    coords <- matrix(rnorm(n * 3), ncol = 3)
    distmat <- as.matrix(dist(coords))
    nn <- compute_knn(distmat, 5)
    permuted <- rnorm(n)
    smoothed <- smooth_surrogate(permuted, nn$indices, nn$distances, k = 5, kernel = "uniform")

    expect_length(smoothed, n)
    expect_true(stats::sd(smoothed) < stats::sd(permuted))
  })

  it("supports exponential kernel", {
    set.seed(42)
    n <- 20
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    nn <- compute_knn(distmat, 5)
    smoothed <- smooth_surrogate(rnorm(n), nn$indices, nn$distances, k = 3, kernel = "exponential")
    expect_length(smoothed, n)
  })

  it("supports gaussian kernel", {
    set.seed(42)
    n <- 20
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    nn <- compute_knn(distmat, 5)
    smoothed <- smooth_surrogate(rnorm(n), nn$indices, nn$distances, k = 3, kernel = "gaussian")
    expect_length(smoothed, n)
  })
})

describe("rank_match", {
  it("preserves the rank distribution of target", {
    target <- c(10, 20, 30, 40, 50)
    surrogate <- c(5, 3, 1, 2, 4)
    matched <- rank_match(surrogate, target)
    expect_equal(sort(matched), sort(target))
  })
})

describe("null_burt2020", {
  it("generates null distribution of correct dimensions", {
    set.seed(42)
    n <- 25
    data <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    result <- null_burt2020(
      data, distmat,
      n_perm = 3L, seed = 1,
      ns = 25, nh = 5, knn = 10,
      deltas = c(0.3, 0.6)
    )

    expect_s3_class(result, "null_distribution")
    expect_equal(result$method, "burt2020")
    expect_equal(result$n_perm, 3)
    expect_equal(result$n, 25)
  })

  it("surrogates have same values as original (rank-matched)", {
    set.seed(42)
    n <- 20
    data <- sort(rnorm(n))
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    result <- null_burt2020(
      data, distmat,
      n_perm = 2L, seed = 1,
      ns = 20, nh = 5, knn = 10,
      deltas = c(0.5)
    )

    for (i in seq_len(2)) {
      expect_equal(sort(result$nulls[, i]), sort(data))
    }
  })

  it("is reproducible with seed", {
    set.seed(42)
    n <- 20
    data <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    r1 <- null_burt2020(data, distmat, n_perm = 2L, seed = 99, ns = 20, nh = 5, knn = 10, deltas = c(0.5))
    r2 <- null_burt2020(data, distmat, n_perm = 2L, seed = 99, ns = 20, nh = 5, knn = 10, deltas = c(0.5))
    expect_identical(r1$nulls, r2$nulls)
  })

  it("errors for dimension mismatch", {
    expect_error(
      null_burt2020(1:5, diag(3)),
      "5 x 5"
    )
  })
})
