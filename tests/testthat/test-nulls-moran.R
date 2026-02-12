describe("compute_weight_matrix", {
  it("produces row-standardized matrix", {
    dm <- as.matrix(dist(matrix(rnorm(30), ncol = 3)))
    w <- compute_weight_matrix(dm)
    rs <- rowSums(w)
    expect_equal(unname(rs), rep(1, 10), tolerance = 1e-10)
    expect_true(all(diag(w) == 0))
  })

  it("supports gaussian kernel", {
    dm <- as.matrix(dist(matrix(rnorm(30), ncol = 3)))
    w <- compute_weight_matrix(dm, kernel = "gaussian")
    expect_equal(unname(rowSums(w)), rep(1, 10), tolerance = 1e-10)
  })

  it("supports bisquare kernel", {
    dm <- as.matrix(dist(matrix(rnorm(30), ncol = 3)))
    w <- compute_weight_matrix(dm, kernel = "bisquare")
    expect_true(all(diag(w) == 0))
  })
})

describe("compute_mem", {
  it("returns eigenvectors and eigenvalues", {
    dm <- as.matrix(dist(matrix(rnorm(30), ncol = 3)))
    w <- compute_weight_matrix(dm)
    mem <- compute_mem(w, 10)
    expect_true(is.matrix(mem$vectors))
    expect_true(is.numeric(mem$values))
    expect_equal(nrow(mem$vectors), 10)
  })

  it("eigenvectors are orthogonal", {
    dm <- as.matrix(dist(matrix(rnorm(30), ncol = 3)))
    w <- compute_weight_matrix(dm)
    mem <- compute_mem(w, 10)
    cross <- crossprod(mem$vectors)
    expect_equal(cross, diag(ncol(mem$vectors)), tolerance = 1e-8)
  })
})

describe("make_pairs", {
  it("pairs eigenvalues within tolerance", {
    eigvals <- c(1.0, 1.0, 0.5, 0.2)
    pairs <- make_pairs(eigvals, tol = 1e-6)
    expect_equal(pairs[[1]], c(1, 2))
    expect_equal(pairs[[2]], 3)
    expect_equal(pairs[[3]], 4)
  })

  it("leaves singletons unpaired", {
    eigvals <- c(3, 2, 1)
    pairs <- make_pairs(eigvals)
    expect_length(pairs, 3)
    expect_equal(pairs[[1]], 1)
    expect_equal(pairs[[2]], 2)
    expect_equal(pairs[[3]], 3)
  })
})

describe("null_moran", {
  it("generates null distribution with singleton procedure", {
    set.seed(42)
    n <- 20
    data <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    result <- null_moran(
      data, distmat,
      n_perm = 5L, seed = 1, procedure = "singleton"
    )

    expect_s3_class(result, "null_distribution")
    expect_equal(result$method, "moran")
    expect_equal(result$n_perm, 5)
    expect_equal(result$n, 20)
    expect_equal(result$params$procedure, "singleton")
  })

  it("generates null distribution with pair procedure", {
    set.seed(42)
    n <- 20
    data <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    result <- null_moran(
      data, distmat,
      n_perm = 5L, seed = 1, procedure = "pair"
    )

    expect_s3_class(result, "null_distribution")
    expect_equal(result$params$procedure, "pair")
  })

  it("is reproducible with seed", {
    set.seed(42)
    n <- 15
    data <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    r1 <- null_moran(data, distmat, n_perm = 5L, seed = 123)
    r2 <- null_moran(data, distmat, n_perm = 5L, seed = 123)
    expect_identical(r1$nulls, r2$nulls)
  })

  it("preserves mean of data", {
    set.seed(42)
    n <- 20
    data <- rnorm(n, mean = 5)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    result <- null_moran(data, distmat, n_perm = 10L, seed = 1)
    null_means <- colMeans(result$nulls)
    expect_equal(null_means, rep(mean(data), 10), tolerance = 1e-8)
  })

  it("errors for dimension mismatch", {
    expect_error(
      null_moran(1:5, diag(3)),
      "5 x 5"
    )
  })
})
