describe("generate_nulls", {
  it("errors for non-numeric data", {
    expect_error(
      generate_nulls("abc", method = "burt2020"),
      "numeric"
    )
  })

  it("errors for NA in data", {
    expect_error(
      generate_nulls(c(1, NA, 3), method = "burt2020"),
      "NA"
    )
  })

  it("errors for n_perm < 1", {
    expect_error(
      generate_nulls(1:5, method = "burt2020", n_perm = 0, distmat = diag(5)),
      "n_perm"
    )
  })

  it("errors when distmat missing for burt2020", {
    expect_error(
      generate_nulls(1:5, method = "burt2020"),
      "distmat"
    )
  })

  it("errors when distmat missing for moran", {
    expect_error(
      generate_nulls(1:5, method = "moran"),
      "distmat"
    )
  })

  it("errors when coords missing for spin_vasa", {
    expect_error(
      generate_nulls(1:5, method = "spin_vasa"),
      "coords"
    )
  })

  it("errors when coords missing for spin_hungarian", {
    expect_error(
      generate_nulls(1:5, method = "spin_hungarian"),
      "coords"
    )
  })

  it("dispatches to moran", {
    set.seed(42)
    n <- 20
    data <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    result <- generate_nulls(
      data, method = "moran",
      n_perm = 5L, distmat = distmat, seed = 1
    )
    expect_s3_class(result, "null_distribution")
    expect_equal(result$method, "moran")
    expect_equal(result$n_perm, 5)
  })

  it("dispatches to spin_vasa", {
    set.seed(42)
    n_lh <- 10
    n_rh <- 10
    coords <- list(
      lh = matrix(rnorm(n_lh * 3), ncol = 3),
      rh = matrix(rnorm(n_rh * 3), ncol = 3)
    )
    data <- rnorm(n_lh + n_rh)
    result <- generate_nulls(
      data, method = "spin_vasa",
      n_perm = 5L, coords = coords, seed = 1
    )
    expect_s3_class(result, "null_distribution")
    expect_equal(result$method, "spin_vasa")
  })

  it("errors when coords missing for alexander_bloch", {
    expect_error(
      generate_nulls(1:5, method = "alexander_bloch"),
      "coords"
    )
  })

  it("errors when coords missing for baum", {
    expect_error(
      generate_nulls(1:2, method = "baum"),
      "coords"
    )
  })

  it("errors when parcellation missing for baum", {
    coords <- list(
      lh = matrix(rnorm(15), ncol = 3),
      rh = matrix(rnorm(15), ncol = 3)
    )
    expect_error(
      generate_nulls(1:2, method = "baum", coords = coords),
      "parcellation"
    )
  })

  it("errors when coords missing for cornblath", {
    expect_error(
      generate_nulls(1:2, method = "cornblath"),
      "coords"
    )
  })

  it("errors when parcellation missing for cornblath", {
    coords <- list(
      lh = matrix(rnorm(15), ncol = 3),
      rh = matrix(rnorm(15), ncol = 3)
    )
    expect_error(
      generate_nulls(1:2, method = "cornblath", coords = coords),
      "parcellation"
    )
  })

  it("errors when distmat missing for burt2018", {
    expect_error(
      generate_nulls(1:5, method = "burt2018"),
      "distmat"
    )
  })

  it("dispatches to alexander_bloch", {
    set.seed(42)
    n_lh <- 10
    n_rh <- 10
    coords <- list(
      lh = matrix(rnorm(n_lh * 3), ncol = 3),
      rh = matrix(rnorm(n_rh * 3), ncol = 3)
    )
    data <- rnorm(n_lh + n_rh)
    result <- generate_nulls(
      data, method = "alexander_bloch",
      n_perm = 5L, coords = coords, seed = 1
    )
    expect_s3_class(result, "null_distribution")
    expect_equal(result$method, "alexander_bloch")
  })

  it("dispatches to burt2018", {
    set.seed(42)
    n <- 20
    data <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    result <- generate_nulls(
      data, method = "burt2018",
      n_perm = 5L, distmat = distmat, seed = 1
    )
    expect_s3_class(result, "null_distribution")
    expect_equal(result$method, "burt2018")
  })
})

describe("compute_distance_matrix", {
  it("computes from a coordinate matrix", {
    coords <- matrix(c(0, 1, 0, 0, 0, 1), nrow = 2, ncol = 3)
    dm <- compute_distance_matrix(coords)
    expect_equal(nrow(dm), 2)
    expect_equal(ncol(dm), 2)
    expect_equal(dm[1, 1], 0)
    expect_true(dm[1, 2] > 0)
  })

  it("computes from a list with lh/rh", {
    coords <- list(
      lh = matrix(c(0, 1, 0, 0, 0, 0), nrow = 2, ncol = 3),
      rh = matrix(c(2, 3, 0, 0, 0, 0), nrow = 2, ncol = 3)
    )
    dm <- compute_distance_matrix(coords)
    expect_equal(nrow(dm), 4)
  })

  it("errors for non-3-column matrix", {
    expect_error(
      compute_distance_matrix(matrix(1:4, ncol = 2)),
      "3 columns"
    )
  })
})
