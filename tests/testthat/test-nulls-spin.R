describe("random_rotation_matrix", {
  it("returns a 3x3 orthogonal matrix", {
    R <- random_rotation_matrix()
    expect_equal(dim(R), c(3, 3))
    expect_equal(R %*% t(R), diag(3), tolerance = 1e-10)
  })

  it("has determinant of 1", {
    R <- random_rotation_matrix()
    expect_equal(det(R), 1, tolerance = 1e-10)
  })
})

describe("rotate_coords", {
  it("returns arrays of correct dimensions", {
    lh <- matrix(rnorm(15), ncol = 3)
    rh <- matrix(rnorm(12), ncol = 3)
    result <- rotate_coords(lh, rh, n_perm = 3, seed = 42)

    expect_equal(dim(result$lh), c(5, 3, 3))
    expect_equal(dim(result$rh), c(4, 3, 3))
  })

  it("is reproducible with seed", {
    lh <- matrix(rnorm(15), ncol = 3)
    rh <- matrix(rnorm(12), ncol = 3)
    r1 <- rotate_coords(lh, rh, n_perm = 3, seed = 42)
    r2 <- rotate_coords(lh, rh, n_perm = 3, seed = 42)
    expect_identical(r1$lh, r2$lh)
    expect_identical(r1$rh, r2$rh)
  })
})

describe("compute_cost_matrix", {
  it("computes squared distances between original and rotated", {
    original <- matrix(c(1, 0, 0, 0, 1, 0), nrow = 2, ncol = 3, byrow = TRUE)
    rotated <- original
    cost <- compute_cost_matrix(original, rotated)
    expect_equal(diag(cost), c(0, 0))
    expect_true(cost[1, 2] > 0)
  })
})

describe("assign_parcels_vasa", {
  it("returns a valid permutation", {
    cost <- matrix(c(1, 10, 10, 1), nrow = 2)
    assignment <- assign_parcels_vasa(cost)
    expect_length(assignment, 2)
    expect_equal(sort(assignment), 1:2)
  })

  it("assigns to nearest available", {
    cost <- matrix(c(1, 2, 3, 4), nrow = 2)
    assignment <- assign_parcels_vasa(cost)
    expect_equal(assignment[1], 1)
    expect_equal(assignment[2], 2)
  })
})

describe("assign_parcels_hungarian", {
  it("returns optimal assignment", {
    skip_if_not_installed("clue")
    cost <- matrix(c(1, 10, 10, 1), nrow = 2)
    assignment <- assign_parcels_hungarian(cost)
    expect_equal(assignment, c(1L, 2L))
  })
})

describe("null_spin_vasa", {
  it("generates null distribution of correct dimensions", {
    set.seed(42)
    n_lh <- 5
    n_rh <- 5
    coords <- list(
      lh = matrix(rnorm(n_lh * 3), ncol = 3),
      rh = matrix(rnorm(n_rh * 3), ncol = 3)
    )
    data <- rnorm(n_lh + n_rh)
    result <- null_spin_vasa(data, coords, n_perm = 3L, seed = 1)

    expect_s3_class(result, "null_distribution")
    expect_equal(result$n, 10)
    expect_equal(result$n_perm, 3)
    expect_equal(result$method, "spin_vasa")
  })

  it("errors when data length mismatches coords", {
    coords <- list(
      lh = matrix(rnorm(9), ncol = 3),
      rh = matrix(rnorm(9), ncol = 3)
    )
    expect_error(null_spin_vasa(1:5, coords), "total parcels")
  })

  it("null values are permutations of original data", {
    set.seed(42)
    coords <- list(
      lh = matrix(rnorm(15), ncol = 3),
      rh = matrix(rnorm(15), ncol = 3)
    )
    data <- c(1:10) * 1.0
    result <- null_spin_vasa(data, coords, n_perm = 5L, seed = 1)

    for (i in seq_len(5)) {
      lh_vals <- sort(result$nulls[1:5, i])
      rh_vals <- sort(result$nulls[6:10, i])
      expect_equal(lh_vals, sort(data[1:5]))
      expect_equal(rh_vals, sort(data[6:10]))
    }
  })
})

describe("null_spin_hungarian", {
  it("generates null distribution", {
    skip_if_not_installed("clue")
    set.seed(42)
    n_lh <- 5
    n_rh <- 5
    coords <- list(
      lh = matrix(rnorm(n_lh * 3), ncol = 3),
      rh = matrix(rnorm(n_rh * 3), ncol = 3)
    )
    data <- rnorm(n_lh + n_rh)
    result <- null_spin_hungarian(data, coords, n_perm = 3L, seed = 1)

    expect_s3_class(result, "null_distribution")
    expect_equal(result$method, "spin_hungarian")
    expect_equal(result$n_perm, 3)
  })

  it("is reproducible with seed", {
    skip_if_not_installed("clue")
    set.seed(42)
    coords <- list(
      lh = matrix(rnorm(15), ncol = 3),
      rh = matrix(rnorm(15), ncol = 3)
    )
    data <- rnorm(10)
    r1 <- null_spin_hungarian(data, coords, n_perm = 3L, seed = 1)
    r2 <- null_spin_hungarian(data, coords, n_perm = 3L, seed = 1)
    expect_identical(r1$nulls, r2$nulls)
  })
})
