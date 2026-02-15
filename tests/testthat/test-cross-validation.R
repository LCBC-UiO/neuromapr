describe("cross-validation with Python reference", {
  skip_if_no_python_fixtures()

  inputs <- load_python_fixture("shared_inputs")
  n <- inputs$n
  distmat <- as.matrix(inputs$distmat)
  data <- inputs$data

  describe("weight matrix (inverse distance)", {
    ref <- load_python_fixture("weight_matrix_inverse_distance")

    it("matches Python output", {
      w <- compute_weight_matrix(distmat, kernel = "inverse_distance")
      expect_equal(w, as.matrix(ref$w), tolerance = 1e-10)
    })
  })

  describe("weight matrix (exponential)", {
    ref <- load_python_fixture("weight_matrix_exponential")

    it("matches Python output", {
      w <- compute_weight_matrix(distmat, kernel = "exponential")
      expect_equal(w, as.matrix(ref$w), tolerance = 1e-10)
    })
  })

  describe("MEM eigendecomposition", {
    ref <- load_python_fixture("mem")
    w <- compute_weight_matrix(distmat, kernel = "inverse_distance")
    mem <- compute_mem(w, n)

    it("eigenvalues match", {
      r_vals <- sort(mem$values, decreasing = TRUE)
      py_vals <- sort(ref$eigenvalues, decreasing = TRUE)
      min_len <- min(length(r_vals), length(py_vals))
      expect_equal(
        r_vals[seq_len(min_len)],
        py_vals[seq_len(min_len)],
        tolerance = 1e-8
      )
    })

    it("eigenvectors match up to sign", {
      r_vecs <- mem$vectors
      py_vecs <- as.matrix(ref$eigenvectors)
      min_cols <- min(ncol(r_vecs), ncol(py_vecs))

      r_order <- order(mem$values, decreasing = TRUE)
      py_order <- order(ref$eigenvalues, decreasing = TRUE)
      r_vecs <- r_vecs[, r_order[seq_len(min_cols)], drop = FALSE]
      py_vecs <- py_vecs[, py_order[seq_len(min_cols)], drop = FALSE]

      for (j in seq_len(min_cols)) {
        same_sign <- sum((r_vecs[, j] - py_vecs[, j])^2)
        flip_sign <- sum((r_vecs[, j] + py_vecs[, j])^2)
        expect_lt(min(same_sign, flip_sign), 1e-8)
      }
    })
  })

  describe("variogram computation", {
    ref <- load_python_fixture("variogram")

    it("matches Python output with fixed indices", {
      idx_r <- sort(ref$idx) + 1L
      vg <- compute_variogram(
        data,
        distmat,
        nh = ref$nh,
        pv = ref$pv,
        ns = ref$ns,
        idx = idx_r
      )
      expect_equal(vg$gamma, ref$gamma, tolerance = 1e-10)
      expect_equal(vg$distance, ref$bin_centers, tolerance = 1e-10)
    })
  })

  describe("ZYZ Euler rotation matrix", {
    ref <- load_python_fixture("rotation_matrix")

    it("matches Python output for fixed angles", {
      alpha <- ref$alpha
      beta <- ref$beta
      gamma <- ref$gamma
      ca <- cos(alpha)
      sa <- sin(alpha)
      cb <- cos(beta)
      sb <- sin(beta)
      cg <- cos(gamma)
      sg <- sin(gamma)

      rot <- matrix(
        c(
          # nolint: object_name
          cg * cb * ca - sg * sa,
          sg * cb * ca + cg * sa,
          -sb * ca,
          -cg * cb * sa - sg * ca,
          -sg * cb * sa + cg * ca,
          sb * sa,
          cg * sb,
          sg * sb,
          cb
        ),
        nrow = 3,
        ncol = 3
      )

      expect_equal(rot, as.matrix(ref$R), tolerance = 1e-12)
    })
  })

  describe("cost matrix", {
    ref <- load_python_fixture("cost_matrix")

    it("matches Python output", {
      original <- as.matrix(ref$original)
      rotated <- as.matrix(ref$rotated)
      cost <- compute_cost_matrix(original, rotated)
      expect_equal(cost, as.matrix(ref$cost), tolerance = 1e-10)
    })
  })

  describe("rank matching", {
    ref <- load_python_fixture("rank_match")

    it("matches Python output", {
      matched <- rank_match(ref$surrogate, ref$target)
      expect_equal(matched, ref$matched)
    })
  })

  describe("burt2020 surrogate properties", {
    it("preserves rank distribution", {
      skip_if(
        !file.exists(file.path(fixture_dir(), "burt2020_stats.json")),
        paste(
          "burt2020 stats fixture not available",
          "(brainsmash not installed in Python)"
        )
      )
      ref <- load_python_fixture("burt2020_stats")
      expect_true(ref$rank_preserved)
    })
  })
})
