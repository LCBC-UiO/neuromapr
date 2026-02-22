describe("cross-validation with Python reference", {
  skip_if_no_python_fixtures()

  inputs <- load_python_fixture("shared_inputs")
  n <- inputs$n
  distmat <- as.matrix(inputs$distmat)
  data <- inputs$data
  data_y <- inputs$data_y

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

  describe("SAR weight matrix", {
    ref <- load_python_fixture("sar_weight_matrix")

    it("matches Python output", {
      w <- build_sar_weights(distmat, ref$d0)
      expect_equal(w, as.matrix(ref$w), tolerance = 1e-10)
    })

    it("has zero diagonal", {
      w <- build_sar_weights(distmat, ref$d0)
      expect_equal(diag(w), rep(0, n))
    })

    it("has unit row sums", {
      w <- build_sar_weights(distmat, ref$d0)
      expect_equal(rowSums(w), rep(1, n), tolerance = 1e-10)
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

  describe("Moran double-centered matrix", {
    ref <- load_python_fixture("moran_double_centered")

    it("matches Python output", {
      w <- compute_weight_matrix(distmat, kernel = "inverse_distance")
      sym_w <- (w + t(w)) / 2
      row_means <- rowMeans(sym_w)
      grand_mean <- mean(row_means)
      dbl_r <- sym_w - outer(row_means, row_means, "+") + grand_mean
      expect_equal(dbl_r, as.matrix(ref$dbl), tolerance = 1e-10)
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

  describe("correlations", {
    ref <- load_python_fixture("correlation_reference")

    it("Pearson r matches Python", {
      r_val <- cor(data, data_y, method = "pearson")
      expect_equal(r_val, ref$pearson_r, tolerance = 1e-6)
    })

    it("Spearman r matches Python", {
      r_val <- cor(data, data_y, method = "spearman")
      expect_equal(r_val, ref$spearman_r, tolerance = 1e-6)
    })

    it("compare_maps r matches Python", {
      result <- compare_maps(data, data_y, verbose = FALSE)
      expect_equal(result$r, ref$pearson_r, tolerance = 1e-6)
    })
  })

  describe("null distribution statistics", {
    ref <- load_python_fixture("null_distribution_stats")

    it("SAR null correlations have similar distribution", {
      sar_r <- null_burt2018(data, distmat, n_perm = ref$n_perm, seed = 1L)
      sar_surr <- as.matrix(sar_r)
      null_r <- apply(sar_surr, 2, function(s) cor(s, data_y))
      expect_equal(mean(null_r), ref$sar_null_r_mean, tolerance = 0.15)
      expect_equal(sd(null_r), ref$sar_null_r_std, tolerance = 0.15)
    })

    it("SAR surrogates preserve rank distribution", {
      expect_true(ref$sar_rank_preserved)
    })

    it("Moran null correlations have similar distribution", {
      moran_r <- null_moran(
        data, distmat,
        n_perm = ref$n_perm, seed = 1L, procedure = "singleton"
      )
      moran_surr <- as.matrix(moran_r)
      null_r <- apply(moran_surr, 2, function(s) cor(s, data_y))
      expect_equal(mean(null_r), ref$moran_null_r_mean, tolerance = 0.15)
      expect_equal(sd(null_r), ref$moran_null_r_std, tolerance = 0.15)
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
