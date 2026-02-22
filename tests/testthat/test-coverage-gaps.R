describe("compute_mem RSpectra branch (coverage)", {
  it("uses RSpectra for large matrices when available", {
    skip_if_not_installed("RSpectra")
    n <- 5001
    w <- matrix(0, n, n)
    idx <- cbind(seq_len(n - 1), 2:n)
    w[idx] <- 1
    w <- w + t(w)
    rs <- rowSums(w)
    rs[rs == 0] <- 1
    w <- w / rs

    mem <- compute_mem(w, n)
    expect_true(is.matrix(mem$vectors))
    expect_true(is.numeric(mem$values))
    expect_true(length(mem$values) <= 500)
  })
})

describe("null_moran pair rotation (coverage)", {
  it("exercises pair rotation with degenerate eigenvalues", {
    n <- 6
    angles <- seq(0, 2 * pi, length.out = n + 1)[seq_len(n)]
    coords <- cbind(cos(angles), sin(angles), 0)
    distmat <- as.matrix(dist(coords))
    data <- rnorm(n)

    w <- compute_weight_matrix(distmat)
    mem <- compute_mem(w, n)
    pairs <- make_pairs(mem$values, tol = 1e-6)
    has_pair <- any(vapply(pairs, length, integer(1)) == 2)
    expect_true(has_pair)

    result <- null_moran(
      data, distmat,
      n_perm = 5L, seed = 1, procedure = "pair"
    )
    expect_s3_class(result, "null_distribution")
    expect_equal(result$n_perm, 5)
  })
})

describe("null_spin_hungarian data mismatch (coverage)", {
  it("errors when data length mismatches coords", {
    coords <- list(
      lh = matrix(rnorm(15), ncol = 3),
      rh = matrix(rnorm(15), ncol = 3)
    )
    expect_error(
      null_spin_hungarian(1:5, coords, n_perm = 3L),
      "total parcels"
    )
  })
})

describe("rodrigues rotation (coverage)", {
  it("produces valid rotation matrices", {
    set.seed(42)
    n_lh <- 5
    n_rh <- 5
    coords <- list(
      lh = matrix(rnorm(n_lh * 3), ncol = 3),
      rh = matrix(rnorm(n_rh * 3), ncol = 3)
    )
    data <- rnorm(n_lh + n_rh)

    result <- null_spin_vasa(
      data, coords,
      n_perm = 3L, seed = 1,
      rotation = "rodrigues"
    )
    expect_s3_class(result, "null_distribution")
    expect_equal(result$n_perm, 3)
  })
})

describe("null_burt2020 via generate_nulls with fixed_idx (coverage)", {
  it("exercises fixed_idx path when resample=FALSE", {
    set.seed(42)
    n <- 30
    data <- rnorm(n)
    distmat <- as.matrix(dist(matrix(rnorm(n * 3), ncol = 3)))
    result <- null_burt2020(
      data, distmat,
      n_perm = 3L, seed = 1, ns = 15L, resample = FALSE
    )
    expect_s3_class(result, "null_distribution")
  })
})

describe("neuromaps-registry refresh and cache (coverage)", {
  it("build_neuromaps_registry with refresh=TRUE clears cache", {
    the$registry <- tibble::tibble(source = "stale")
    the$osf_json <- "stale"
    the$meta_json <- "stale"

    mock_fname <- paste0(
      "source-t_desc-m_space-fs",
      "_den-10k_hemi-L_feature.func.gii"
    )

    local_mocked_bindings(
      fetch_neuromaps_osf_json = function() {
        list(annotations = list(
          list(
            source = "t", desc = "m",
            space = "fs", den = "10k", res = NULL,
            hemi = "L", format = "surface",
            fname = mock_fname,
            rel_path = "t/m/fs/",
            checksum = "aaa", tags = list("pet"),
            url = list("4mw3a", "f1")
          )
        ))
      },
      fetch_neuromaps_meta_json = function() {
        list(annotations = list(
          list(
            annot = list(
              source = "t", desc = "m",
              space = "fs", den = "10k"
            ),
            full_desc = "Test",
            demographics = list(N = 10, age = "30")
          )
        ))
      }
    )

    result <- build_neuromaps_registry(refresh = TRUE)
    expect_equal(result$source, "t")

    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL
  })

  it("clear_neuromaps_cache resets all cached data", {
    the$osf_json <- "data"
    the$meta_json <- "data"
    the$registry <- tibble::tibble(source = "cached")

    result <- clear_neuromaps_cache()
    expect_null(the$osf_json)
    expect_null(the$meta_json)
    expect_null(the$registry)
    expect_null(result)
  })
})

describe("read_surface_coordinates (coverage)", {
  it("errors for unsupported format", {
    expect_error(
      read_surface_coordinates("file.obj"),
      "Unsupported"
    )
  })
})

describe("validate_distmat (coverage)", {
  it("errors for non-matrix input", {
    expect_error(
      validate_distmat(1:5, 5),
      "matrix"
    )
  })
})

describe("read_brain_map_values branches (coverage)", {
  it("errors for .surf.gii files", {
    expect_error(
      read_brain_map_values("test.surf.gii"),
      "surface geometry"
    )
  })

  it("errors for unsupported format", {
    expect_error(
      read_brain_map_values("test.xyz"),
      "Unsupported"
    )
  })
})

describe("validate_coords (coverage)", {
  it("errors for non-list input", {
    expect_error(validate_coords("not a list"), "list")
  })

  it("errors for bad lh matrix", {
    coords <- list(
      lh = matrix(1:4, ncol = 2),
      rh = matrix(1:6, ncol = 3)
    )
    expect_error(validate_coords(coords), "3 columns")
  })

  it("errors for bad rh matrix", {
    coords <- list(
      lh = matrix(1:6, ncol = 3),
      rh = matrix(1:4, ncol = 2)
    )
    expect_error(validate_coords(coords), "3 columns")
  })
})

describe("density_to_n unknown density (coverage)", {
  it("errors for unknown density string", {
    expect_error(density_to_n("99k"), "Unknown density")
  })
})

describe("parcellate with file-like args (coverage)", {
  it("parcellate reads brain map values from path", {
    local_mocked_bindings(
      read_brain_map_values = function(path) c(1, 2, 3, 4)
    )
    local_mocked_bindings(
      read_parcellation_labels = function(path) c(1L, 1L, 2L, 2L)
    )

    result <- parcellate("fake_data.func.gii", "fake_parc.label.gii")
    expect_equal(unname(result), c(1.5, 3.5))
  })

  it("unparcellate reads parcellation from path", {
    local_mocked_bindings(
      read_parcellation_labels = function(path) c(1L, 1L, 2L, 2L)
    )

    parcel_data <- c("1" = 10, "2" = 20)
    result <- unparcellate(parcel_data, "fake_parc.label.gii")
    expect_equal(result, c(10, 10, 20, 20))
  })
})

describe("get_parcel_centroids surface and geodesic (coverage)", {
  it("computes geodesic centroids", {
    vertices <- matrix(c(
      0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      1, 1, 0
    ), ncol = 3, byrow = TRUE)
    faces <- matrix(c(1L, 2L, 3L, 2L, 3L, 4L), ncol = 3, byrow = TRUE)
    labels <- c(1, 1, 1, 1)

    centroids <- get_parcel_centroids(
      vertices, labels,
      method = "geodesic", faces = faces
    )
    expect_equal(nrow(centroids), 1)
    expect_equal(ncol(centroids), 3)
  })

  it("errors for invalid vertices", {
    expect_error(
      get_parcel_centroids(matrix(1:4, ncol = 2), c(1, 1)),
      "3 columns"
    )
  })
})

describe("resample_images strategies (coverage)", {
  it("errors for downsample_only with different spaces", {
    src <- withr::local_tempfile(fileext = ".gii")
    trg <- withr::local_tempfile(fileext = ".gii")
    file.create(src)
    file.create(trg)
    expect_error(
      resample_images(
        src, trg,
        src_space = "fsaverage",
        trg_space = "fsLR",
        strategy = "downsample_only"
      ),
      "same space"
    )
  })

  it("errors for missing target file", {
    src <- withr::local_tempfile(fileext = ".gii")
    file.create(src)
    expect_error(
      resample_images(
        src, "nonexistent.gii",
        src_space = "fsaverage",
        trg_space = "fsaverage"
      ),
      "not found"
    )
  })

  it("downsample_only returns same files when equal density", {
    src <- withr::local_tempfile(fileext = ".gii")
    trg <- withr::local_tempfile(fileext = ".gii")
    file.create(src)
    file.create(trg)

    local_mocked_bindings(
      get_gifti_density = function(path) "10k"
    )

    result <- resample_images(
      src, trg,
      src_space = "fsaverage",
      trg_space = "fsaverage",
      strategy = "downsample_only"
    )
    expect_equal(result$src, src)
    expect_equal(result$trg, trg)
  })

  it("downsample_only downsamples source when higher", {
    src <- withr::local_tempfile(fileext = ".gii")
    trg <- withr::local_tempfile(fileext = ".gii")
    file.create(src)
    file.create(trg)

    density_call <- 0L
    local_mocked_bindings(
      get_gifti_density = function(path) {
        density_call <<- density_call + 1L
        if (density_call == 1L) "32k" else "10k"
      },
      transform_to_space = function(paths, ...) "resampled.gii"
    )

    result <- resample_images(
      src, trg,
      src_space = "fsaverage",
      trg_space = "fsaverage",
      strategy = "downsample_only"
    )
    expect_equal(result$src, "resampled.gii")
    expect_equal(result$trg, trg)
  })

  it("downsample_only downsamples target when higher", {
    src <- withr::local_tempfile(fileext = ".gii")
    trg <- withr::local_tempfile(fileext = ".gii")
    file.create(src)
    file.create(trg)

    density_call <- 0L
    local_mocked_bindings(
      get_gifti_density = function(path) {
        density_call <<- density_call + 1L
        if (density_call == 1L) "10k" else "32k"
      },
      transform_to_space = function(paths, ...) "resampled.gii"
    )

    result <- resample_images(
      src, trg,
      src_space = "fsaverage",
      trg_space = "fsaverage",
      strategy = "downsample_only"
    )
    expect_equal(result$src, src)
    expect_equal(result$trg, "resampled.gii")
  })

  it("transform_to_src transforms target to source space", {
    src <- withr::local_tempfile(fileext = ".gii")
    trg <- withr::local_tempfile(fileext = ".gii")
    file.create(src)
    file.create(trg)

    local_mocked_bindings(
      get_gifti_density = function(path) "10k",
      transform_to_space = function(...) "resampled.gii"
    )

    result <- resample_images(
      src, trg,
      src_space = "fsaverage",
      trg_space = "fsLR",
      strategy = "transform_to_src"
    )
    expect_equal(result$src, src)
    expect_equal(result$trg, "resampled.gii")
  })

  it("transform_to_trg transforms source to target space", {
    src <- withr::local_tempfile(fileext = ".gii")
    trg <- withr::local_tempfile(fileext = ".gii")
    file.create(src)
    file.create(trg)

    local_mocked_bindings(
      get_gifti_density = function(path) "10k",
      transform_to_space = function(...) "resampled.gii"
    )

    result <- resample_images(
      src, trg,
      src_space = "fsaverage",
      trg_space = "fsLR",
      strategy = "transform_to_trg"
    )
    expect_equal(result$src, "resampled.gii")
    expect_equal(result$trg, trg)
  })

  it("transform_to_alt transforms both to alt space", {
    src <- withr::local_tempfile(fileext = ".gii")
    trg <- withr::local_tempfile(fileext = ".gii")
    file.create(src)
    file.create(trg)

    local_mocked_bindings(
      get_gifti_density = function(path) "10k",
      transform_to_space = function(...) "resampled.gii"
    )

    result <- resample_images(
      src, trg,
      src_space = "fsaverage",
      trg_space = "fsLR",
      strategy = "transform_to_alt",
      alt_space = "fsaverage",
      alt_density = "32k"
    )
    expect_equal(result$src, "resampled.gii")
    expect_equal(result$trg, "resampled.gii")
  })
})

describe("transform_to_space full path (coverage)", {
  it("transforms a file via ciftiTools mock", {
    skip_if_not_installed("ciftiTools")

    tmp <- withr::local_tempfile(fileext = ".func.gii")
    file.create(tmp)

    local_mocked_bindings(
      check_wb_command = function(...) "/mock/wb_command"
    )
    local_mocked_bindings(
      ciftiTools.setOption = function(...) NULL,
      resample_gifti = function(...) NULL,
      .package = "ciftiTools"
    )

    result <- transform_to_space(
      tmp,
      target_space = "fsLR",
      target_density = "32k",
      method = "barycentric",
      verbose = TRUE
    )
    expect_length(result, 1)
    expect_true(grepl("fsLR_32k", result))
  })

  it("uses adaptive method with area surfaces", {
    skip_if_not_installed("ciftiTools")

    tmp <- withr::local_tempfile(fileext = ".func.gii")
    area_cur <- withr::local_tempfile(fileext = ".surf.gii")
    area_new <- withr::local_tempfile(fileext = ".surf.gii")
    file.create(tmp)
    file.create(area_cur)
    file.create(area_new)

    captured_args <- NULL
    local_mocked_bindings(
      check_wb_command = function(...) "/mock/wb_command"
    )
    local_mocked_bindings(
      ciftiTools.setOption = function(...) NULL,
      resample_gifti = function(...) {
        captured_args <<- list(...)
        NULL
      },
      .package = "ciftiTools"
    )

    result <- transform_to_space(
      tmp,
      target_space = "fsaverage",
      target_density = "10k",
      method = "adaptive",
      area_surf_current = area_cur,
      area_surf_new = area_new,
      verbose = FALSE
    )
    expect_true(grepl("fsaverage_10k", result))
  })
})

describe("check_wb_command ciftiTools path (coverage)", {
  it("returns ciftiTools configured path", {
    skip_if_not_installed("ciftiTools")

    tmp <- withr::local_tempfile()
    file.create(tmp)

    local_mocked_bindings(
      ciftiTools.getOption = function(...) tmp,
      .package = "ciftiTools"
    )

    result <- check_wb_command()
    expect_equal(result, tmp)
  })

  it("falls back to system PATH", {
    skip_if_not_installed("ciftiTools")

    local_mocked_bindings(
      ciftiTools.getOption = function(...) NULL,
      .package = "ciftiTools"
    )

    sys_wb <- Sys.which("wb_command")
    if (nzchar(sys_wb)) {
      result <- check_wb_command()
      expect_true(nzchar(result))
    } else {
      withr::local_envvar(PATH = "")
      expect_error(check_wb_command(), "wb_command")
    }
  })
})

describe("annot_to_gifti full path (coverage)", {
  it("converts annotation file to GIFTI", {
    skip_if_not_installed("freesurferformats")

    tmp_annot <- withr::local_tempfile(fileext = ".annot")
    tmp_out <- withr::local_tempfile(fileext = ".label.gii")

    local_mocked_bindings(
      read.fs.annot = function(path) {
        list(label_codes = c(1L, 2L, 3L, 1L, 2L))
      },
      .package = "freesurferformats"
    )
    local_mocked_bindings(
      write_gifti = function(gii, path) {
        writeLines("mock", path)
      },
      .package = "gifti"
    )

    file.create(tmp_annot)
    result <- annot_to_gifti(tmp_annot, output_path = tmp_out)
    expect_equal(result, tmp_out)
    expect_true(file.exists(tmp_out))
  })

  it("auto-generates output path", {
    skip_if_not_installed("freesurferformats")

    tmp_dir <- withr::local_tempdir()
    tmp_annot <- file.path(tmp_dir, "test.annot")
    file.create(tmp_annot)

    local_mocked_bindings(
      read.fs.annot = function(path) {
        list(label_codes = c(1L, 2L))
      },
      .package = "freesurferformats"
    )
    local_mocked_bindings(
      write_gifti = function(gii, path) {
        writeLines("mock", path)
      },
      .package = "gifti"
    )

    result <- annot_to_gifti(tmp_annot)
    expect_true(grepl("\\.label\\.gii$", result))
  })
})

describe("fsmorph_to_gifti full path (coverage)", {
  it("converts morphometry file to GIFTI", {
    skip_if_not_installed("freesurferformats")

    tmp_morph <- withr::local_tempfile(fileext = ".curv")
    tmp_out <- withr::local_tempfile(fileext = ".func.gii")

    local_mocked_bindings(
      read.fs.morph = function(path) c(0.1, 0.2, 0.3),
      .package = "freesurferformats"
    )
    local_mocked_bindings(
      write_gifti = function(gii, path) {
        writeLines("mock", path)
      },
      .package = "gifti"
    )

    file.create(tmp_morph)
    result <- fsmorph_to_gifti(tmp_morph, output_path = tmp_out)
    expect_equal(result, tmp_out)
    expect_true(file.exists(tmp_out))
  })

  it("auto-generates output path", {
    skip_if_not_installed("freesurferformats")

    tmp_dir <- withr::local_tempdir()
    tmp_morph <- file.path(tmp_dir, "test.curv")
    file.create(tmp_morph)

    local_mocked_bindings(
      read.fs.morph = function(path) c(0.1, 0.2),
      .package = "freesurferformats"
    )
    local_mocked_bindings(
      write_gifti = function(gii, path) {
        writeLines("mock", path)
      },
      .package = "gifti"
    )

    result <- fsmorph_to_gifti(tmp_morph)
    expect_true(grepl("\\.func\\.gii$", result))
  })
})

describe("get_gifti_density with mock (coverage)", {
  it("detects density from vertex count", {
    local_mocked_bindings(
      read_gifti = function(path) {
        list(data = list(numeric(10242)))
      },
      .package = "gifti"
    )

    result <- get_gifti_density("fake.func.gii")
    expect_equal(result, "10k")
  })

  it("errors for unknown vertex count", {
    local_mocked_bindings(
      read_gifti = function(path) {
        list(data = list(numeric(999)))
      },
      .package = "gifti"
    )

    expect_error(
      get_gifti_density("fake.func.gii"),
      "Unknown vertex count"
    )
  })
})

describe("parse_neuromaps_filename (coverage)", {
  it("parses BIDS-style filename", {
    result <- parse_neuromaps_filename(
      "source-abagen_desc-genepc1_feature.func.gii"
    )
    expect_equal(result$source, "abagen")
    expect_equal(result$desc, "genepc1")
    expect_equal(result$ext, ".func.gii")
  })
})

describe("neuromaps_cache_dir (coverage)", {
  it("uses option when set", {
    withr::local_options(neuromapr.data_dir = "/tmp/test_cache")
    result <- neuromaps_cache_dir()
    expect_equal(result, "/tmp/test_cache")
  })

  it("uses env var when set", {
    withr::local_options(neuromapr.data_dir = NULL)
    withr::local_envvar(NEUROMAPR_DATA_DIR = "/tmp/env_cache")
    result <- neuromaps_cache_dir()
    expect_equal(result, "/tmp/env_cache")
  })

  it("falls back to R_user_dir", {
    withr::local_options(neuromapr.data_dir = NULL)
    withr::local_envvar(NEUROMAPR_DATA_DIR = "")
    result <- neuromaps_cache_dir()
    expect_true(grepl("neuromapr", result))
  })
})

describe("validate_checksum (coverage)", {
  it("returns TRUE for null expected", {
    expect_true(validate_checksum("anyfile", NULL))
  })

  it("returns TRUE for empty expected", {
    expect_true(validate_checksum("anyfile", ""))
  })
})

describe("compute_knn (coverage)", {
  it("finds k nearest neighbors", {
    dm <- as.matrix(dist(1:5))
    result <- compute_knn(dm, k = 2)
    expect_equal(nrow(result$indices), 5)
    expect_equal(ncol(result$indices), 2)
    expect_equal(result$indices[1, 1], 2L)
    expect_true(all(result$distances > 0))
  })
})

describe("read_surface_coordinates (coverage)", {
  it("reads .surf.gii via gifti", {
    local_mocked_bindings(
      read_gifti = function(path) {
        list(data = list(matrix(rnorm(12), ncol = 3)))
      },
      .package = "gifti"
    )
    result <- read_surface_coordinates("test.surf.gii")
    expect_true(is.matrix(result))
    expect_equal(ncol(result), 3)
  })

  it("reads plain .gii via gifti", {
    local_mocked_bindings(
      read_gifti = function(path) {
        list(data = list(matrix(rnorm(12), ncol = 3)))
      },
      .package = "gifti"
    )
    result <- read_surface_coordinates("test.gii")
    expect_true(is.matrix(result))
  })
})

describe("read_brain_map_values .label.gii (coverage)", {
  it("reads .label.gii files", {
    local_mocked_bindings(
      read_gifti = function(path) {
        list(data = list(c(1L, 2L, 3L)))
      },
      .package = "gifti"
    )
    result <- read_brain_map_values("test.label.gii")
    expect_equal(result, c(1, 2, 3))
  })

  it("reads generic .gii files", {
    local_mocked_bindings(
      read_gifti = function(path) {
        list(data = list(c(0.5, 1.5, 2.5)))
      },
      .package = "gifti"
    )
    result <- read_brain_map_values("test_data.gii")
    expect_equal(result, c(0.5, 1.5, 2.5))
  })
})

describe("read_brain_map_values NIfTI (coverage)", {
  it("reads .nii.gz files", {
    skip_if_not_installed("RNifti")
    local_mocked_bindings(
      readNifti = function(path) array(1:8, dim = c(2, 2, 2)),
      .package = "RNifti"
    )
    result <- read_brain_map_values("test.nii.gz")
    expect_equal(result, as.numeric(1:8))
  })

  it("reads .nii files", {
    skip_if_not_installed("RNifti")
    local_mocked_bindings(
      readNifti = function(path) array(1:4, dim = c(2, 2)),
      .package = "RNifti"
    )
    result <- read_brain_map_values("test.nii")
    expect_equal(result, as.numeric(1:4))
  })
})

describe("null_baum NA branch (coverage)", {
  it("produces NA when rotated labels are all zero", {
    n_lh <- 3
    n_rh <- 3
    coords <- list(
      lh = matrix(rnorm(n_lh * 3), ncol = 3),
      rh = matrix(rnorm(n_rh * 3), ncol = 3)
    )
    parcellation <- c(1, 0, 0, 0, 0, 2)
    data <- c("1" = 10, "2" = 20)

    local_mocked_bindings(
      rotate_coords = function(...) {
        lh_rot <- array(0, dim = c(n_lh, 3, 1))
        rh_rot <- array(0, dim = c(n_rh, 3, 1))
        list(lh = lh_rot, rh = rh_rot)
      }
    )

    result <- null_baum(
      data, coords, parcellation,
      n_perm = 1L, seed = 1
    )
    expect_s3_class(result, "null_distribution")
    expect_true(any(is.na(result$nulls)))
  })
})

describe("null_cornblath NA branch (coverage)", {
  it("produces NA when no valid nearest labels", {
    n_lh <- 5
    n_rh <- 5
    coords <- list(
      lh = matrix(rnorm(n_lh * 3), ncol = 3),
      rh = matrix(rnorm(n_rh * 3), ncol = 3)
    )
    parcellation <- c(1, 1, 1, 0, 0, 0, 0, 0, 2, 2)
    data <- c("1" = 10, "2" = 20)

    local_mocked_bindings(
      rotate_coords = function(...) {
        list(
          lh = array(0, dim = c(n_lh, 3, 1)),
          rh = array(0, dim = c(n_rh, 3, 1))
        )
      },
      nearest_valid_label = function(rotated, original, labels, valid) {
        rep(0L, length(labels))
      }
    )

    result <- null_cornblath(
      data, coords, parcellation,
      n_perm = 1L, seed = 1
    )
    expect_s3_class(result, "null_distribution")
  })
})

describe("read_parcellation_labels (coverage)", {
  it("reads labels from GIFTI file", {
    local_mocked_bindings(
      read_gifti = function(path) {
        list(data = list(c(1L, 2L, 3L, 1L)))
      },
      .package = "gifti"
    )
    result <- read_parcellation_labels("fake.label.gii")
    expect_equal(result, c(1L, 2L, 3L, 1L))
  })
})
