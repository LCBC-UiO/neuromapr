describe("neuromaps_cache_dir", {
  it("uses option when set", {
    withr::local_options(neuromapr.data_dir = "/tmp/custom-cache")
    expect_equal(neuromaps_cache_dir(), "/tmp/custom-cache")
  })

  it("uses env var when option is not set", {
    withr::local_options(neuromapr.data_dir = NULL)
    withr::local_envvar(NEUROMAPR_DATA_DIR = "/tmp/env-cache")
    expect_equal(neuromaps_cache_dir(), "/tmp/env-cache")
  })

  it("falls back to R_user_dir", {
    withr::local_options(neuromapr.data_dir = NULL)
    withr::local_envvar(NEUROMAPR_DATA_DIR = "")
    result <- neuromaps_cache_dir()
    expect_true(grepl("neuromapr", result))
  })
})


describe("validate_checksum", {
  it("returns TRUE for matching checksum", {
    tmp <- withr::local_tempfile()
    writeLines("test content", tmp)
    md5 <- unname(tools::md5sum(tmp))
    expect_true(validate_checksum(tmp, md5))
  })

  it("returns FALSE for mismatched checksum", {
    tmp <- withr::local_tempfile()
    writeLines("test content", tmp)
    expect_false(validate_checksum(tmp, "0000000000000000"))
  })

  it("returns TRUE when expected is NULL", {
    tmp <- withr::local_tempfile()
    writeLines("test content", tmp)
    expect_true(validate_checksum(tmp, NULL))
  })

  it("returns TRUE when expected is empty string", {
    tmp <- withr::local_tempfile()
    writeLines("test content", tmp)
    expect_true(validate_checksum(tmp, ""))
  })
})


describe("parse_neuromaps_filename", {
  it("parses surface GIFTI filename", {
    fname <- paste0(
      "source-abagen_desc-genepc1_space-fsaverage",
      "_den-10k_hemi-L_feature.func.gii"
    )
    result <- parse_neuromaps_filename(fname)
    expect_equal(result$source, "abagen")
    expect_equal(result$desc, "genepc1")
    expect_equal(result$space, "fsaverage")
    expect_equal(result$den, "10k")
    expect_equal(result$hemi, "L")
    expect_equal(result$ext, ".func.gii")
  })

  it("parses volume NIfTI filename", {
    result <- parse_neuromaps_filename(
      "source-aghourian2017_desc-feobv_space-MNI152_res-1mm_feature.nii.gz"
    )
    expect_equal(result$source, "aghourian2017")
    expect_equal(result$desc, "feobv")
    expect_equal(result$space, "MNI152")
    expect_equal(result$res, "1mm")
    expect_null(result$hemi)
    expect_equal(result$ext, ".nii.gz")
  })
})
