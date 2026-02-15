describe("get_gifti_density", {
  it("detects density from a real GIFTI", {
    skip_on_cran()
    skip_if_offline()

    cache <- withr::local_tempdir()
    paths <- fetch_neuromaps_annotation(
      "abagen", "genepc1", "fsaverage",
      density = "10k",
      data_dir = cache,
      verbose = FALSE
    )

    expect_equal(get_gifti_density(paths[1]), "10k")
  })
})

describe("density_to_n", {
  it("maps known densities", {
    expect_equal(density_to_n("32k"), 32492L)
    expect_equal(density_to_n("164k"), 163842L)
    expect_equal(density_to_n("10k"), 10242L)
    expect_equal(density_to_n("41k"), 40962L)
  })
})

describe("resample_images", {
  it("errors for missing source file", {
    expect_error(
      resample_images(
        "nonexistent.gii", "also_missing.gii",
        src_space = "fsaverage",
        trg_space = "fsaverage"
      ),
      "not found"
    )
  })

  it("requires alt_space and alt_density for transform_to_alt", {
    src <- withr::local_tempfile(fileext = ".gii")
    trg <- withr::local_tempfile(fileext = ".gii")
    file.create(src)
    file.create(trg)
    expect_error(
      resample_images(
        src, trg,
        src_space = "fsaverage",
        trg_space = "fsLR",
        strategy = "transform_to_alt"
      ),
      "alt_space"
    )
  })
})
