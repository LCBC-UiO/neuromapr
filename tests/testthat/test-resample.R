describe("get_gifti_density", {
  it("is not tested without gifti package and real files", {
    skip_if_not_installed("gifti")
    skip("Requires real GIFTI files")
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
