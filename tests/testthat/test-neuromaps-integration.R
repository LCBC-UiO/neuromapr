describe("neuromaps registry (integration)", {
  it("fetches osf.json from GitHub", {
    skip_if_not_installed("vcr")

    the$osf_json <- NULL

    vcr::use_cassette("neuromaps_osf_json", {
      result <- fetch_neuromaps_osf_json()
    })

    expect_type(result, "list")
    expect_true("annotations" %in% names(result))
    expect_true(length(result$annotations) > 0)

    the$osf_json <- NULL
  })

  it("fetches meta.json from GitHub", {
    skip_if_not_installed("vcr")

    the$meta_json <- NULL

    vcr::use_cassette("neuromaps_meta_json", {
      result <- fetch_neuromaps_meta_json()
    })

    expect_type(result, "list")
    expect_true("annotations" %in% names(result))
    expect_true(length(result$annotations) > 0)

    the$meta_json <- NULL
  })

  it("builds and queries the full registry", {
    skip_if_not_installed("vcr")

    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL

    vcr::use_cassette("neuromaps_registry_full", {
      result <- neuromaps_available()
    })

    expect_s3_class(result, "tbl_df")
    expect_true(nrow(result) > 10)
    expect_true(all(
      c("source", "desc", "space", "hemi", "format") %in%
        names(result)
    ))

    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL
  })

  it("filters the live registry by source", {
    skip_if_not_installed("vcr")

    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL

    vcr::use_cassette("neuromaps_registry_filter_source", {
      result <- neuromaps_available(source = "abagen")
    })

    expect_true(nrow(result) > 0)
    expect_true(all(grepl("abagen", result$source)))

    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL
  })

  it("filters by format", {
    skip_if_not_installed("vcr")

    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL

    vcr::use_cassette("neuromaps_registry_filter_format", {
      surfaces <- neuromaps_available(format = "surface")
      volumes <- neuromaps_available(format = "volume")
    })

    expect_true(nrow(surfaces) > 0)
    expect_true(nrow(volumes) > 0)
    expect_true(all(surfaces$format == "surface"))
    expect_true(all(volumes$format == "volume"))

    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL
  })
})


describe("neuromaps download (integration)", {
  it("downloads a real annotation from OSF", {
    skip_on_cran()
    skip_if_offline()

    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL

    tmp_dir <- withr::local_tempdir()

    paths <- fetch_neuromaps_annotation(
      source = "abagen",
      desc = "genepc1",
      space = "fsaverage",
      density = "10k",
      hemisphere = "L",
      data_dir = tmp_dir,
      verbose = FALSE
    )

    expect_length(paths, 1)
    expect_true(file.exists(paths))
    expect_true(file.size(paths) > 0)
    expect_true(grepl("hemi-L", paths))
    expect_true(grepl("\\.func\\.gii$", paths))

    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL
  })

  it("uses cache on second download", {
    skip_on_cran()
    skip_if_offline()

    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL

    tmp_dir <- withr::local_tempdir()

    paths1 <- fetch_neuromaps_annotation(
      source = "abagen",
      desc = "genepc1",
      space = "fsaverage",
      density = "10k",
      hemisphere = "L",
      data_dir = tmp_dir,
      verbose = FALSE
    )

    mtime1 <- file.mtime(paths1)

    paths2 <- fetch_neuromaps_annotation(
      source = "abagen",
      desc = "genepc1",
      space = "fsaverage",
      density = "10k",
      hemisphere = "L",
      data_dir = tmp_dir,
      verbose = FALSE
    )

    expect_equal(paths1, paths2)
    expect_equal(file.mtime(paths2), mtime1)

    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL
  })

  it("reads and compares downloaded files", {
    skip_on_cran()
    skip_if_offline()

    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL

    tmp_dir <- withr::local_tempdir()

    paths <- fetch_neuromaps_annotation(
      source = "abagen",
      desc = "genepc1",
      space = "fsaverage",
      density = "10k",
      data_dir = tmp_dir,
      verbose = FALSE
    )

    result <- compare_maps(paths[1], paths[1])
    expect_s3_class(result, "neuromaps_comparison")
    expect_equal(result$r, 1)
    expect_true(result$n > 0)

    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL
  })
})
