describe("fetch_neuromaps_osf_json", {
  it("fetches and caches osf.json", {
    the$osf_json <- NULL
    fetch_count <- 0

    local_mocked_bindings(
      req_perform = function(req) "mock_response",
      resp_body_json = function(resp, ...) {
        fetch_count <<- fetch_count + 1
        list(annotations = list())
      },
      .package = "httr2"
    )

    result <- fetch_neuromaps_osf_json()
    expect_type(result, "list")

    fetch_neuromaps_osf_json()
    expect_equal(fetch_count, 1)

    the$osf_json <- NULL
  })
})


describe("fetch_neuromaps_meta_json", {
  it("fetches and caches meta.json", {
    the$meta_json <- NULL
    fetch_count <- 0

    local_mocked_bindings(
      req_perform = function(req) "mock_response",
      resp_body_json = function(resp, ...) {
        fetch_count <<- fetch_count + 1
        list(annotations = list())
      },
      .package = "httr2"
    )

    result <- fetch_neuromaps_meta_json()
    expect_type(result, "list")

    fetch_neuromaps_meta_json()
    expect_equal(fetch_count, 1)

    the$meta_json <- NULL
  })
})


describe("parse_osf_annotations", {
  it("parses annotation entries into tibble", {
    long_fname_l <- paste0(
      "source-abagen_desc-genepc1_space-fsaverage",
      "_den-10k_hemi-L_feature.func.gii"
    )
    long_fname_r <- paste0(
      "source-abagen_desc-genepc1_space-fsaverage",
      "_den-10k_hemi-R_feature.func.gii"
    )

    osf_data <- list(
      fsaverage = list(),
      annotations = list(
        list(
          source = "abagen", desc = "genepc1",
          space = "fsaverage",
          den = "10k", res = NULL, hemi = "L",
          format = "surface",
          fname = long_fname_l,
          rel_path = "abagen/genepc1/fsaverage/",
          checksum = "abc123",
          tags = list("genetics"),
          url = list("4mw3a", "file123")
        ),
        list(
          source = "abagen", desc = "genepc1",
          space = "fsaverage",
          den = "10k", res = NULL, hemi = "R",
          format = "surface",
          fname = long_fname_r,
          rel_path = "abagen/genepc1/fsaverage/",
          checksum = "def456",
          tags = list("genetics"),
          url = list("4mw3a", "file456")
        )
      )
    )

    result <- parse_osf_annotations(osf_data)
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 2)
    expect_equal(result$osf_project, c("4mw3a", "4mw3a"))
    expect_equal(result$osf_file_id, c("file123", "file456"))
    expect_equal(result$hemi, c("L", "R"))
  })

  it("skips template entries", {
    osf_data <- list(
      fsaverage = list("10k" = list(url = list("4mw3a", "xxx"))),
      annotations = list()
    )
    result <- parse_osf_annotations(osf_data)
    expect_equal(nrow(result), 0)
  })
})


describe("parse_meta_annotations", {
  it("parses meta entries into tibble", {
    meta_data <- list(annotations = list(
      list(
        annot = list(
          source = "abagen", desc = "genepc1",
          space = "fsaverage", den = "10k"
        ),
        full_desc = "Gene expression PC1",
        demographics = list(N = 6, age = "24-55")
      )
    ))

    result <- parse_meta_annotations(meta_data)
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 1)
    expect_equal(result$source, "abagen")
    expect_equal(result$full_desc, "Gene expression PC1")
    expect_equal(result$N, 6)
    expect_equal(result$age, "24-55")
  })
})


describe("build_neuromaps_registry", {
  it("merges osf and meta data", {
    the$registry <- NULL
    the$osf_json <- NULL
    the$meta_json <- NULL

    mock_fname <- paste0(
      "source-test_desc-map1_space-fsaverage",
      "_den-10k_hemi-L_feature.func.gii"
    )

    local_mocked_bindings(
      fetch_neuromaps_osf_json = function() {
        list(annotations = list(
          list(
            source = "test", desc = "map1",
            space = "fsaverage",
            den = "10k", res = NULL, hemi = "L",
            format = "surface",
            fname = mock_fname,
            rel_path = "test/map1/fsaverage/",
            checksum = "aaa", tags = list("pet"),
            url = list("4mw3a", "f1")
          )
        ))
      },
      fetch_neuromaps_meta_json = function() {
        list(annotations = list(
          list(
            annot = list(
              source = "test", desc = "map1",
              space = "fsaverage", den = "10k"
            ),
            full_desc = "Test map",
            demographics = list(N = 10, age = "30")
          )
        ))
      }
    )

    result <- build_neuromaps_registry()
    expect_s3_class(result, "tbl_df")
    expect_equal(result$full_desc, "Test map")
    expect_equal(result$N, 10)

    the$registry <- NULL
  })

  it("returns cached registry on second call", {
    the$registry <- tibble::tibble(
      source = "cached", desc = "x", space = "fs"
    )

    result <- build_neuromaps_registry()
    expect_equal(result$source, "cached")

    the$registry <- NULL
  })
})


describe("filter_neuromaps_registry", {
  reg <- tibble::tibble(
    source = c("abagen", "beliveau"),
    desc = c("genepc1", "az"),
    space = c("fsaverage", "MNI152"),
    den = c("10k", NA),
    res = c(NA, "1mm"),
    hemi = c("L", NA),
    format = c("surface", "volume"),
    tags = list(c("genetics"), c("pet"))
  )

  it("filters by source", {
    result <- filter_neuromaps_registry(reg, source = "abagen")
    expect_equal(nrow(result), 1)
    expect_equal(result$source, "abagen")
  })

  it("filters by desc", {
    result <- filter_neuromaps_registry(reg, desc = "genepc1")
    expect_equal(nrow(result), 1)
  })

  it("filters by space", {
    result <- filter_neuromaps_registry(reg, space = "MNI152")
    expect_equal(nrow(result), 1)
  })

  it("filters by density", {
    result <- filter_neuromaps_registry(reg, density = "10k")
    expect_equal(nrow(result), 1)
  })

  it("filters by resolution", {
    result <- filter_neuromaps_registry(
      reg, resolution = "1mm"
    )
    expect_equal(nrow(result), 1)
  })

  it("filters by hemisphere", {
    result <- filter_neuromaps_registry(
      reg, hemisphere = "L"
    )
    expect_equal(nrow(result), 1)
  })

  it("filters by format", {
    result <- filter_neuromaps_registry(
      reg, format = "volume"
    )
    expect_equal(nrow(result), 1)
  })

  it("filters by tags with AND logic", {
    reg2 <- tibble::tibble(
      source = c("a", "b", "c"),
      desc = c("x", "y", "z"),
      space = "fs", den = "10k", res = NA,
      hemi = "L", format = "surface",
      tags = list(
        c("pet", "receptor"), c("pet"),
        c("receptor", "pet")
      )
    )
    result <- filter_neuromaps_registry(
      reg2, tags = c("pet", "receptor")
    )
    expect_equal(nrow(result), 2)
  })
})


describe("resolve_neuromaps_entries", {
  it("errors on no match", {
    the$registry <- NULL

    local_mocked_bindings(
      build_neuromaps_registry = function() {
        tibble::tibble(
          source = character(), desc = character(),
          space = character(), den = character(),
          res = character(), hemi = character()
        )
      }
    )

    expect_error(
      resolve_neuromaps_entries(
        "nonexistent", "none", "nowhere"
      ),
      "No matching"
    )

    the$registry <- NULL
  })

  it("returns matching entries", {
    the$registry <- NULL

    local_mocked_bindings(
      build_neuromaps_registry = function() {
        tibble::tibble(
          source = c("test", "test"),
          desc = c("m", "m"),
          space = c("fs", "fs"),
          den = c("10k", "10k"),
          res = c(NA, NA), hemi = c("L", "R"),
          fname = c("f1.gii", "f2.gii")
        )
      }
    )

    result <- resolve_neuromaps_entries(
      "test", "m", "fs", density = "10k"
    )
    expect_equal(nrow(result), 2)

    the$registry <- NULL
  })

  it("filters by resolution", {
    the$registry <- NULL

    local_mocked_bindings(
      build_neuromaps_registry = function() {
        tibble::tibble(
          source = c("test", "test"),
          desc = c("m", "m"),
          space = c("mni", "mni"),
          den = c(NA, NA),
          res = c("1mm", "2mm"),
          hemi = c(NA, NA),
          fname = c("f1.nii.gz", "f2.nii.gz")
        )
      }
    )

    result <- resolve_neuromaps_entries(
      "test", "m", "mni", resolution = "1mm"
    )
    expect_equal(nrow(result), 1)

    the$registry <- NULL
  })

  it("filters by hemisphere", {
    the$registry <- NULL

    local_mocked_bindings(
      build_neuromaps_registry = function() {
        tibble::tibble(
          source = c("test", "test"),
          desc = c("m", "m"),
          space = c("fs", "fs"),
          den = c("10k", "10k"),
          res = c(NA, NA), hemi = c("L", "R"),
          fname = c("f1.gii", "f2.gii")
        )
      }
    )

    result <- resolve_neuromaps_entries(
      "test", "m", "fs", hemisphere = "L"
    )
    expect_equal(nrow(result), 1)
    expect_equal(result$hemi, "L")

    the$registry <- NULL
  })
})
