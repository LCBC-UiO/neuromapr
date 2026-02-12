describe("neuromaps_available", {
  it("returns full registry when no filters", {
    local_mocked_bindings(
      build_neuromaps_registry = function() {
        tibble::tibble(
          source = c("a", "b"), desc = c("x", "y"),
          space = "fs", den = "10k", res = NA,
          hemi = "L", format = "surface",
          fname = c("f1", "f2"),
          full_desc = c("A map", "B map"),
          tags = list("pet", "mri"),
          N = c(10, 20), age = c("30", "40")
        )
      }
    )

    result <- neuromaps_available()
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 2)
  })

  it("applies filters", {
    local_mocked_bindings(
      build_neuromaps_registry = function() {
        tibble::tibble(
          source = c("abagen", "beliveau"),
          desc = c("x", "y"),
          space = "fs", den = "10k", res = NA,
          hemi = "L", format = "surface",
          fname = c("f1", "f2"),
          full_desc = c("A", "B"),
          tags = list("genetics", "pet"),
          N = c(6, 18), age = c("30", "40")
        )
      }
    )

    result <- neuromaps_available(source = "beliveau")
    expect_equal(nrow(result), 1)
    expect_equal(result$source, "beliveau")
  })
})


describe("fetch_neuromaps_annotation", {
  it("errors when both density and resolution provided", {
    expect_error(
      fetch_neuromaps_annotation(
        "a", "b", "c",
        density = "10k", resolution = "1mm"
      ),
      "mutually exclusive"
    )
  })

  it("downloads resolved entries", {
    downloaded <- list()
    tmp_dir <- withr::local_tempdir()

    local_mocked_bindings(
      resolve_neuromaps_entries = function(...) {
        tibble::tibble(
          source = "test", desc = "m", space = "fs",
          den = "10k", res = NA, hemi = c("L", "R"),
          fname = c("left.gii", "right.gii"),
          rel_path = c("test/m/fs/", "test/m/fs/"),
          checksum = c("aaa", "bbb"),
          osf_project = c("4mw3a", "4mw3a"),
          osf_file_id = c("f1", "f2")
        )
      },
      download_neuromaps_file = function(url, destfile, ...) {
        downloaded[[length(downloaded) + 1]] <<- destfile
        destfile
      }
    )

    result <- fetch_neuromaps_annotation(
      "test", "m", "fs",
      density = "10k", data_dir = tmp_dir
    )
    expect_length(result, 2)
    expect_length(downloaded, 2)
  })
})
