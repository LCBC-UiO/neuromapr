describe("osf_download_url", {
  it("constructs correct URL", {
    url <- osf_download_url("4mw3a", "abc123")
    expect_equal(
      url,
      "https://files.osf.io/v1/resources/4mw3a/providers/osfstorage/abc123"
    )
  })
})


describe("download_neuromaps_file", {
  it("skips download when cached file has valid checksum", {
    tmp <- withr::local_tempfile()
    writeLines("cached content", tmp)
    md5 <- unname(tools::md5sum(tmp))

    result <- download_neuromaps_file(
      url = "https://example.com/fake",
      destfile = tmp,
      checksum = md5,
      overwrite = FALSE,
      verbose = FALSE
    )
    expect_equal(result, tmp)
  })

  it("re-downloads when overwrite is TRUE", {
    tmp_dir <- withr::local_tempdir()
    destfile <- file.path(tmp_dir, "test.gii")
    writeLines("old content", destfile)

    download_called <- FALSE
    local_mocked_bindings(
      download.file = function(url, destfile, ...) {
        download_called <<- TRUE
        writeLines("new content", destfile)
      },
      .package = "utils"
    )

    download_neuromaps_file(
      url = "https://example.com/fake",
      destfile = destfile,
      checksum = NULL,
      overwrite = TRUE,
      verbose = FALSE
    )
    expect_true(download_called)
  })

  it("errors on checksum mismatch after download", {
    tmp_dir <- withr::local_tempdir()
    destfile <- file.path(tmp_dir, "test.gii")

    local_mocked_bindings(
      download.file = function(url, destfile, ...) {
        writeLines("bad content", destfile)
      },
      .package = "utils"
    )

    expect_error(
      download_neuromaps_file(
        url = "https://example.com/fake",
        destfile = destfile,
        checksum = "0000000000000000",
        overwrite = FALSE,
        verbose = FALSE
      ),
      "Checksum validation failed"
    )
    expect_false(file.exists(destfile))
  })

  it("re-downloads when checksum mismatches cached file", {
    tmp_dir <- withr::local_tempdir()
    destfile <- file.path(tmp_dir, "test.gii")
    writeLines("stale content", destfile)
    fresh_file <- withr::local_tempfile()
    writeLines("fresh content", fresh_file)
    fresh_md5 <- unname(tools::md5sum(fresh_file))

    local_mocked_bindings(
      download.file = function(url, destfile, ...) {
        writeLines("fresh content", destfile)
      },
      .package = "utils"
    )

    expect_message(
      expect_message(
        expect_message(
          download_neuromaps_file(
            url = "https://example.com/fake",
            destfile = destfile,
            checksum = fresh_md5,
            overwrite = FALSE,
            verbose = TRUE
          ),
          "mismatch"
        ),
        "Downloading"
      ),
      "Saved"
    )
  })

  it("prints verbose messages on cache hit", {
    tmp <- withr::local_tempfile()
    writeLines("cached", tmp)
    md5 <- unname(tools::md5sum(tmp))

    expect_message(
      download_neuromaps_file(
        url = "https://example.com/fake",
        destfile = tmp,
        checksum = md5,
        verbose = TRUE
      ),
      "cached"
    )
  })

  it("prints verbose messages during download", {
    tmp_dir <- withr::local_tempdir()
    destfile <- file.path(tmp_dir, "test.gii")

    local_mocked_bindings(
      download.file = function(url, destfile, ...) {
        writeLines("content", destfile)
      },
      .package = "utils"
    )

    expect_message(
      expect_message(
        download_neuromaps_file(
          url = "https://example.com/fake",
          destfile = destfile,
          checksum = NULL,
          verbose = TRUE
        ),
        "Downloading"
      ),
      "Saved"
    )
  })

  it("creates parent directories", {
    tmp_dir <- withr::local_tempdir()
    destfile <- file.path(tmp_dir, "a", "b", "test.gii")

    local_mocked_bindings(
      download.file = function(url, destfile, ...) {
        writeLines("content", destfile)
      },
      .package = "utils"
    )

    download_neuromaps_file(
      url = "https://example.com/fake",
      destfile = destfile,
      checksum = NULL,
      overwrite = FALSE,
      verbose = FALSE
    )
    expect_true(file.exists(destfile))
  })
})
