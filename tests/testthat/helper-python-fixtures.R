fixture_dir <- function() {
  testthat::test_path("fixtures", "python")
}

skip_if_no_python_fixtures <- function() {
  dir <- fixture_dir()
  if (!dir.exists(dir) || length(list.files(dir, pattern = "\\.json$")) == 0) {
    testthat::skip(paste(
      "Python reference fixtures not found",
      "(run tools/generate-reference-data.py)"
    ))
  }
}

load_python_fixture <- function(name) {
  path <- file.path(fixture_dir(), paste0(name, ".json"))
  if (!file.exists(path)) {
    testthat::skip(paste0("Fixture not found: ", name, ".json"))
  }
  jsonlite::fromJSON(path, simplifyVector = TRUE)
}
