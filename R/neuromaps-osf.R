#' @noRd
#' @keywords internal
osf_download_url <- function(osf_project, osf_file_id) {
  paste0(
    "https://files.osf.io/v1/resources/",
    osf_project,
    "/providers/osfstorage/",
    osf_file_id
  )
}

#' @noRd
#' @keywords internal
download_neuromaps_file <- function(
  url,
  destfile,
  checksum = NULL,
  overwrite = FALSE,
  verbose = TRUE
) {
  if (file.exists(destfile) && !overwrite) {
    if (validate_checksum(destfile, checksum)) {
      if (verbose) {
        cli::cli_alert_info(
          "Using cached {.file {basename(destfile)}}"
        )
      }
      return(invisible(destfile))
    }
    if (verbose) {
      cli::cli_alert_warning(
        "Checksum mismatch, re-downloading {.file {basename(destfile)}}"
      )
    }
  }

  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)

  if (verbose) {
    cli::cli_alert("Downloading {.file {basename(destfile)}}")
  }
  utils::download.file(url, destfile, mode = "wb", quiet = !verbose)

  if (!validate_checksum(destfile, checksum)) {
    unlink(destfile)
    cli::cli_abort(
      "Checksum validation failed for {.file {basename(destfile)}}"
    )
  }

  if (verbose) cli::cli_alert_success("Saved {.file {basename(destfile)}}")
  invisible(destfile)
}
