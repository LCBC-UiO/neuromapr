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
  perform_download(url, destfile)

  if (!validate_checksum(destfile, checksum)) {
    unlink(destfile)
    cli::cli_abort(
      "Checksum validation failed for {.file {basename(destfile)}}"
    )
  }

  if (verbose) cli::cli_alert_success("Saved {.file {basename(destfile)}}")
  invisible(destfile)
}

#' @noRd
#' @keywords internal
perform_download <- function(url, destfile) {
  resp <- httr2::request(url) |>
    httr2::req_timeout(seconds = 120) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
  writeBin(httr2::resp_body_raw(resp), destfile)
}
