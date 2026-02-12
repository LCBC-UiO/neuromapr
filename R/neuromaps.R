#' List available neuromaps annotations
#'
#' Query the neuromaps registry to see which brain map annotations are
#' available for download. Data is fetched from the
#' [neuromaps](https://github.com/netneurolab/neuromaps) project's GitHub
#' repository and cached for the session.
#'
#' @template neuromaps-params
#' @param tags Character vector of tags. All must match (AND logic).
#' @param format Filter by format (`"surface"` or `"volume"`).
#'
#' @details
#' When used in `neuromaps_available()`, all parameters act as regex filters.
#' When used in [fetch_neuromaps_annotation()], `source`, `desc`, and `space`
#' are exact matches.
#'
#' @return A tibble of available annotations with columns: source, desc,
#'   space, den, res, hemi, format, fname, full_desc, tags, N, age.
#' @export
#' @examples
#' \dontrun{
#' neuromaps_available()
#' neuromaps_available(source = "beliveau")
#' neuromaps_available(tags = "pet")
#' }
neuromaps_available <- function(
  source = NULL,
  desc = NULL,
  space = NULL,
  density = NULL,
  resolution = NULL,
  hemisphere = NULL,
  tags = NULL,
  format = NULL
) {
  registry <- build_neuromaps_registry()
  filter_neuromaps_registry(
    registry,
    source = source,
    desc = desc,
    space = space,
    density = density,
    resolution = resolution,
    hemisphere = hemisphere,
    tags = tags,
    format = format
  )
}


#' Download a neuromaps annotation
#'
#' Download brain map annotation files from the neuromaps OSF repository.
#' Surface annotations return two files (left and right hemispheres),
#' volume annotations return one.
#'
#' @template neuromaps-params
#' @param data_dir Directory for cached downloads. Defaults to
#'   `neuromaps_cache_dir()`.
#' @param overwrite Re-download even if cached file exists.
#' @param verbose Print progress messages.
#'
#' @return Character vector of downloaded file path(s).
#' @export
#' @examples
#' \dontrun{
#' fetch_neuromaps_annotation("abagen", "genepc1", "fsaverage", density = "10k")
#' }
fetch_neuromaps_annotation <- function(
  source,
  desc,
  space,
  density = NULL,
  resolution = NULL,
  hemisphere = NULL,
  data_dir = neuromaps_cache_dir(),
  overwrite = FALSE,
  verbose = TRUE
) {
  if (!is.null(density) && !is.null(resolution)) {
    cli::cli_abort(
      "{.arg density} and {.arg resolution} are mutually exclusive."
    )
  }

  entries <- resolve_neuromaps_entries(
    source = source,
    desc = desc,
    space = space,
    density = density,
    resolution = resolution,
    hemisphere = hemisphere
  )

  paths <- character(nrow(entries))
  for (i in seq_len(nrow(entries))) {
    entry <- entries[i, ]
    destfile <- file.path(data_dir, "annotations", entry$rel_path, entry$fname)
    url <- osf_download_url(
      entry$osf_project, entry$osf_file_id
    )
    paths[i] <- download_neuromaps_file(
      url = url,
      destfile = destfile,
      checksum = entry$checksum,
      overwrite = overwrite,
      verbose = verbose
    )
  }

  paths
}
