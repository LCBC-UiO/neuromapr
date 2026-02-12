#' Resample brain maps for comparison
#'
#' Aligns two brain maps into the same coordinate space and density before
#' comparison. Supports multiple strategies for choosing the target space.
#'
#' @param src Character, file path to the source GIFTI.
#' @param trg Character, file path to the target GIFTI.
#' @param src_space Source coordinate space (`"fsaverage"` or `"fsLR"`).
#' @param trg_space Target coordinate space (`"fsaverage"` or `"fsLR"`).
#' @param strategy Resampling strategy. One of `"downsample_only"`,
#'   `"transform_to_src"`, `"transform_to_trg"`, or `"transform_to_alt"`.
#' @param alt_space Alternative space for `"transform_to_alt"` strategy.
#' @param alt_density Alternative density for `"transform_to_alt"` strategy.
#' @param hemisphere Which hemispheres: `"left"`, `"right"`, or both.
#' @param area_surf_current Path to a current-resolution area-correction
#'   surface (e.g. midthickness). Passed to [transform_to_space()].
#' @param area_surf_new Path to a target-resolution area-correction surface.
#'   Passed to [transform_to_space()].
#' @param wb_path Path to `wb_command` executable.
#' @param verbose Logical, print progress messages.
#'
#' @return List with `$src` and `$trg` file paths in the aligned space.
#'
#' @references
#' Markello RD et al. (2022) Nature Methods 19:1472-1480.
#' doi:10.1038/s41592-022-01625-w
#'
#' @export
resample_images <- function(src,
                            trg,
                            src_space = c("fsaverage", "fsLR"),
                            trg_space = c("fsaverage", "fsLR"),
                            strategy = c(
                              "downsample_only",
                              "transform_to_src",
                              "transform_to_trg",
                              "transform_to_alt"
                            ),
                            alt_space = NULL,
                            alt_density = NULL,
                            hemisphere = c("left", "right"),
                            area_surf_current = NULL,
                            area_surf_new = NULL,
                            wb_path = NULL,
                            verbose = TRUE) {
  src_space <- match.arg(src_space)
  trg_space <- match.arg(trg_space)
  strategy <- match.arg(strategy)
  hemisphere <- match.arg(hemisphere, several.ok = TRUE)

  if (!file.exists(src)) cli::cli_abort("Source file not found: {.file {src}}")
  if (!file.exists(trg)) cli::cli_abort("Target file not found: {.file {trg}}")

  alt_missing <- is.null(alt_space) || is.null(alt_density)
  if (strategy == "transform_to_alt" && alt_missing) {
    cli::cli_abort(paste(
      "{.arg alt_space} and {.arg alt_density} are required",
      "for {.val transform_to_alt}."
    ))
  }
  if (strategy == "downsample_only" && src_space != trg_space) {
    cli::cli_abort(paste(
      "{.val downsample_only} requires maps in the same space",
      "({src_space} vs {trg_space})."
    ))
  }

  src_density <- get_gifti_density(src)
  trg_density <- get_gifti_density(trg)

  switch(strategy,
    downsample_only = {
      src_n <- density_to_n(src_density)
      trg_n <- density_to_n(trg_density)
      if (src_n > trg_n) {
        src_out <- transform_to_space(
          src, target_space = src_space, target_density = trg_density,
          hemisphere = hemisphere, area_surf_current = area_surf_current,
          area_surf_new = area_surf_new, wb_path = wb_path, verbose = verbose
        )
        list(src = src_out, trg = trg)
      } else if (trg_n > src_n) {
        trg_out <- transform_to_space(
          trg, target_space = trg_space, target_density = src_density,
          hemisphere = hemisphere, area_surf_current = area_surf_current,
          area_surf_new = area_surf_new, wb_path = wb_path, verbose = verbose
        )
        list(src = src, trg = trg_out)
      } else {
        list(src = src, trg = trg)
      }
    },
    transform_to_src = {
      trg_out <- transform_to_space(
        trg, target_space = src_space, target_density = src_density,
        hemisphere = hemisphere, area_surf_current = area_surf_current,
        area_surf_new = area_surf_new, wb_path = wb_path, verbose = verbose
      )
      list(src = src, trg = trg_out)
    },
    transform_to_trg = {
      src_out <- transform_to_space(
        src, target_space = trg_space, target_density = trg_density,
        hemisphere = hemisphere, area_surf_current = area_surf_current,
        area_surf_new = area_surf_new, wb_path = wb_path, verbose = verbose
      )
      list(src = src_out, trg = trg)
    },
    transform_to_alt = {
      src_out <- transform_to_space(
        src, target_space = alt_space, target_density = alt_density,
        hemisphere = hemisphere, area_surf_current = area_surf_current,
        area_surf_new = area_surf_new, wb_path = wb_path, verbose = verbose
      )
      trg_out <- transform_to_space(
        trg, target_space = alt_space, target_density = alt_density,
        hemisphere = hemisphere, area_surf_current = area_surf_current,
        area_surf_new = area_surf_new, wb_path = wb_path, verbose = verbose
      )
      list(src = src_out, trg = trg_out)
    }
  )
}

#' @noRd
#' @keywords internal
get_gifti_density <- function(path) {
  gii <- gifti::read_gifti(path)
  n_vert <- length(gii$data[[1]])
  density_map <- c(
    "163842" = "164k",
    "40962" = "41k",
    "32492" = "32k",
    "10242" = "10k"
  )
  key <- as.character(n_vert)
  if (key %in% names(density_map)) {
    density_map[[key]]
  } else {
    cli::cli_abort("Unknown vertex count {n_vert}, cannot determine density.")
  }
}

#' @noRd
#' @keywords internal
density_to_n <- function(density) {
  n_map <- c("164k" = 163842L, "41k" = 40962L, "32k" = 32492L, "10k" = 10242L)
  n_map[[density]]
}
