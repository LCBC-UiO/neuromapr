#' Transform brain maps between coordinate spaces
#'
#' Resamples GIFTI surface files between `fsaverage` and `fsLR` coordinate
#' spaces using Connectome Workbench via `ciftiTools`.
#'
#' @param paths Character vector of GIFTI file paths to transform.
#' @param target_space Target coordinate space: `"fsLR"` or `"fsaverage"`.
#' @param target_density Target mesh density (e.g., `"32k"`, `"164k"`).
#' @param hemisphere Which hemispheres to transform: `"left"`, `"right"`,
#'   or both.
#' @param method Resampling method: `"adaptive"` (default) or
#'   `"barycentric"`.
#' @param area_surf_current Path to the current-resolution area-correction
#'   surface (e.g. midthickness). Recommended when `method = "adaptive"` for
#'   proper vertex-area correction (matches neuromaps Python behaviour).
#' @param area_surf_new Path to the target-resolution area-correction surface.
#'   If `NULL` and `area_surf_current` is provided, ciftiTools resamples it
#'   barycentrically.
#' @param wb_path Path to `wb_command` executable. If `NULL`, auto-detected
#'   via `ciftiTools`.
#' @param verbose Logical, print progress messages.
#'
#' @return Character vector of transformed file paths.
#'
#' @references
#' Robinson EC et al. (2014) NeuroImage 100:414-426.
#' doi:10.1016/j.neuroimage.2014.07.033
#'
#' Robinson EC et al. (2018) NeuroImage 167:150-165.
#' doi:10.1016/j.neuroimage.2017.10.037
#'
#' @export
transform_to_space <- function(paths,
                               target_space = c("fsLR", "fsaverage"),
                               target_density = "32k",
                               hemisphere = c("left", "right"),
                               method = c("adaptive", "barycentric"),
                               area_surf_current = NULL,
                               area_surf_new = NULL,
                               wb_path = NULL,
                               verbose = TRUE) {
  rlang::check_installed("ciftiTools", reason = "to transform brain maps between spaces")
  target_space <- match.arg(target_space)
  method <- match.arg(method)
  hemisphere <- match.arg(hemisphere, several.ok = TRUE)
  wb_path <- check_wb_command(wb_path)
  ciftiTools::ciftiTools.setOption("wb_path", wb_path)

  out_paths <- character(length(paths))

  for (i in seq_along(paths)) {
    path <- paths[i]
    if (!file.exists(path)) {
      cli::cli_abort("File not found: {.file {path}}")
    }

    out_dir <- dirname(path)
    base <- tools::file_path_sans_ext(basename(path))
    out_file <- file.path(out_dir, paste0(base, "_", target_space, "_", target_density, ".func.gii"))

    if (verbose) {
      cli::cli_alert_info("Transforming {.file {basename(path)}} to {target_space} {target_density}")
    }

    resample_args <- list(
      original_fname = path,
      target_fname = out_file,
      hemisphere = if (length(hemisphere) == 1) hemisphere else "both",
      file_type = "metric",
      original_res = NULL,
      target_res = as.integer(gsub("k", "000", target_density)),
      resamp_method = if (method == "adaptive") "ADAP_BARY_AREA" else "BARYCENTRIC"
    )
    if (!is.null(area_surf_current)) {
      resample_args$area_original_fname <- area_surf_current
    }
    if (!is.null(area_surf_new)) {
      resample_args$area_target_fname <- area_surf_new
    }
    do.call(ciftiTools::resample_gifti, resample_args)

    out_paths[i] <- out_file
  }

  out_paths
}

#' Check for Connectome Workbench
#'
#' Verifies that `wb_command` is available. If `wb_path` is `NULL`,
#' checks `ciftiTools` default and system PATH.
#'
#' @param wb_path Optional explicit path to `wb_command`.
#'
#' @return Path to `wb_command` executable.
#'
#' @export
check_wb_command <- function(wb_path = NULL) {
  rlang::check_installed("ciftiTools", reason = "to locate wb_command")

  if (!is.null(wb_path)) {
    if (!file.exists(wb_path)) {
      cli::cli_abort("wb_command not found at {.file {wb_path}}")
    }
    return(wb_path)
  }

  ct_path <- tryCatch(
    ciftiTools::ciftiTools.getOption("wb_path"),
    error = function(e) NULL
  )
  if (!is.null(ct_path) && nzchar(ct_path) && file.exists(ct_path)) {
    return(ct_path)
  }

  sys_path <- Sys.which("wb_command")
  if (nzchar(sys_path)) {
    return(unname(sys_path))
  }

  cli::cli_abort(c(
    "Connectome Workbench {.code wb_command} not found.",
    i = "Install from {.url https://www.humanconnectome.org/software/get-connectome-workbench}",
    i = "Or set path with {.code ciftiTools::ciftiTools.setOption('wb_path', '/path/to/wb_command')}"
  ))
}
