#' @noRd
#' @keywords internal
read_surface_coordinates <- function(path) {
  ext <- tools::file_ext(path)
  if (ext == "gii" || grepl("\\.surf\\.gii$", path)) {
    g <- gifti::read_gifti(path)
    g$data[[1]]
  } else {
    cli::cli_abort("Unsupported surface file format: {.file {path}}")
  }
}

#' @noRd
#' @keywords internal
compute_distance_matrix <- function(coords,
                                    method = c("euclidean", "geodesic"),
                                    faces = NULL) {
  method <- match.arg(method)
  if (is.list(coords) && !is.null(coords$lh) && !is.null(coords$rh)) {
    coords <- rbind(coords$lh, coords$rh)
  }
  if (!is.matrix(coords) || ncol(coords) != 3) {
    cli::cli_abort("{.arg coords} must be a matrix with 3 columns (x, y, z).")
  }
  if (method == "geodesic") {
    if (is.null(faces)) {
      cli::cli_abort("{.arg faces} is required for geodesic distance.")
    }
    get_surface_distance(coords, faces)
  } else {
    stats::dist(coords) |> as.matrix()
  }
}

#' @noRd
#' @keywords internal
compute_knn <- function(distmat, k) {
  n <- nrow(distmat)
  k <- min(k, n - 1L)
  indices <- matrix(0L, nrow = n, ncol = k)
  distances <- matrix(0, nrow = n, ncol = k)
  for (i in seq_len(n)) {
    d <- distmat[i, ]
    d[i] <- Inf
    top_k <- order(d)[seq_len(k)]
    indices[i, ] <- top_k
    distances[i, ] <- d[top_k]
  }
  list(indices = indices, distances = distances)
}

#' @noRd
#' @keywords internal
validate_data <- function(data, arg = "data") {
  if (!is.numeric(data)) {
    cli::cli_abort("{.arg {arg}} must be numeric.")
  }
  if (anyNA(data)) {
    cli::cli_abort("{.arg {arg}} must not contain NA values.")
  }
  invisible(data)
}

#' @noRd
#' @keywords internal
validate_distmat <- function(distmat, n, arg = "distmat") {
  if (!is.matrix(distmat)) {
    cli::cli_abort("{.arg {arg}} must be a matrix.")
  }
  if (nrow(distmat) != n || ncol(distmat) != n) {
    cli::cli_abort(
      "{.arg {arg}} must be {n} x {n}, got {nrow(distmat)} x {ncol(distmat)}."
    )
  }
  invisible(distmat)
}

#' Read vertex/voxel values from a brain map file
#'
#' Reads GIFTI (`.func.gii`) or NIfTI (`.nii.gz`) files and returns the
#' values as a numeric vector. Used internally by [compare_maps()] when
#' file paths are passed instead of numeric vectors.
#'
#' @param path Path to a `.func.gii` (GIFTI) or `.nii.gz` (NIfTI) file.
#'
#' @return A numeric vector of map values.
#' @export
read_brain_map_values <- function(path) {
  if (grepl("\\.surf\\.gii$", path, ignore.case = TRUE)) {
    cli::cli_abort(c(
      "{.file {path}} is a surface geometry file, not a data file.",
      "i" = "Expected a {.file .func.gii} or {.file .label.gii} file."
    ))
  }
  if (grepl("\\.gii$", path, ignore.case = TRUE)) {
    gii <- gifti::read_gifti(path)
    return(as.numeric(gii$data[[1]]))
  }
  ext <- tolower(tools::file_ext(path))
  if (ext == "gz" || ext == "nii") {
    rlang::check_installed("RNifti", reason = "to read NIfTI volume files")
    img <- RNifti::readNifti(path)
    return(as.numeric(img))
  }
  cli::cli_abort("Unsupported file format: {.file {path}}")
}

#' @noRd
#' @keywords internal
validate_coords <- function(coords) {
  if (!is.list(coords) || is.null(coords$lh) || is.null(coords$rh)) {
    cli::cli_abort(paste(
      "{.arg coords} must be a list with",
      "{.field $lh} and {.field $rh} matrices."
    ))
  }
  if (!is.matrix(coords$lh) || ncol(coords$lh) != 3) {
    cli::cli_abort("{.field coords$lh} must be a matrix with 3 columns.")
  }
  if (!is.matrix(coords$rh) || ncol(coords$rh) != 3) {
    cli::cli_abort("{.field coords$rh} must be a matrix with 3 columns.")
  }
  invisible(coords)
}

#' @noRd
#' @keywords internal
validate_parcellation <- function(labels, n) {
  if (!is.numeric(labels) && !is.integer(labels)) {
    cli::cli_abort("{.arg parcellation} must be an integer or numeric vector.")
  }
  if (length(labels) != n) {
    cli::cli_abort(paste(
      "{.arg parcellation} length ({length(labels)})",
      "must match data length ({n})."
    ))
  }
  invisible(labels)
}

#' @noRd
#' @keywords internal
neuromaps_cache_dir <- function() {
  dir <- getOption(
    "neuromapr.data_dir",
    Sys.getenv("NEUROMAPR_DATA_DIR", unset = "")
  )
  if (nzchar(dir)) return(dir)
  tools::R_user_dir("neuromapr", "cache")
}

#' @noRd
#' @keywords internal
validate_checksum <- function(file, expected) {
  if (is.null(expected) || !nzchar(expected)) return(TRUE)
  actual <- unname(tools::md5sum(file))
  identical(actual, expected)
}

#' @noRd
#' @keywords internal
parse_neuromaps_filename <- function(fname) {
  base <- sub("_feature\\..*$", "", fname)
  ext <- sub("^.*_feature", "", fname)

  pairs <- strsplit(base, "_")[[1]]
  result <- list()
  for (pair in pairs) {
    parts <- strsplit(pair, "-", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      result[[parts[1]]] <- parts[2]
    }
  }
  result$ext <- ext
  result
}
