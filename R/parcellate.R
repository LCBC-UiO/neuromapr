#' Aggregate vertex data into parcels
#'
#' Summarises vertex-level data by parcellation labels.
#'
#' @param data Numeric vector of vertex-level values.
#' @param labels Integer vector of parcel labels. `0` and `NA` are treated as
#'   medial wall and excluded.
#' @param summary_func Function to summarise each parcel (default: [mean]).
#'
#' @return Named numeric vector of parcel-level values.
#'
#' @references
#' Markello RD et al. (2022) Nature Methods 19:1472-1480.
#' doi:10.1038/s41592-022-01625-w
#'
#' @export
vertices_to_parcels <- function(data, labels, summary_func = mean) {
  if (length(data) != length(labels)) {
    cli::cli_abort(
      "{.arg data} ({length(data)}) and {.arg labels} ({length(labels)}) must have the same length."
    )
  }
  valid <- !is.na(labels) & labels != 0
  result <- tapply(data[valid], labels[valid], summary_func)
  stats::setNames(as.numeric(result), names(result))
}

#' Map parcel data back to vertices
#'
#' Expands parcel-level values to a vertex-level vector using parcellation
#' labels.
#'
#' @param parcel_data Named numeric vector of parcel values (names match labels).
#' @param labels Integer vector of parcel labels. `0` and `NA` are medial wall.
#' @param fill Value for medial wall vertices (default: `NA_real_`).
#'
#' @return Numeric vector of `length(labels)`.
#'
#' @references
#' Markello RD et al. (2022) Nature Methods 19:1472-1480.
#' doi:10.1038/s41592-022-01625-w
#'
#' @export
parcels_to_vertices <- function(parcel_data, labels, fill = NA_real_) {
  out <- rep(fill, length(labels))
  valid <- !is.na(labels) & labels != 0
  label_names <- as.character(labels[valid])
  matched <- parcel_data[label_names]
  out[valid] <- unname(matched)
  out
}

#' Parcellate brain map data
#'
#' High-level wrapper that reads data and parcellation from file paths or
#' vectors, then aggregates vertices into parcels.
#'
#' @param data Numeric vector or file path to a GIFTI/NIfTI brain map.
#' @param parcellation Integer vector of labels or file path to a GIFTI label
#'   file.
#' @param summary_func Function to summarise each parcel (default: [mean]).
#'
#' @return Named numeric vector of parcel-level values.
#'
#' @references
#' Markello RD et al. (2022) Nature Methods 19:1472-1480.
#' doi:10.1038/s41592-022-01625-w
#'
#' @export
parcellate <- function(data, parcellation, summary_func = mean) {
  if (is.character(data) && length(data) == 1) {
    data <- read_brain_map_values(data)
  }
  if (is.character(parcellation) && length(parcellation) == 1) {
    parcellation <- read_parcellation_labels(parcellation)
  }
  validate_parcellation(parcellation, length(data))
  vertices_to_parcels(data, parcellation, summary_func)
}

#' Unparcellate brain map data
#'
#' Inverse of [parcellate()]: maps parcel-level values back to vertices.
#'
#' @param parcel_data Named numeric vector of parcel values.
#' @param parcellation Integer vector of labels or file path to a GIFTI label
#'   file.
#' @param fill Value for medial wall vertices (default: `NA_real_`).
#'
#' @return Numeric vector of vertex-level values.
#'
#' @references
#' Markello RD et al. (2022) Nature Methods 19:1472-1480.
#' doi:10.1038/s41592-022-01625-w
#'
#' @export
unparcellate <- function(parcel_data, parcellation, fill = NA_real_) {
  if (is.character(parcellation) && length(parcellation) == 1) {
    parcellation <- read_parcellation_labels(parcellation)
  }
  parcels_to_vertices(parcel_data, parcellation, fill)
}

#' Compute parcel centroids
#'
#' Finds the centroid of each parcel using one of three methods.
#'
#' @param vertices Numeric matrix (n x 3) of vertex coordinates.
#' @param labels Integer vector of parcel labels. `0` and `NA` are medial wall.
#' @param method Centroid method: `"average"` (coordinate means), `"surface"`
#'   (vertex closest to the average centroid), or `"geodesic"` (vertex
#'   minimizing sum of geodesic distances within parcel).
#' @param faces Integer matrix (m x 3) of face indices. Required for
#'   `"geodesic"` method.
#'
#' @return Numeric matrix (n_parcels x 3) with rownames set to parcel labels.
#'
#' @references
#' Markello RD et al. (2022) Nature Methods 19:1472-1480.
#' doi:10.1038/s41592-022-01625-w
#'
#' @export
get_parcel_centroids <- function(vertices,
                                 labels,
                                 method = c("average", "surface", "geodesic"),
                                 faces = NULL) {
  method <- match.arg(method)
  if (!is.matrix(vertices) || ncol(vertices) != 3) {
    cli::cli_abort("{.arg vertices} must be a matrix with 3 columns.")
  }
  validate_parcellation(labels, nrow(vertices))

  if (method == "geodesic" && is.null(faces)) {
    cli::cli_abort("{.arg faces} is required for geodesic centroid method.")
  }

  valid <- !is.na(labels) & labels != 0
  ulabels <- sort(unique(labels[valid]))
  centroids <- matrix(0, nrow = length(ulabels), ncol = 3)
  rownames(centroids) <- ulabels

  for (i in seq_along(ulabels)) {
    idx <- which(labels == ulabels[i])
    parcel_verts <- vertices[idx, , drop = FALSE]

    if (method == "average") {
      centroids[i, ] <- colMeans(parcel_verts)
    } else if (method == "surface") {
      avg <- colMeans(parcel_verts)
      dists <- rowSums(sweep(parcel_verts, 2, avg)^2)
      centroids[i, ] <- parcel_verts[which.min(dists), ]
    } else {
      gdist <- get_surface_distance(vertices, faces, source_vertices = idx)
      gdist_within <- gdist[, idx, drop = FALSE]
      sum_dists <- rowSums(gdist_within)
      best <- which.min(sum_dists)
      centroids[i, ] <- parcel_verts[best, ]
    }
  }

  centroids
}

read_parcellation_labels <- function(path) {
  gii <- gifti::read_gifti(path)
  as.integer(gii$data[[1]])
}
