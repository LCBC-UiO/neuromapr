#' Cornblath spin test null model
#'
#' Spin-based null model where each rotated vertex receives the label of its
#' nearest non-medial-wall original vertex, then parcels are reassigned by
#' majority vote among the resulting vertex labels.
#'
#' @template null-params
#' @param coords List with `$lh` and `$rh` matrices of spherical coordinates
#'   (n x 3 each).
#' @param parcellation Integer vector of parcel labels for all vertices.
#'   `0` and `NA` indicate medial wall.
#' @param rotation Rotation generation method: `"euler"` (ZYZ Euler angles,
#'   default, matches neuromaps Python) or `"rodrigues"` (Rodrigues axis-angle
#'   formula).
#'
#' @return A [null_distribution] object.
#'
#' @references
#' Cornblath EJ et al. (2020) Communications Biology 3:590.
#' doi:10.1038/s42003-020-01296-5
#'
#' @examples
#' \dontrun{
#' coords <- list(lh = matrix(rnorm(30), 10, 3), rh = matrix(rnorm(30), 10, 3))
#' parcellation <- c(rep(1L, 5), rep(2L, 5), rep(3L, 5), rep(4L, 5))
#' data <- c(1.0, 2.0, 3.0, 4.0)
#' nd <- null_cornblath(data, coords, parcellation, n_perm = 10L, seed = 1L)
#' }
#' @export
null_cornblath <- function(
    data, coords, parcellation,
    n_perm = 1000L, seed = NULL,
    rotation = c("euler", "rodrigues")) {
  validate_data(data)
  validate_coords(coords)
  rotation <- match.arg(rotation)
  n_lh <- nrow(coords$lh)
  n_rh <- nrow(coords$rh)
  n <- n_lh + n_rh
  validate_parcellation(parcellation, n)

  valid <- !is.na(parcellation) & parcellation != 0
  ulabels <- sort(unique(parcellation[valid]))
  n_parcels <- length(ulabels)
  if (length(data) != n_parcels) {
    cli::cli_abort(paste(
      "Length of {.arg data} ({length(data)})",
      "must match number of parcels ({n_parcels})."
    ))
  }
  names(data) <- ulabels

  valid_lh <- which(valid[seq_len(n_lh)])
  valid_rh <- which(valid[n_lh + seq_len(n_rh)])

  rotated <- rotate_coords(
    coords$lh, coords$rh, n_perm, seed,
    rotation = rotation
  )
  nulls <- matrix(0, nrow = n_parcels, ncol = n_perm)

  msg <- "Generating cornblath nulls"
  for (i in cli::cli_progress_along(seq_len(n_perm), msg)) {
    rot_lh <- rotated$lh[, , i]
    rot_rh <- rotated$rh[, , i]

    new_labels_lh <- nearest_valid_label(
      rot_lh, coords$lh, parcellation[seq_len(n_lh)], valid_lh
    )
    new_labels_rh <- nearest_valid_label(
      rot_rh, coords$rh, parcellation[n_lh + seq_len(n_rh)], valid_rh
    )

    new_labels <- c(new_labels_lh, new_labels_rh)

    for (j in seq_along(ulabels)) {
      orig_mask <- which(parcellation == ulabels[j])
      votes <- new_labels[orig_mask]
      votes <- votes[!is.na(votes) & votes != 0]
      if (length(votes) > 0) {
        tbl <- table(votes)
        best_label <- names(tbl)[which.max(tbl)]
        nulls[j, i] <- data[best_label]
      } else {
        nulls[j, i] <- NA_real_
      }
    }
  }

  new_null_distribution(nulls, "cornblath", data, list(n_perm = n_perm))
}

#' @noRd
#' @keywords internal
nearest_valid_label <- function(
    rotated_coords, original_coords,
    labels, valid_idx) {
  valid_coords <- original_coords[valid_idx, , drop = FALSE]
  valid_labels <- labels[valid_idx]
  a <- rowSums(rotated_coords^2)
  b <- rowSums(valid_coords^2)
  dist_sq <- outer(a, b, "+") - 2 * tcrossprod(rotated_coords, valid_coords)
  nearest <- apply(dist_sq, 1, which.min)
  valid_labels[nearest]
}
