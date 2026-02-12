#' Baum spin test null model
#'
#' Spin-based null model with maximum-overlap parcel reassignment. After
#' rotating vertex coordinates, each original parcel is assigned the value
#' of the rotated parcel with the most vertex overlap.
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
#' Baum GL et al. (2020) PNAS 117:21854-21861.
#' doi:10.1073/pnas.2005518117
#'
#' @export
null_baum <- function(data, coords, parcellation, n_perm = 1000L, seed = NULL,
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

  rotated <- rotate_coords(
    coords$lh, coords$rh, n_perm, seed,
    rotation = rotation
  )
  nulls <- matrix(0, nrow = n_parcels, ncol = n_perm)

  for (i in seq_len(n_perm)) {
    cost_lh <- compute_cost_matrix(coords$lh, rotated$lh[, , i])
    cost_rh <- compute_cost_matrix(coords$rh, rotated$rh[, , i])
    assign_lh <- apply(cost_lh, 1, which.min)
    assign_rh <- apply(cost_rh, 1, which.min)

    rotated_labels <- integer(n)
    rotated_labels[seq_len(n_lh)] <- parcellation[assign_lh]
    rotated_labels[n_lh + seq_len(n_rh)] <- parcellation[n_lh + assign_rh]

    for (j in seq_along(ulabels)) {
      orig_mask <- which(parcellation == ulabels[j])
      rot_at_orig <- rotated_labels[orig_mask]
      rot_at_orig <- rot_at_orig[rot_at_orig != 0 & !is.na(rot_at_orig)]
      if (length(rot_at_orig) > 0) {
        tbl <- table(rot_at_orig)
        best_label <- names(tbl)[which.max(tbl)]
        nulls[j, i] <- data[best_label]
      } else {
        nulls[j, i] <- NA_real_
      }
    }
  }

  new_null_distribution(nulls, "baum", data, list(n_perm = n_perm))
}
