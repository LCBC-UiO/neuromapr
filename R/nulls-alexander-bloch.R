#' Alexander-Bloch spin test null model
#'
#' Original vertex-level spin test: rotates coordinates and assigns each rotated
#' vertex the value of its nearest original vertex (no optimal matching).
#'
#' @template null-params
#' @param coords List with `$lh` and `$rh` matrices of spherical coordinates
#'   (n x 3 each).
#' @param rotation Rotation generation method: `"euler"` (ZYZ Euler angles,
#'   default, matches neuromaps Python) or `"rodrigues"` (Rodrigues axis-angle
#'   formula).
#'
#' @return A [null_distribution] object.
#'
#' @references
#' Alexander-Bloch AF et al. (2018) NeuroImage 175:111-120.
#' doi:10.1016/j.neuroimage.2018.04.023
#'
#' @export
null_alexander_bloch <- function(data, coords, n_perm = 1000L, seed = NULL,
                                 rotation = c("euler", "rodrigues")) {
  validate_data(data)
  validate_coords(coords)
  rotation <- match.arg(rotation)
  n_lh <- nrow(coords$lh)
  n_rh <- nrow(coords$rh)
  n <- n_lh + n_rh
  if (length(data) != n) {
    cli::cli_abort(
      "Length of {.arg data} ({length(data)}) must match total vertices ({n})."
    )
  }

  rotated <- rotate_coords(
    coords$lh, coords$rh, n_perm, seed,
    rotation = rotation
  )
  nulls <- matrix(0, nrow = n, ncol = n_perm)

  for (i in seq_len(n_perm)) {
    cost_lh <- compute_cost_matrix(coords$lh, rotated$lh[, , i])
    cost_rh <- compute_cost_matrix(coords$rh, rotated$rh[, , i])
    assign_lh <- apply(cost_lh, 1, which.min)
    assign_rh <- apply(cost_rh, 1, which.min)
    nulls[seq_len(n_lh), i] <- data[assign_lh]
    nulls[n_lh + seq_len(n_rh), i] <- data[n_lh + assign_rh]
  }

  new_null_distribution(nulls, "alexander_bloch", data, list(n_perm = n_perm))
}
