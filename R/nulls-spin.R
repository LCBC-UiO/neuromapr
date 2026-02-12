#' Spin-test null models for brain maps
#'
#' Generate spatially-constrained null distributions using spin-based
#' permutation of spherical coordinates.
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
#' Vasa F et al. (2018) Cerebral Cortex 28:3293-3303.
#' doi:10.1093/cercor/bhx195
#'
#' Markello RD, Misic B (2021) NeuroImage 236:118052.
#' doi:10.1016/j.neuroimage.2021.118052
#'
#' @export
null_spin_vasa <- function(data, coords, n_perm = 1000L,
                           seed = NULL,
                           rotation = c("euler", "rodrigues")) {
  validate_data(data)
  validate_coords(coords)
  rotation <- match.arg(rotation)
  n_lh <- nrow(coords$lh)
  n_rh <- nrow(coords$rh)
  n <- n_lh + n_rh
  if (length(data) != n) {
    cli::cli_abort(
      "Length of {.arg data} ({length(data)}) must match total parcels ({n})."
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
    assign_lh <- assign_parcels_vasa(cost_lh)
    assign_rh <- assign_parcels_vasa(cost_rh)
    nulls[seq_len(n_lh), i] <- data[assign_lh]
    nulls[n_lh + seq_len(n_rh), i] <- data[n_lh + assign_rh]
  }

  new_null_distribution(
    nulls, "spin_vasa", data, list(n_perm = n_perm)
  )
}

#' @rdname null_spin_vasa
#' @export
null_spin_hungarian <- function(
    data, coords, n_perm = 1000L, seed = NULL,
    rotation = c("euler", "rodrigues")) {
  rlang::check_installed(
    "clue",
    reason = "for Hungarian assignment in spin tests"
  )
  validate_data(data)
  validate_coords(coords)
  rotation <- match.arg(rotation)
  n_lh <- nrow(coords$lh)
  n_rh <- nrow(coords$rh)
  n <- n_lh + n_rh
  if (length(data) != n) {
    cli::cli_abort(
      "Length of {.arg data} ({length(data)}) must match total parcels ({n})."
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
    assign_lh <- assign_parcels_hungarian(cost_lh)
    assign_rh <- assign_parcels_hungarian(cost_rh)
    nulls[seq_len(n_lh), i] <- data[assign_lh]
    nulls[n_lh + seq_len(n_rh), i] <- data[n_lh + assign_rh]
  }

  new_null_distribution(
    nulls, "spin_hungarian", data, list(n_perm = n_perm)
  )
}

#' @noRd
#' @keywords internal
random_rotation_matrix <- function(
    method = c("euler", "rodrigues")) {
  method <- match.arg(method)
  switch(method,
    euler = random_rotation_euler(),
    rodrigues = random_rotation_rodrigues()
  )
}

#' @noRd
#' @keywords internal
random_rotation_euler <- function() {
  alpha <- stats::runif(1, 0, 2 * pi)
  beta <- acos(stats::runif(1, -1, 1))
  gamma <- stats::runif(1, 0, 2 * pi)
  ca <- cos(alpha)
  sa <- sin(alpha)
  cb <- cos(beta)
  sb <- sin(beta)
  cg <- cos(gamma)
  sg <- sin(gamma)
  matrix(c(
    cg * cb * ca - sg * sa,
    sg * cb * ca + cg * sa,
    -sb * ca,
    -cg * cb * sa - sg * ca,
    -sg * cb * sa + cg * ca,
    sb * sa,
    cg * sb,
    sg * sb,
    cb
  ), nrow = 3, ncol = 3)
}

#' @noRd
#' @keywords internal
random_rotation_rodrigues <- function() {
  u <- stats::rnorm(3)
  u <- u / sqrt(sum(u^2))
  theta <- stats::runif(1, 0, 2 * pi)
  ct <- cos(theta)
  st <- sin(theta)
  ux <- u[1]
  uy <- u[2]
  uz <- u[3]
  matrix(c(
    ct + ux^2 * (1 - ct),
    uy * ux * (1 - ct) + uz * st,
    uz * ux * (1 - ct) - uy * st,
    ux * uy * (1 - ct) - uz * st,
    ct + uy^2 * (1 - ct),
    uz * uy * (1 - ct) + ux * st,
    ux * uz * (1 - ct) + uy * st,
    uy * uz * (1 - ct) - ux * st,
    ct + uz^2 * (1 - ct)
  ), nrow = 3, ncol = 3)
}

#' @noRd
#' @keywords internal
rotate_coords <- function(coords_lh, coords_rh, n_perm,
                          seed = NULL, rotation = "euler") {
  if (!is.null(seed)) set.seed(seed)
  lh_rotated <- array(0, dim = c(nrow(coords_lh), 3, n_perm))
  rh_rotated <- array(0, dim = c(nrow(coords_rh), 3, n_perm))

  for (i in seq_len(n_perm)) {
    rot1 <- random_rotation_matrix(rotation)
    rot2 <- random_rotation_matrix(rotation)
    lh_rotated[, , i] <- coords_lh %*% rot1
    rh_rotated[, , i] <- coords_rh %*% rot2
  }
  list(lh = lh_rotated, rh = rh_rotated)
}

#' @noRd
#' @keywords internal
compute_cost_matrix <- function(original, rotated) {
  n <- nrow(original)
  cost <- matrix(0, nrow = n, ncol = n)
  for (i in seq_len(n)) {
    diff <- sweep(rotated, 2, original[i, ])
    cost[i, ] <- rowSums(diff^2)
  }
  cost
}

#' @noRd
#' @keywords internal
assign_parcels_vasa <- function(cost_matrix) {
  n <- nrow(cost_matrix)
  assignment <- integer(n)
  available <- rep(TRUE, n)

  for (i in seq_len(n)) {
    row_order <- order(cost_matrix[i, ])
    for (j in row_order) {
      if (available[j]) {
        assignment[i] <- j
        available[j] <- FALSE
        break
      }
    }
  }
  assignment
}

#' @noRd
#' @keywords internal
assign_parcels_hungarian <- function(cost_matrix) {
  sol <- clue::solve_LSAP(cost_matrix)
  as.integer(sol)
}
