#' Moran spectral randomization null model
#'
#' Generates spatially-constrained surrogate brain maps using Moran's
#' eigenvector maps (MEMs) for spectral randomization.
#'
#' @template null-params
#' @param distmat Distance matrix between parcels/vertices.
#' @param procedure Character, either `"pair"` (default, random 2D rotations
#'   of near-degenerate eigenvector pairs, matches neuromaps Python) or
#'   `"singleton"` (random sign flips of individual eigenvectors).
#' @param kernel Weight matrix kernel: `"inverse_distance"` (default, matches
#'   neuromaps Python), `"exponential"`, `"gaussian"`, or `"bisquare"`.
#' @param tol Numeric tolerance for eigenvalue comparison.
#'
#' @return A [null_distribution] object.
#'
#' @references
#' Wagner HH, Dray S (2015) Methods in Ecology and Evolution 6:1169-1178.
#' doi:10.1111/2041-210X.12407
#'
#' @export
null_moran <- function(data,
                       distmat,
                       n_perm = 1000L,
                       seed = NULL,
                       procedure = c("pair", "singleton"),
                       kernel = c("inverse_distance", "exponential",
                                  "gaussian", "bisquare"),
                       tol = 1e-6) {
  validate_data(data)
  procedure <- match.arg(procedure)
  kernel <- match.arg(kernel)
  n <- length(data)
  validate_distmat(distmat, n)

  weight_mat <- compute_weight_matrix(distmat, kernel = kernel)
  mem <- compute_mem(weight_mat, n)

  eigvecs <- mem$vectors
  eigvals <- mem$values
  n_mem <- ncol(eigvecs)

  coeffs <- drop(crossprod(eigvecs, data - mean(data)))

  if (!is.null(seed)) set.seed(seed)
  nulls <- matrix(0, nrow = n, ncol = n_perm)

  if (procedure == "singleton") {
    for (i in seq_len(n_perm)) {
      signs <- sample(c(-1, 1), n_mem, replace = TRUE)
      nulls[, i] <- eigvecs %*% (coeffs * signs) + mean(data)
    }
  } else {
    pairs <- make_pairs(eigvals, tol)
    for (i in seq_len(n_perm)) {
      new_coeffs <- coeffs
      for (pair in pairs) {
        if (length(pair) == 1) {
          new_coeffs[pair] <- new_coeffs[pair] * sample(c(-1, 1), 1)
        } else {
          theta <- stats::runif(1, 0, 2 * pi)
          ct <- cos(theta)
          st <- sin(theta)
          a <- new_coeffs[pair[1]]
          b <- new_coeffs[pair[2]]
          new_coeffs[pair[1]] <- a * ct - b * st
          new_coeffs[pair[2]] <- a * st + b * ct
        }
      }
      nulls[, i] <- eigvecs %*% new_coeffs + mean(data)
    }
  }

  new_null_distribution(nulls, "moran", data, list(
    procedure = procedure,
    kernel = kernel,
    n_mem = n_mem,
    tol = tol
  ))
}

#' @noRd
#' @keywords internal
compute_weight_matrix <- function(
    distmat,
    kernel = c("inverse_distance", "exponential",
               "gaussian", "bisquare")) {
  kernel <- match.arg(kernel)

  w <- switch(kernel,
    inverse_distance = {
      inv <- 1 / (distmat + .Machine$double.eps)
      diag(inv) <- 0
      inv
    },
    exponential = {
      bw <- stats::median(distmat[distmat > 0])
      exp(-distmat / bw)
    },
    gaussian = {
      bw <- stats::median(distmat[distmat > 0])
      exp(-0.5 * (distmat / bw)^2)
    },
    bisquare = {
      bw <- stats::median(distmat[distmat > 0])
      u <- distmat / bw
      ifelse(u < 1, (1 - u^2)^2, 0)
    }
  )
  diag(w) <- 0
  rs <- rowSums(w)
  rs[rs == 0] <- 1
  w / rs
}

#' @noRd
#' @keywords internal
compute_mem <- function(weight_mat, n) {
  centering <- diag(n) - matrix(1 / n, n, n)
  sym_w <- (weight_mat + t(weight_mat)) / 2
  dbl_centered <- centering %*% sym_w %*% centering

  if (n > 5000 && rlang::is_installed("RSpectra")) {
    k <- min(n - 1, 500)
    eig <- RSpectra::eigs_sym(dbl_centered, k = k)
  } else {
    eig <- eigen(dbl_centered, symmetric = TRUE)
  }

  keep <- abs(eig$values) > 1e-10
  list(
    vectors = eig$vectors[, keep, drop = FALSE],
    values = eig$values[keep]
  )
}

#' @noRd
#' @keywords internal
make_pairs <- function(eigvals, tol = 1e-6) {
  n <- length(eigvals)
  used <- logical(n)
  pairs <- list()

  for (i in seq_len(n)) {
    if (used[i]) next
    paired <- FALSE
    if (i < n) {
      for (j in (i + 1):n) {
        if (!used[j] && abs(eigvals[i] - eigvals[j]) < tol) {
          pairs <- c(pairs, list(c(i, j)))
          used[c(i, j)] <- TRUE
          paired <- TRUE
          break
        }
      }
    }
    if (!paired) {
      pairs <- c(pairs, list(i))
      used[i] <- TRUE
    }
  }
  pairs
}
