#' Variogram-matching null model
#'
#' Generates spatially-constrained surrogate brain maps by matching the
#' empirical variogram of the original data through smoothed random
#' permutations.
#'
#' @template null-params
#' @param distmat Distance matrix between parcels/vertices.
#' @param ns Integer, subsample size for variogram computation.
#' @param nh Integer, number of distance bins for variogram.
#' @param pv Numeric, percentile cutoff for maximum distance in variogram.
#' @param knn Integer, number of nearest neighbors for smoothing.
#' @param deltas Numeric vector of smoothing levels (fractions of `knn`).
#' @param kernel Smoothing kernel function.
#' @param resample Logical. If `FALSE` (default, matches brainsmash Python),
#'   variogram subsample indices are computed once and reused across all
#'   permutations. If `TRUE`, a fresh subsample is drawn each time.
#'
#' @return A [null_distribution] object.
#'
#' @references
#' Burt JB et al. (2020) NeuroImage 220:117038.
#' doi:10.1016/j.neuroimage.2020.117038
#'
#' @export
null_burt2020 <- function(data,
                          distmat,
                          n_perm = 1000L,
                          seed = NULL,
                          ns = 500L,
                          nh = 25L,
                          pv = 25,
                          knn = 1000L,
                          deltas = seq(0.1, 0.9, by = 0.1),
                          kernel = c("exponential", "gaussian", "uniform"),
                          resample = FALSE) {
  validate_data(data)
  n <- length(data)
  validate_distmat(distmat, n)
  kernel <- match.arg(kernel)
  knn <- min(as.integer(knn), n - 1L)

  if (!is.null(seed)) set.seed(seed)

  fixed_idx <- NULL
  if (!resample && ns < n) {
    fixed_idx <- sample.int(n, ns)
  }

  target_variogram <- compute_variogram(
    data, distmat, nh = nh, pv = pv, ns = ns,
    idx = fixed_idx
  )
  nn <- compute_knn(distmat, knn)

  nulls <- matrix(0, nrow = n, ncol = n_perm)

  for (i in seq_len(n_perm)) {
    permuted <- sample(data)
    best_sse <- Inf
    best_surrogate <- permuted

    for (delta in deltas) {
      k <- max(1L, round(delta * knn))
      smoothed <- smooth_surrogate(
        permuted, nn$indices, nn$distances, k, kernel
      )
      smoothed <- rank_match(smoothed, data)
      vg <- compute_variogram(
        smoothed, distmat, nh = nh, pv = pv, ns = ns,
        idx = fixed_idx
      )
      sse <- sum((vg$gamma - target_variogram$gamma)^2)

      if (sse < best_sse) {
        best_sse <- sse
        best_surrogate <- smoothed
      }
    }
    nulls[, i] <- best_surrogate
  }

  new_null_distribution(nulls, "burt2020", data, list(
    ns = ns, nh = nh, pv = pv, knn = knn,
    deltas = deltas, kernel = kernel
  ))
}

#' @noRd
#' @keywords internal
compute_variogram <- function(data, distmat, nh = 25L,
                              pv = 25, ns = 500L,
                              idx = NULL) {
  n <- length(data)
  if (is.null(idx) && ns < n) {
    idx <- sample.int(n, ns)
  }
  if (!is.null(idx)) {
    sub_data <- data[idx]
    sub_dist <- distmat[idx, idx]
  } else {
    sub_data <- data
    sub_dist <- distmat
  }

  upper_idx <- which(upper.tri(sub_dist), arr.ind = TRUE)
  dists <- sub_dist[upper_idx]
  diffs_sq <- 0.5 * (sub_data[upper_idx[, 1]] - sub_data[upper_idx[, 2]])^2

  max_dist <- stats::quantile(dists, pv / 100)
  keep <- dists <= max_dist
  dists <- dists[keep]
  diffs_sq <- diffs_sq[keep]

  breaks <- seq(0, max_dist, length.out = nh + 1L)
  bins <- findInterval(dists, breaks, all.inside = TRUE)

  gamma <- tapply(diffs_sq, bins, mean)
  bin_centers <- (breaks[-length(breaks)] + breaks[-1]) / 2
  present <- as.integer(names(gamma))

  data.frame(
    distance = bin_centers[present],
    gamma = as.numeric(gamma)
  )
}

#' @noRd
#' @keywords internal
smooth_surrogate <- function(permuted, nn_indices, nn_distances, k, kernel) {
  n <- length(permuted)
  k <- min(k, ncol(nn_indices))
  smoothed <- numeric(n)

  for (i in seq_len(n)) {
    idx <- nn_indices[i, seq_len(k)]
    d <- nn_distances[i, seq_len(k)]
    w <- switch(kernel,
      exponential = exp(-d / (max(d) + .Machine$double.eps)),
      gaussian = exp(-0.5 * (d / (max(d) + .Machine$double.eps))^2),
      uniform = rep(1, k)
    )
    smoothed[i] <- stats::weighted.mean(permuted[idx], w)
  }
  smoothed
}

#' @noRd
#' @keywords internal
rank_match <- function(surrogate, target) {
  target_sorted <- sort(target)
  target_sorted[rank(surrogate, ties.method = "first")]
}
