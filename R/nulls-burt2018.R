#' Burt 2018 spatial autoregressive null model
#'
#' Generates surrogate brain maps using a spatial autoregressive (SAR) model.
#' Estimates spatial autocorrelation and distance decay parameters, then
#' generates surrogates by solving the SAR equation and rank-matching to the
#' original data.
#'
#' @template null-params
#' @param distmat Distance matrix between parcels/vertices.
#'
#' @return A [null_distribution] object.
#'
#' @references
#' Burt JB et al. (2018) Nature Neuroscience 21:1251-1259.
#' doi:10.1038/s41593-018-0195-0
#'
#' @export
null_burt2018 <- function(data, distmat, n_perm = 1000L, seed = NULL) {
  validate_data(data)
  n <- length(data)
  validate_distmat(distmat, n)

  params <- estimate_sar_params(data, distmat)
  weight_mat <- build_sar_weights(distmat, params$d0)

  if (!is.null(seed)) set.seed(seed)
  nulls <- matrix(0, nrow = n, ncol = n_perm)

  for (i in seq_len(n_perm)) {
    nulls[, i] <- sar_surrogate(weight_mat, params$rho, data)
  }

  new_null_distribution(nulls, "burt2018", data, list(
    n_perm = n_perm,
    rho = params$rho,
    d0 = params$d0
  ))
}

estimate_sar_params <- function(data, distmat) {
  d_vals <- distmat[upper.tri(distmat)]
  d_candidates <- stats::quantile(d_vals[d_vals > 0], probs = seq(0.1, 0.9, by = 0.1))

  best_sse <- Inf
  best_rho <- 0
  best_d0 <- stats::median(d_vals[d_vals > 0])

  for (d0 in d_candidates) {
    w <- build_sar_weights(distmat, d0)
    wx <- drop(w %*% data)
    fit <- stats::lm.fit(cbind(1, wx), data)
    rho <- fit$coefficients[2]
    sse <- sum(fit$residuals^2)
    if (sse < best_sse) {
      best_sse <- sse
      best_rho <- rho
      best_d0 <- d0
    }
  }

  list(rho = best_rho, d0 = best_d0)
}

build_sar_weights <- function(distmat, d0) {
  w <- exp(-distmat / d0)
  diag(w) <- 0
  rs <- rowSums(w)
  rs[rs == 0] <- 1
  w / rs
}

sar_surrogate <- function(weight_mat, rho, data) {
  n <- length(data)
  z <- stats::rnorm(n)
  A <- diag(n) - rho * weight_mat
  surrogate <- solve(A, z)
  rank_match(surrogate, data)
}
