#' Permutation test for any metric between brain maps
#'
#' Computes a user-specified metric between two vectors and tests significance
#' using either spatially-constrained null surrogates or simple random
#' permutation.
#'
#' @param x,y Numeric vectors.
#' @param metric_func Function taking `(x, y)` and returning a scalar.
#' @param n_perm Integer number of permutations.
#' @param seed Optional integer seed for reproducibility.
#' @param null_method Optional null model method passed to [generate_nulls()].
#'   If `NULL`, uses simple random permutation.
#' @param distmat Distance matrix (passed to [generate_nulls()] if needed).
#' @param coords Coordinate list (passed to [generate_nulls()] if needed).
#' @param parcellation Integer vector (passed to [generate_nulls()] if needed).
#' @param ... Additional arguments passed to [generate_nulls()].
#'
#' @return List with `$observed`, `$null_values`, `$p_value`, and `$n_perm`.
#'
#' @references
#' Markello RD et al. (2022) Nature Methods 19:1472-1480.
#' doi:10.1038/s41592-022-01625-w
#'
#' @export
permtest_metric <- function(x,
                            y,
                            metric_func = stats::cor,
                            n_perm = 1000L,
                            seed = NULL,
                            null_method = NULL,
                            distmat = NULL,
                            coords = NULL,
                            parcellation = NULL,
                            ...) {
  validate_data(x, "x")
  validate_data(y, "y")
  if (length(x) != length(y)) {
    cli::cli_abort(
      "{.arg x} ({length(x)}) and {.arg y} ({length(y)}) must have the same length."
    )
  }

  observed <- metric_func(x, y)
  n_perm <- as.integer(n_perm)

  if (!is.null(null_method)) {
    null_dist <- generate_nulls(
      x,
      method = null_method,
      n_perm = n_perm,
      distmat = distmat,
      coords = coords,
      parcellation = parcellation,
      seed = seed,
      ...
    )
    null_values <- apply(null_dist$nulls, 2, function(x_perm) {
      metric_func(x_perm, y)
    })
  } else {
    if (!is.null(seed)) set.seed(seed)
    null_values <- vapply(seq_len(n_perm), function(i) {
      metric_func(sample(x), y)
    }, numeric(1))
  }

  p_value <- (sum(abs(null_values) >= abs(observed)) + 1) / (n_perm + 1)

  list(
    observed = observed,
    null_values = null_values,
    p_value = p_value,
    n_perm = n_perm
  )
}
