#' Compare brain maps with spatial null model significance testing
#'
#' Computes the correlation between two brain maps and optionally tests
#' significance using a spatial null model to account for spatial
#' autocorrelation.
#'
#' @param x,y Numeric vectors of brain map values, or file paths to
#'   GIFTI/NIfTI files.
#' @param method Correlation method: `"pearson"` or `"spearman"`.
#' @param null_method Optional null model method for empirical p-values.
#'   One of `"burt2020"`, `"moran"`, `"spin_vasa"`, `"spin_hungarian"`,
#'   `"alexander_bloch"`, `"baum"`, `"cornblath"`, `"burt2018"`,
#'   or `NULL` for parametric only.
#' @param n_perm Integer number of null permutations.
#' @param nulls Pre-computed [null_distribution] object for `x`.
#' @param distmat Distance matrix (passed to null model if needed).
#' @param coords Coordinate list (passed to spin null models if needed).
#' @param seed Optional integer seed for reproducibility.
#' @param na.rm Logical, remove NA values before computing correlation.
#' @param verbose Logical, print progress messages.
#' @param ... Additional arguments passed to [generate_nulls()].
#'
#' @return A `neuromaps_enhanced_comparison` object (inherits
#'   `neuromaps_comparison`) with additional fields `p_null`, `null_method`,
#'   `null_r`, and `n_perm`.
#'
#' @references
#' Markello RD et al. (2022) Nature Methods 19:1472-1480.
#' doi:10.1038/s41592-022-01625-w
#'
#' @export
compare_maps <- function(x,
                         y,
                         method = c("pearson", "spearman"),
                         null_method = NULL,
                         n_perm = 1000L,
                         nulls = NULL,
                         distmat = NULL,
                         coords = NULL,
                         seed = NULL,
                         na.rm = TRUE, # nolint: object_name_linter.
                         verbose = TRUE,
                         ...) {
  method <- match.arg(method)

  if (is.character(x) && length(x) == 1 && file.exists(x)) {
    x <- read_brain_map_values(x)
  }

  if (is.character(y) && length(y) == 1 && file.exists(y)) {
    y <- read_brain_map_values(y)
  }

  if (length(x) != length(y)) {
    cli::cli_abort(
      "{.arg x} and {.arg y} must have the same length ({length(x)} vs {length(y)})."
    )
  }

  if (na.rm) {
    keep <- !is.na(x) & !is.na(y)
    x <- x[keep]
    y <- y[keep]
  }

  test <- stats::cor.test(x, y, method = method)
  r <- unname(test$estimate)
  p <- test$p.value
  n <- length(x)

  p_null <- NULL
  null_r <- NULL
  null_method_used <- null_method

  if (!is.null(nulls)) {
    validate_null_distribution(nulls)
    if (nrow(nulls$nulls) != n) {
      cli::cli_abort(
        "Pre-computed nulls have {nrow(nulls$nulls)} rows but data has {n} observations."
      )
    }
    null_method_used <- nulls$method
    n_perm <- nulls$n_perm
    if (verbose) cli::cli_alert_info("Using pre-computed {.val {null_method_used}} null distribution")
    null_r <- compute_null_correlations(nulls$nulls, y, method)
    p_null <- (sum(abs(null_r) >= abs(r)) + 1) / (n_perm + 1)
  } else if (!is.null(null_method)) {
    null_method <- match.arg(
      null_method,
      c("burt2020", "moran", "spin_vasa", "spin_hungarian",
        "alexander_bloch", "baum", "cornblath", "burt2018")
    )
    if (verbose) cli::cli_alert_info("Generating {.val {null_method}} nulls ({n_perm} permutations)")
    null_dist <- generate_nulls(
      x,
      method = null_method,
      n_perm = n_perm,
      distmat = distmat,
      coords = coords,
      seed = seed,
      ...
    )
    null_r <- compute_null_correlations(null_dist$nulls, y, method)
    p_null <- (sum(abs(null_r) >= abs(r)) + 1) / (n_perm + 1)
  }

  structure(
    list(
      r = r,
      p = p,
      p_null = p_null,
      method = method,
      n = n,
      null_method = null_method_used,
      null_r = null_r,
      n_perm = if (!is.null(null_r)) n_perm else NULL
    ),
    class = c("neuromaps_enhanced_comparison", "neuromaps_comparison")
  )
}

compute_null_correlations <- function(null_matrix, y, method) {
  apply(null_matrix, 2, function(null_x) {
    stats::cor(null_x, y, method = method, use = "complete.obs")
  })
}

#' @export
print.neuromaps_enhanced_comparison <- function(x, ...) {
  cli::cli_h3("Brain Map Comparison")
  cli::cli_text("{.field Method}: {x$method}")
  cli::cli_text(
    "{.field r} = {round(x$r, 4)}, {.field p} = {format.pval(x$p, digits = 3)}"
  )
  cli::cli_text("{.field n} = {x$n}")

  if (!is.null(x$p_null)) {
    cli::cli_rule()
    cli::cli_text("{.field Null model}: {x$null_method} ({x$n_perm} permutations)")
    cli::cli_text("{.field p_null} = {format.pval(x$p_null, digits = 3)}")
  }

  invisible(x)
}
