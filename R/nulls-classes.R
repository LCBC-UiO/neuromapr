#' Create a null distribution object
#'
#' @param nulls Numeric matrix (n x n_perm) of surrogate values.
#' @param method Character string identifying the null model method.
#' @param observed Numeric vector of original data values.
#' @param params Named list of algorithm parameters.
#'
#' @return A `null_distribution` object.
#'
#' @name null_distribution
#' @export
new_null_distribution <- function(nulls, method, observed, params = list()) {
  structure(
    list(
      nulls = nulls,
      method = method,
      observed = observed,
      params = params,
      n_perm = ncol(nulls),
      n = nrow(nulls)
    ),
    class = c("null_distribution", "list")
  )
}

validate_null_distribution <- function(x) {
  if (!inherits(x, "null_distribution")) {
    cli::cli_abort("{.arg x} must be a {.cls null_distribution} object.")
  }
  if (!is.matrix(x$nulls)) {
    cli::cli_abort("{.field nulls} must be a matrix.")
  }
  if (length(x$observed) != nrow(x$nulls)) {
    cli::cli_abort(
      "{.field observed} length ({length(x$observed)}) must match rows in {.field nulls} ({nrow(x$nulls)})."
    )
  }
  invisible(x)
}

#' @export
print.null_distribution <- function(x, ...) {
  cli::cli_h3("Null Distribution")
  cli::cli_ul(c(
    "Method: {x$method}",
    "Permutations: {x$n_perm}",
    "Observations: {x$n}"
  ))
  invisible(x)
}

#' @export
summary.null_distribution <- function(object, ...) {
  null_means <- rowMeans(object$nulls)
  null_sds <- apply(object$nulls, 1, stats::sd)
  list(
    method = object$method,
    n_perm = object$n_perm,
    n = object$n,
    null_mean = null_means,
    null_sd = null_sds,
    observed = object$observed
  )
}

#' @export
as.matrix.null_distribution <- function(x, ...) {
  x$nulls
}

#' @export
plot.null_distribution <- function(x, parcel = 1L, ...) {
  df <- data.frame(value = x$nulls[parcel, ])
  obs <- x$observed[parcel]
  ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(
      xintercept = obs,
      linetype = "dashed",
      color = "firebrick"
    ) +
    ggplot2::labs(
      title = paste("Null distribution:", x$method),
      subtitle = paste("Parcel", parcel),
      x = "Value",
      y = "Count"
    )
}
