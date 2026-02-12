#' Generate null distributions for brain map data
#'
#' Dispatches to the appropriate null model method for generating
#' spatially-constrained surrogate brain maps.
#'
#' @template null-params
#' @param method Character string specifying the null model method.
#' @param distmat Distance matrix (required for `"burt2020"`, `"burt2018"`,
#'   and `"moran"`).
#' @param coords List with `$lh` and `$rh` coordinate matrices
#'   (required for spin methods).
#' @param parcellation Integer vector of parcel labels (required for `"baum"`
#'   and `"cornblath"`).
#' @param ... Additional arguments passed to the specific null method
#'   (e.g. `rotation` for spin methods, `kernel` for moran/burt2020).
#'
#' @return A [null_distribution] object.
#'
#' @references
#' Markello RD et al. (2022) Nature Methods 19:1472-1480.
#' doi:10.1038/s41592-022-01625-w
#'
#' @export
generate_nulls <- function(data,
                           method = c(
                             "burt2020", "moran", "spin_vasa", "spin_hungarian",
                             "alexander_bloch", "baum", "cornblath", "burt2018"
                           ),
                           n_perm = 1000L,
                           distmat = NULL,
                           coords = NULL,
                           parcellation = NULL,
                           seed = NULL,
                           ...) {
  method <- match.arg(method)
  validate_data(data)
  n_perm <- as.integer(n_perm)
  if (n_perm < 1L) {
    cli::cli_abort("{.arg n_perm} must be >= 1.")
  }

  switch(method,
    burt2020 = {
      if (is.null(distmat)) {
        cli::cli_abort("{.arg distmat} is required for method {.val burt2020}.")
      }
      null_burt2020(data, distmat, n_perm = n_perm, seed = seed, ...)
    },
    moran = {
      if (is.null(distmat)) {
        cli::cli_abort("{.arg distmat} is required for method {.val moran}.")
      }
      null_moran(data, distmat, n_perm = n_perm, seed = seed, ...)
    },
    spin_vasa = {
      if (is.null(coords)) {
        cli::cli_abort("{.arg coords} is required for method {.val spin_vasa}.")
      }
      null_spin_vasa(data, coords, n_perm = n_perm, seed = seed, ...)
    },
    spin_hungarian = {
      if (is.null(coords)) {
        cli::cli_abort(
          "{.arg coords} is required for method {.val spin_hungarian}."
        )
      }
      null_spin_hungarian(
        data, coords, n_perm = n_perm, seed = seed, ...
      )
    },
    alexander_bloch = {
      if (is.null(coords)) {
        cli::cli_abort(
          "{.arg coords} is required for method {.val alexander_bloch}."
        )
      }
      null_alexander_bloch(
        data, coords, n_perm = n_perm, seed = seed, ...
      )
    },
    baum = {
      if (is.null(coords)) {
        cli::cli_abort("{.arg coords} is required for method {.val baum}.")
      }
      if (is.null(parcellation)) {
        cli::cli_abort(
          "{.arg parcellation} is required for method {.val baum}."
        )
      }
      null_baum(
        data, coords, parcellation,
        n_perm = n_perm, seed = seed, ...
      )
    },
    cornblath = {
      if (is.null(coords)) {
        cli::cli_abort("{.arg coords} is required for method {.val cornblath}.")
      }
      if (is.null(parcellation)) {
        cli::cli_abort(
          "{.arg parcellation} is required for method {.val cornblath}."
        )
      }
      null_cornblath(
        data, coords, parcellation,
        n_perm = n_perm, seed = seed, ...
      )
    },
    burt2018 = {
      if (is.null(distmat)) {
        cli::cli_abort("{.arg distmat} is required for method {.val burt2018}.")
      }
      null_burt2018(data, distmat, n_perm = n_perm, seed = seed)
    }
  )
}
