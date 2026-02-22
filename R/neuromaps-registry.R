the <- new.env(parent = emptyenv())

neuromaps_osf_url <- paste0(
  "https://raw.githubusercontent.com/netneurolab/neuromaps/",
  "main/neuromaps/datasets/data/osf.json"
)

neuromaps_meta_url <- paste0(
  "https://raw.githubusercontent.com/netneurolab/neuromaps/",
  "main/neuromaps/datasets/data/meta.json"
)

#' @noRd
#' @keywords internal
fetch_neuromaps_osf_json <- function() {
  if (!is.null(the$osf_json)) return(the$osf_json)
  rlang::check_installed("httr2", reason = "to fetch neuromaps registry")
  resp <- httr2::request(neuromaps_osf_url) |>
    httr2::req_perform()
  the$osf_json <- httr2::resp_body_json(
    resp,
    check_type = FALSE
  )
  the$osf_json
}

#' @noRd
#' @keywords internal
fetch_neuromaps_meta_json <- function() {
  if (!is.null(the$meta_json)) return(the$meta_json)
  rlang::check_installed("httr2", reason = "to fetch neuromaps registry")
  resp <- httr2::request(neuromaps_meta_url) |>
    httr2::req_perform()
  the$meta_json <- httr2::resp_body_json(
    resp,
    check_type = FALSE
  )
  the$meta_json
}

#' @noRd
#' @keywords internal
parse_osf_annotations <- function(osf_data) {
  annotations <- osf_data[["annotations"]]
  rows <- lapply(annotations, function(a) {
    tibble::tibble(
      source   = a[["source"]] %||% NA_character_,
      desc     = a[["desc"]] %||% NA_character_,
      space    = a[["space"]] %||% NA_character_,
      den      = a[["den"]] %||% NA_character_,
      res      = a[["res"]] %||% NA_character_,
      hemi     = a[["hemi"]] %||% NA_character_,
      format   = a[["format"]] %||% NA_character_,
      fname    = a[["fname"]] %||% NA_character_,
      rel_path = a[["rel_path"]] %||% NA_character_,
      checksum = a[["checksum"]] %||% NA_character_,
      tags     = list(a[["tags"]] %||% character()),
      osf_project = a[["url"]][[1]] %||% NA_character_,
      osf_file_id = a[["url"]][[2]] %||% NA_character_
    )
  })
  dplyr::bind_rows(rows)
}

#' @noRd
#' @keywords internal
parse_meta_annotations <- function(meta_data) {
  annotations <- meta_data[["annotations"]]
  rows <- lapply(annotations, function(a) {
    annot <- a[["annot"]]
    demo <- a[["demographics"]] %||% list()
    tibble::tibble(
      source    = annot[["source"]] %||% NA_character_,
      desc      = annot[["desc"]] %||% NA_character_,
      space     = annot[["space"]] %||% NA_character_,
      den       = annot[["den"]] %||% NA_character_,
      res       = annot[["res"]] %||% NA_character_,
      full_desc = a[["full_desc"]] %||% NA_character_,
      N         = demo[["N"]] %||% NA_integer_,
      age       = as.character(demo[["age"]] %||% NA_character_)
    )
  })
  dplyr::bind_rows(rows)
}

#' @noRd
#' @keywords internal
build_neuromaps_registry <- function(refresh = FALSE) {
  if (!is.null(the$registry) && !refresh) return(the$registry)

  if (refresh) {
    the$osf_json <- NULL
    the$meta_json <- NULL
  }

  osf_data <- fetch_neuromaps_osf_json()
  meta_data <- fetch_neuromaps_meta_json()

  osf_tbl <- parse_osf_annotations(osf_data)
  meta_tbl <- parse_meta_annotations(meta_data)

  join_by <- intersect(
    c("source", "desc", "space", "den", "res"),
    intersect(names(osf_tbl), names(meta_tbl))
  )

  the$registry <- dplyr::left_join(osf_tbl, meta_tbl, by = join_by)
  the$registry
}

#' @noRd
#' @keywords internal
filter_neuromaps_registry <- function(
  registry,
  source = NULL,
  desc = NULL,
  space = NULL,
  density = NULL,
  resolution = NULL,
  hemisphere = NULL,
  tags = NULL,
  format = NULL,
  fixed = FALSE
) {
  if (!is.null(source)) {
    registry <- registry[grepl(source, registry$source, fixed = fixed), ]
  }
  if (!is.null(desc)) {
    registry <- registry[grepl(desc, registry$desc, fixed = fixed), ]
  }
  if (!is.null(space)) {
    registry <- registry[grepl(space, registry$space, fixed = fixed), ]
  }
  if (!is.null(density)) {
    registry <- registry[grepl(density, registry$den, fixed = fixed), ]
  }
  if (!is.null(resolution)) {
    registry <- registry[grepl(resolution, registry$res, fixed = fixed), ]
  }
  if (!is.null(hemisphere)) {
    registry <- registry[grepl(hemisphere, registry$hemi, fixed = fixed), ]
  }
  if (!is.null(format)) {
    registry <- registry[grepl(format, registry$format, fixed = fixed), ]
  }

  if (!is.null(tags)) {
    has_all_tags <- vapply(registry$tags, function(t) {
      all(tags %in% t)
    }, logical(1))
    registry <- registry[has_all_tags, ]
  }

  registry
}

#' Clear cached neuromaps registry data
#'
#' Removes the session-level cache of the neuromaps annotation registry,
#' forcing a fresh download on the next call to [neuromaps_available()] or
#' [fetch_neuromaps_annotation()].
#'
#' @return `NULL`, invisibly.
#' @examples
#' clear_neuromaps_cache()
#' @export
clear_neuromaps_cache <- function() {
  the$osf_json <- NULL
  the$meta_json <- NULL
  the$registry <- NULL
  invisible(NULL)
}

#' @noRd
#' @keywords internal
resolve_neuromaps_entries <- function(
  source,
  desc,
  space,
  density = NULL,
  resolution = NULL,
  hemisphere = NULL
) {
  registry <- build_neuromaps_registry()

  matches <- registry[
    registry$source == source &
      registry$desc == desc &
      registry$space == space,
  ]

  if (!is.null(density)) {
    matches <- matches[
      !is.na(matches$den) & matches$den == density,
    ]
  }
  if (!is.null(resolution)) {
    matches <- matches[
      !is.na(matches$res) & matches$res == resolution,
    ]
  }
  if (!is.null(hemisphere)) {
    matches <- matches[
      !is.na(matches$hemi) & matches$hemi == hemisphere,
    ]
  }

  if (nrow(matches) == 0) {
    cli::cli_abort(c(
      "No matching neuromaps annotation found.",
      "i" = "source={.val {source}}, desc={.val {desc}}, space={.val {space}}"
    ))
  }

  matches
}
