#' Compute per-vertex surface areas
#'
#' Each triangle distributes one-third of its area to each of its three
#' vertices. Triangle area is computed via the cross-product formula.
#'
#' @param vertices Numeric matrix (n x 3) of vertex coordinates.
#' @param faces Integer matrix (m x 3) of face indices (1-indexed).
#'
#' @return Numeric vector of length `nrow(vertices)`.
#'
#' @export
vertex_areas <- function(vertices, faces) {
  if (!is.matrix(vertices) || ncol(vertices) != 3) {
    cli::cli_abort("{.arg vertices} must be a matrix with 3 columns.")
  }
  if (!is.matrix(faces) || ncol(faces) != 3) {
    cli::cli_abort("{.arg faces} must be a matrix with 3 columns.")
  }

  areas <- numeric(nrow(vertices))

  a <- vertices[faces[, 1], , drop = FALSE]
  b <- vertices[faces[, 2], , drop = FALSE]
  c <- vertices[faces[, 3], , drop = FALSE]
  ab <- b - a
  ac <- c - a

  cross <- cbind(
    ab[, 2] * ac[, 3] - ab[, 3] * ac[, 2],
    ab[, 3] * ac[, 1] - ab[, 1] * ac[, 3],
    ab[, 1] * ac[, 2] - ab[, 2] * ac[, 1]
  )
  tri_areas <- 0.5 * sqrt(rowSums(cross^2))
  third_area <- tri_areas / 3

  for (col in 1:3) {
    idx <- faces[, col]
    for (j in seq_along(idx)) {
      areas[idx[j]] <- areas[idx[j]] + third_area[j]
    }
  }

  areas
}

#' Convert FreeSurfer annotation to GIFTI
#'
#' Reads a FreeSurfer `.annot` file and writes a GIFTI label file.
#'
#' @param annot_path Path to FreeSurfer `.annot` file.
#' @param output_path Output GIFTI path. If `NULL`, replaces the extension
#'   with `.label.gii`.
#'
#' @return The output file path (invisibly).
#'
#' @references
#' Markello RD et al. (2022) Nature Methods 19:1472-1480.
#' doi:10.1038/s41592-022-01625-w
#'
#' @export
annot_to_gifti <- function(annot_path, output_path = NULL) {
  rlang::check_installed(
    "freesurferformats",
    reason = "to read FreeSurfer annotation files"
  )

  if (!file.exists(annot_path)) {
    cli::cli_abort("File not found: {.file {annot_path}}")
  }

  annot <- freesurferformats::read.fs.annot(annot_path)
  labels <- annot$label_codes

  if (is.null(output_path)) {
    output_path <- sub("\\.[^.]+$", ".label.gii", annot_path)
  }

  gii <- structure(
    list(
      data = list(as.integer(labels)),
      data_info = data.frame(
        Intent = "NIFTI_INTENT_LABEL",
        DataType = "NIFTI_TYPE_INT32",
        n = length(labels),
        Dim0 = length(labels),
        stringsAsFactors = FALSE
      )
    ),
    class = "gifti"
  )
  gifti::write_gifti(gii, output_path)

  invisible(output_path)
}

#' Convert FreeSurfer morphometry to GIFTI
#'
#' Reads a FreeSurfer morphometry file (`.curv`, `.thickness`, `.sulc`) and
#' writes a GIFTI func file.
#'
#' @param morph_path Path to FreeSurfer morphometry file.
#' @param output_path Output GIFTI path. If `NULL`, replaces the extension
#'   with `.func.gii`.
#'
#' @return The output file path (invisibly).
#'
#' @references
#' Markello RD et al. (2022) Nature Methods 19:1472-1480.
#' doi:10.1038/s41592-022-01625-w
#'
#' @export
fsmorph_to_gifti <- function(morph_path, output_path = NULL) {
  rlang::check_installed(
    "freesurferformats",
    reason = "to read FreeSurfer morphometry files"
  )

  if (!file.exists(morph_path)) {
    cli::cli_abort("File not found: {.file {morph_path}}")
  }

  morph <- freesurferformats::read.fs.morph(morph_path)

  if (is.null(output_path)) {
    output_path <- sub("\\.[^.]+$", ".func.gii", morph_path)
  }

  gii <- structure(
    list(
      data = list(as.numeric(morph)),
      data_info = data.frame(
        Intent = "NIFTI_INTENT_SHAPE",
        DataType = "NIFTI_TYPE_FLOAT32",
        n = length(morph),
        Dim0 = length(morph),
        stringsAsFactors = FALSE
      )
    ),
    class = "gifti"
  )
  gifti::write_gifti(gii, output_path)

  invisible(output_path)
}
