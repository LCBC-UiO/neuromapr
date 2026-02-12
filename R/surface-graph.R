#' Build an igraph from a triangular surface mesh
#'
#' Extracts unique edges from triangular faces, computes Euclidean edge weights,
#' and returns an igraph graph object suitable for geodesic distance computation.
#'
#' @param vertices Numeric matrix (n x 3) of vertex coordinates.
#' @param faces Integer matrix (m x 3) of face indices (1-indexed).
#'
#' @return An `igraph` graph object with weighted edges.
#'
#' @references
#' Markello RD et al. (2022) Nature Methods 19:1472-1480.
#' doi:10.1038/s41592-022-01625-w
#'
#' @export
make_surf_graph <- function(vertices, faces) {
  if (!is.matrix(vertices) || ncol(vertices) != 3) {
    cli::cli_abort("{.arg vertices} must be a matrix with 3 columns.")
  }
  if (!is.matrix(faces) || ncol(faces) != 3) {
    cli::cli_abort("{.arg faces} must be a matrix with 3 columns.")
  }
  edges <- rbind(
    faces[, c(1, 2)],
    faces[, c(2, 3)],
    faces[, c(1, 3)]
  )
  edges <- t(apply(edges, 1, sort))
  edges <- unique(edges)

  weights <- sqrt(rowSums((vertices[edges[, 1], ] - vertices[edges[, 2], ])^2))

  igraph::graph_from_edgelist(edges, directed = FALSE) |>
    igraph::set_edge_attr("weight", value = weights)
}

#' Compute geodesic distances on a surface mesh
#'
#' Builds a graph from a triangular mesh and computes shortest-path (Dijkstra)
#' distances between vertices.
#'
#' @param vertices Numeric matrix (n x 3) of vertex coordinates.
#' @param faces Integer matrix (m x 3) of face indices (1-indexed).
#' @param source_vertices Optional integer vector of source vertex indices.
#'   If `NULL`, computes the full n x n distance matrix.
#'
#' @return Numeric distance matrix. If `source_vertices` is provided, returns
#'   a `length(source_vertices) x n` matrix; otherwise an `n x n` matrix.
#'
#' @references
#' Markello RD et al. (2022) Nature Methods 19:1472-1480.
#' doi:10.1038/s41592-022-01625-w
#'
#' @export
get_surface_distance <- function(vertices, faces, source_vertices = NULL) {
  g <- make_surf_graph(vertices, faces)
  if (is.null(source_vertices)) {
    igraph::distances(g, algorithm = "dijkstra")
  } else {
    igraph::distances(g, v = source_vertices, algorithm = "dijkstra")
  }
}
