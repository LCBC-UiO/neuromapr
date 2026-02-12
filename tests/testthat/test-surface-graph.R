describe("make_surf_graph", {
  it("builds a weighted igraph from a simple mesh", {
    skip_if_not_installed("igraph")
    vertices <- matrix(c(
      0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      1, 1, 0
    ), ncol = 3, byrow = TRUE)
    faces <- matrix(c(1, 2, 3, 2, 3, 4), ncol = 3, byrow = TRUE)

    g <- make_surf_graph(vertices, faces)
    expect_s3_class(g, "igraph")
    expect_true(igraph::is_weighted(g))
    expect_equal(igraph::vcount(g), 4)
  })

  it("errors for non-3-column vertices", {
    expect_error(
      make_surf_graph(matrix(1:4, ncol = 2), matrix(1:3, ncol = 3)),
      "3 columns"
    )
  })

  it("errors for non-3-column faces", {
    expect_error(
      make_surf_graph(matrix(1:6, ncol = 3), matrix(1:4, ncol = 2)),
      "3 columns"
    )
  })
})

describe("get_surface_distance", {
  it("returns full distance matrix by default", {
    skip_if_not_installed("igraph")
    vertices <- matrix(c(
      0, 0, 0,
      1, 0, 0,
      0, 1, 0
    ), ncol = 3, byrow = TRUE)
    faces <- matrix(c(1, 2, 3), ncol = 3)

    dm <- get_surface_distance(vertices, faces)
    expect_equal(nrow(dm), 3)
    expect_equal(ncol(dm), 3)
    expect_equal(dm[1, 1], 0)
    expect_true(dm[1, 2] > 0)
  })

  it("returns subset when source_vertices given", {
    skip_if_not_installed("igraph")
    vertices <- matrix(c(
      0, 0, 0,
      1, 0, 0,
      0, 1, 0
    ), ncol = 3, byrow = TRUE)
    faces <- matrix(c(1, 2, 3), ncol = 3)

    dm <- get_surface_distance(vertices, faces, source_vertices = 1L)
    expect_equal(nrow(dm), 1)
    expect_equal(ncol(dm), 3)
  })
})

describe("compute_distance_matrix geodesic", {
  it("dispatches to geodesic when method specified", {
    skip_if_not_installed("igraph")
    vertices <- matrix(c(
      0, 0, 0,
      1, 0, 0,
      0, 1, 0
    ), ncol = 3, byrow = TRUE)
    faces <- matrix(c(1, 2, 3), ncol = 3)

    dm <- compute_distance_matrix(vertices, method = "geodesic", faces = faces)
    expect_equal(nrow(dm), 3)
  })

  it("errors without faces for geodesic", {
    vertices <- matrix(rnorm(9), ncol = 3)
    expect_error(
      compute_distance_matrix(vertices, method = "geodesic"),
      "faces"
    )
  })
})
