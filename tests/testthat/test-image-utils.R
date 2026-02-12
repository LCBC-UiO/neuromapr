describe("vertex_areas", {
  it("computes areas for a single triangle", {
    vertices <- matrix(c(
      0, 0, 0,
      1, 0, 0,
      0, 1, 0
    ), ncol = 3, byrow = TRUE)
    faces <- matrix(c(1, 2, 3), ncol = 3)

    areas <- vertex_areas(vertices, faces)
    expect_equal(length(areas), 3)
    expect_equal(sum(areas), 0.5)
    expect_true(all(areas > 0))
    expect_equal(areas[1], areas[2])
    expect_equal(areas[2], areas[3])
  })

  it("distributes area equally across face vertices", {
    vertices <- matrix(c(
      0, 0, 0,
      2, 0, 0,
      0, 2, 0
    ), ncol = 3, byrow = TRUE)
    faces <- matrix(c(1, 2, 3), ncol = 3)

    areas <- vertex_areas(vertices, faces)
    expect_equal(areas, rep(2 / 3, 3), tolerance = 1e-10)
  })

  it("accumulates from multiple faces", {
    vertices <- matrix(c(
      0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      1, 1, 0
    ), ncol = 3, byrow = TRUE)
    faces <- matrix(c(1, 2, 3, 2, 3, 4), ncol = 3, byrow = TRUE)

    areas <- vertex_areas(vertices, faces)
    expect_equal(sum(areas), 1.0, tolerance = 1e-10)
    expect_true(areas[2] > areas[1])
    expect_true(areas[3] > areas[4])
  })

  it("errors for invalid vertices", {
    expect_error(vertex_areas(matrix(1:4, ncol = 2), matrix(1:3, ncol = 3)), "3 columns")
  })

  it("errors for invalid faces", {
    expect_error(vertex_areas(matrix(1:6, ncol = 3), matrix(1:4, ncol = 2)), "3 columns")
  })
})

describe("annot_to_gifti", {
  it("errors for missing file", {
    skip_if_not_installed("freesurferformats")
    skip_if_not_installed("gifti")
    expect_error(annot_to_gifti("nonexistent.annot"), "not found")
  })
})

describe("fsmorph_to_gifti", {
  it("errors for missing file", {
    skip_if_not_installed("freesurferformats")
    skip_if_not_installed("gifti")
    expect_error(fsmorph_to_gifti("nonexistent.curv"), "not found")
  })
})
