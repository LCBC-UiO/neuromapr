describe("vertices_to_parcels", {
  it("aggregates by labels", {
    data <- c(1, 2, 3, 4, 5, 6)
    labels <- c(1, 1, 2, 2, 3, 3)
    result <- vertices_to_parcels(data, labels)
    expect_equal(unname(result), c(1.5, 3.5, 5.5))
  })

  it("excludes medial wall (0 and NA)", {
    data <- c(1, 2, 3, 4)
    labels <- c(0, 1, NA, 1)
    result <- vertices_to_parcels(data, labels)
    expect_equal(length(result), 1)
    expect_equal(unname(result), 3)
  })

  it("errors for length mismatch", {
    expect_error(
      vertices_to_parcels(1:3, 1:4),
      "same length"
    )
  })

  it("accepts custom summary function", {
    data <- c(1, 2, 3, 4)
    labels <- c(1, 1, 2, 2)
    result <- vertices_to_parcels(data, labels, summary_func = max)
    expect_equal(unname(result), c(2, 4))
  })
})

describe("parcels_to_vertices", {
  it("maps parcel values to vertex positions", {
    parcel_data <- c("1" = 10, "2" = 20)
    labels <- c(1, 1, 2, 2, 0)
    result <- parcels_to_vertices(parcel_data, labels)
    expect_equal(result, c(10, 10, 20, 20, NA_real_))
  })

  it("fills medial wall with custom value", {
    parcel_data <- c("1" = 10)
    labels <- c(1, 0)
    result <- parcels_to_vertices(parcel_data, labels, fill = -1)
    expect_equal(result, c(10, -1))
  })
})

describe("parcellate / unparcellate roundtrip", {
  it("roundtrips through parcellate and unparcellate", {
    data <- c(1, 2, 3, 4, 5, 6)
    labels <- c(1, 1, 2, 2, 3, 3)
    parcelled <- parcellate(data, labels)
    unparcelled <- unparcellate(parcelled, labels)
    expect_equal(unparcelled, c(1.5, 1.5, 3.5, 3.5, 5.5, 5.5))
  })
})

describe("get_parcel_centroids", {
  it("computes average centroids", {
    vertices <- matrix(c(
      0, 0, 0,
      2, 0, 0,
      0, 2, 0,
      2, 2, 0
    ), ncol = 3, byrow = TRUE)
    labels <- c(1, 1, 2, 2)

    centroids <- get_parcel_centroids(vertices, labels, method = "average")
    expect_equal(nrow(centroids), 2)
    expect_equal(centroids[1, ], c(1, 0, 0))
    expect_equal(centroids[2, ], c(1, 2, 0))
  })

  it("computes surface centroids", {
    vertices <- matrix(c(
      0, 0, 0,
      2, 0, 0,
      10, 0, 0
    ), ncol = 3, byrow = TRUE)
    labels <- c(1, 1, 1)

    centroids <- get_parcel_centroids(vertices, labels, method = "surface")
    expect_equal(nrow(centroids), 1)
    expect_equal(centroids[1, ], c(2, 0, 0))
  })

  it("errors for geodesic without faces", {
    vertices <- matrix(rnorm(9), ncol = 3)
    labels <- c(1, 1, 1)
    expect_error(
      get_parcel_centroids(vertices, labels, method = "geodesic"),
      "faces"
    )
  })
})

describe("validate_parcellation", {
  it("errors for wrong length", {
    expect_error(validate_parcellation(1:3, 5), "length")
  })

  it("errors for non-numeric", {
    expect_error(validate_parcellation(letters[1:3], 3), "integer")
  })
})
