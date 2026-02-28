# neuromapr 0.2.1

* CRAN acceptance release.
* Improved efficiency and vectorisation across null model methods.
* Added examples to all exported functions.
* Bug fixes across null models and infrastructure from code review.
* Added vignettes for rotation methods.

# neuromapr 0.2.0

* Initial CRAN submission.
* Eight spatial null model methods: variogram-matching (`burt2020`),
  SAR model (`burt2018`), Moran spectral randomization (`moran`),
  spin-based permutation (`alexander_bloch`, `spin_vasa`, `spin_hungarian`),
  and parcel spin (`baum`, `cornblath`).
* `compare_maps()` for brain map comparison with optional spatial null testing.
* `generate_nulls()` dispatcher for all null model methods.
* Parcellation utilities: `parcellate()`, `unparcellate()`,
  `vertices_to_parcels()`, `parcels_to_vertices()`, `get_parcel_centroids()`.
* Geodesic surface distance via `make_surf_graph()` and
  `get_surface_distance()`.
* `permtest_metric()` for custom metric permutation testing.
* Neuromaps annotation registry interface: `neuromaps_available()` and
  `fetch_neuromaps_annotation()`.
* Format conversions: `annot_to_gifti()`, `fsmorph_to_gifti()`.
* Coordinate-space transforms via `transform_to_space()` and
  `resample_images()` (requires 'ciftiTools').
