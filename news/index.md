# Changelog

## neuromapr 0.2.1

CRAN release: 2026-02-27

- CRAN acceptance release.
- Improved efficiency and vectorisation across null model methods.
- Added examples to all exported functions.
- Bug fixes across null models and infrastructure from code review.
- Added vignettes for rotation methods.

## neuromapr 0.2.0

- Initial CRAN submission.
- Eight spatial null model methods: variogram-matching (`burt2020`), SAR
  model (`burt2018`), Moran spectral randomization (`moran`), spin-based
  permutation (`alexander_bloch`, `spin_vasa`, `spin_hungarian`), and
  parcel spin (`baum`, `cornblath`).
- [`compare_maps()`](https://lcbc-uio.github.io/neuromapr/reference/compare_maps.md)
  for brain map comparison with optional spatial null testing.
- [`generate_nulls()`](https://lcbc-uio.github.io/neuromapr/reference/generate_nulls.md)
  dispatcher for all null model methods.
- Parcellation utilities:
  [`parcellate()`](https://lcbc-uio.github.io/neuromapr/reference/parcellate.md),
  [`unparcellate()`](https://lcbc-uio.github.io/neuromapr/reference/unparcellate.md),
  [`vertices_to_parcels()`](https://lcbc-uio.github.io/neuromapr/reference/vertices_to_parcels.md),
  [`parcels_to_vertices()`](https://lcbc-uio.github.io/neuromapr/reference/parcels_to_vertices.md),
  [`get_parcel_centroids()`](https://lcbc-uio.github.io/neuromapr/reference/get_parcel_centroids.md).
- Geodesic surface distance via
  [`make_surf_graph()`](https://lcbc-uio.github.io/neuromapr/reference/make_surf_graph.md)
  and
  [`get_surface_distance()`](https://lcbc-uio.github.io/neuromapr/reference/get_surface_distance.md).
- [`permtest_metric()`](https://lcbc-uio.github.io/neuromapr/reference/permtest_metric.md)
  for custom metric permutation testing.
- Neuromaps annotation registry interface:
  [`neuromaps_available()`](https://lcbc-uio.github.io/neuromapr/reference/neuromaps_available.md)
  and
  [`fetch_neuromaps_annotation()`](https://lcbc-uio.github.io/neuromapr/reference/fetch_neuromaps_annotation.md).
- Format conversions:
  [`annot_to_gifti()`](https://lcbc-uio.github.io/neuromapr/reference/annot_to_gifti.md),
  [`fsmorph_to_gifti()`](https://lcbc-uio.github.io/neuromapr/reference/fsmorph_to_gifti.md).
- Coordinate-space transforms via
  [`transform_to_space()`](https://lcbc-uio.github.io/neuromapr/reference/transform_to_space.md)
  and
  [`resample_images()`](https://lcbc-uio.github.io/neuromapr/reference/resample_images.md)
  (requires ‘ciftiTools’).
