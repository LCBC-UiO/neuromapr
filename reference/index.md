# Package index

## Annotation Registry

Browse and download brain map annotations from the
[neuromaps](https://github.com/netneurolab/neuromaps) OSF repository.

- [`neuromaps_available()`](https://lcbc-uio.github.io/neuromapr/reference/neuromaps_available.md)
  : List available neuromaps annotations
- [`fetch_neuromaps_annotation()`](https://lcbc-uio.github.io/neuromapr/reference/fetch_neuromaps_annotation.md)
  : Download a neuromaps annotation

## Map Comparison

Statistical comparison of brain maps using spatial null models.

- [`compare_maps()`](https://lcbc-uio.github.io/neuromapr/reference/compare_maps.md)
  : Compare brain maps with spatial null model significance testing
- [`permtest_metric()`](https://lcbc-uio.github.io/neuromapr/reference/permtest_metric.md)
  : Permutation test for any metric between brain maps

## Null Model Generation

Generate spatially-constrained null distributions for brain maps.
[`generate_nulls()`](https://lcbc-uio.github.io/neuromapr/reference/generate_nulls.md)
is the main entry point; individual methods are also available directly.

- [`generate_nulls()`](https://lcbc-uio.github.io/neuromapr/reference/generate_nulls.md)
  : Generate null distributions for brain map data
- [`null_alexander_bloch()`](https://lcbc-uio.github.io/neuromapr/reference/null_alexander_bloch.md)
  : Alexander-Bloch spin test null model
- [`null_baum()`](https://lcbc-uio.github.io/neuromapr/reference/null_baum.md)
  : Baum spin test null model
- [`null_burt2018()`](https://lcbc-uio.github.io/neuromapr/reference/null_burt2018.md)
  : Burt 2018 spatial autoregressive null model
- [`null_burt2020()`](https://lcbc-uio.github.io/neuromapr/reference/null_burt2020.md)
  : Variogram-matching null model
- [`null_cornblath()`](https://lcbc-uio.github.io/neuromapr/reference/null_cornblath.md)
  : Cornblath spin test null model
- [`new_null_distribution()`](https://lcbc-uio.github.io/neuromapr/reference/null_distribution.md)
  : Create a null distribution object
- [`null_moran()`](https://lcbc-uio.github.io/neuromapr/reference/null_moran.md)
  : Moran spectral randomization null model
- [`null_spin_vasa()`](https://lcbc-uio.github.io/neuromapr/reference/null_spin_vasa.md)
  [`null_spin_hungarian()`](https://lcbc-uio.github.io/neuromapr/reference/null_spin_vasa.md)
  : Spin-test null models for brain maps

## Parcellation

Convert between vertex-level and parcel-level representations.

- [`parcellate()`](https://lcbc-uio.github.io/neuromapr/reference/parcellate.md)
  : Parcellate brain map data
- [`unparcellate()`](https://lcbc-uio.github.io/neuromapr/reference/unparcellate.md)
  : Unparcellate brain map data
- [`vertices_to_parcels()`](https://lcbc-uio.github.io/neuromapr/reference/vertices_to_parcels.md)
  : Aggregate vertex data into parcels
- [`parcels_to_vertices()`](https://lcbc-uio.github.io/neuromapr/reference/parcels_to_vertices.md)
  : Map parcel data back to vertices
- [`get_parcel_centroids()`](https://lcbc-uio.github.io/neuromapr/reference/get_parcel_centroids.md)
  : Compute parcel centroids

## Surface Geometry

Compute geodesic distances and other surface properties.

- [`make_surf_graph()`](https://lcbc-uio.github.io/neuromapr/reference/make_surf_graph.md)
  : Build an igraph from a triangular surface mesh
- [`get_surface_distance()`](https://lcbc-uio.github.io/neuromapr/reference/get_surface_distance.md)
  : Compute geodesic distances on a surface mesh
- [`vertex_areas()`](https://lcbc-uio.github.io/neuromapr/reference/vertex_areas.md)
  : Compute per-vertex surface areas

## Transforms and Resampling

Transform brain maps between coordinate spaces using Connectome
Workbench.

- [`transform_to_space()`](https://lcbc-uio.github.io/neuromapr/reference/transform_to_space.md)
  : Transform brain maps between coordinate spaces
- [`resample_images()`](https://lcbc-uio.github.io/neuromapr/reference/resample_images.md)
  : Resample brain maps for comparison
- [`check_wb_command()`](https://lcbc-uio.github.io/neuromapr/reference/check_wb_command.md)
  : Check for Connectome Workbench

## File I/O and Conversion

Read brain maps from various neuroimaging formats and convert between
them.

- [`read_brain_map_values()`](https://lcbc-uio.github.io/neuromapr/reference/read_brain_map_values.md)
  : Read vertex/voxel values from a brain map file
- [`annot_to_gifti()`](https://lcbc-uio.github.io/neuromapr/reference/annot_to_gifti.md)
  : Convert FreeSurfer annotation to GIFTI
- [`fsmorph_to_gifti()`](https://lcbc-uio.github.io/neuromapr/reference/fsmorph_to_gifti.md)
  : Convert FreeSurfer morphometry to GIFTI
