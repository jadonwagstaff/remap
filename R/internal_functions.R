
# process_regions
# ==============================================================================
# A function for processing 'regions' input to remap() or redist()
# Input:
#   regions - 'regions' from remap() or redist()
#   region_id - 'region_id' column of 'regions' from remap() or redist()
#     (can be missing)
# Output:
#   A replacement 'regions' object that has unique 'region_id' values in the
#   first column and sf geometries in the second column.
# ==============================================================================
process_regions <- function(regions, region_id) {

  # Make an ID list
  # ============================================================================
  if (missing(region_id)) {
    region_id <- "region"
    id_list <- 1:nrow(regions)
    regions[[region_id]] <- id_list
  } else {
    if (!region_id %in% colnames(regions)) {
      stop("'region_id' must be a column name of 'regions'")
    }
    id_list <- unique(as.character(regions[[region_id]]))
  }

  # Remove all data from regions except for geometry and region_id
  # ============================================================================
  # If there is more than one region for each unique region_id then combine the
  # geometries for each uniqe region_id
  if (length(id_list) < nrow(regions)) {
    new_geom <- list()
    for (id in id_list) {
      new_geom[[id]] <- Reduce(sf::st_union,
                               sf::st_geometry(regions[regions[[region_id]] == id,]))
    }
    regions <- sf::st_sf(geom = sf::st_sfc(new_geom), crs = sf::st_crs(regions))
    regions[[region_id]] <- id_list
  }

  return(regions[c(region_id, "geom")])
}
