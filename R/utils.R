# check_input
# ==============================================================================
# A function for checking some inputs for all functions in remap.
# Input:
#   data - 'data' from remap(), redist(), or predict.remap().
#   regions - 'regions' from remap() or redist(). Can be empty.
#   distances - 'distances' from remap() or predict.remap(). Can be empty.
# Output:
#   NULL or stop message if there is an problem.
# ==============================================================================
check_input <- function(data, cores, regions, distances) {
  if (!methods::is(data, "sf")) stop("data must be class 'sf', see sf package.")
  if (!all(sf::st_geometry_type(data) == "POINT")) {
    stop("'data' must have point geometry.")
  }

  if (cores < 1) {
    stop("'cores' must be a number greater than 0.")
  }

  if (!missing(regions)) {
    if (!methods::is(regions, "sf")) {
      stop("regions must be class 'sf', see sf package.")
    }
    if (!all(sf::st_geometry_type(regions) %in% c("POLYGON", "MULTIPOLYGON"))) {
      stop("'regions' must have polygon or multipolygon geometry.")
    }
    if (sf::st_crs(data) != sf::st_crs(regions)) {
      stop("data and regions must have the same CRS.",
           " See sf::st_transform() for help.")
    }
    if (nrow(regions) == 0) {
      stop("regions must have at least 1 row.")
    }
  }
  # TODO: what if data has one row
  if (!missing(distances) && nrow(data) != nrow(distances)) {
    stop("Rows in data must be same length as row in distances.")
  }
}



# process_regions
# ==============================================================================
# A function for processing 'regions' input to remap() or redist().
# Input:
#   regions - 'regions' from remap() or redist().
#   region_id - 'region_id' column of 'regions' from remap() or redist(). Can
#     be empty.
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
      new_geom[[id]] <- Reduce(
        sf::st_union,
        sf::st_geometry(regions[regions[[region_id]] == id,])
      )
    }
    regions <- sf::st_sf(geom = sf::st_sfc(new_geom), crs = sf::st_crs(regions))
    regions[[region_id]] <- id_list
  }

  return(regions[region_id])
}



# process_numbers
# ==============================================================================
# A function for processing 'smooth' or 'buffer' or input to remap().
# Input:
#   x - 'smooth' or 'buffer' values
#   name - One of 'smooth' or 'buffer'
#   region_id - 'region_id' column of 'regions' from remap().
# Output:
#   An object with values of x and length id_list that has id_list names.
# ==============================================================================
process_numbers <- function(x, name, id_list) {

  if (missing(x) || anyNA(x) ||
      !is.numeric(x) || any(as.numeric(x) < 0)) {
    stop(name, " must be a number >= 0.")
  }

  if (length(x) == 1) {
    x <- rep(x, length(id_list))
    names(x) <- id_list
  } else if (all(id_list == 1:length(x))) {
    names(x) <- id_list
  } else if (!all(names(x) %in% id_list)) {
    stop(name, " values must have names equal to unique values",
         " in the 'region_id' column of 'regions'.")
  }

  x <- x[id_list]
  units(x) <- units::as_units("km")
  return(x)
}




