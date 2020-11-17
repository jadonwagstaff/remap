#' Get distances between data and regions.
#'
#' Finds distances in km between data provided as sf dataframe with point geometry
#' and regions provided as sf dataframe with polygon or multipolygon geometry.
#'
#'
#' @param data An sf data frame with point geometry.
#' @param regions An sf dataframe with polygon or multipolygon geometry.
#' @param region_id Optional name of column in 'regions' that contains the id
#' that each region belongs to (no quotes). If null, it will be assumed that
#' each row is its own region.
#' @param max_dist a maximum distance that is needed for future calculations.
#' (Set equal to maximum 'smooth' when predicting on new observations.)
#' @param cores Number of cores for parallel computing. 'cores' above
#' default of 1 will require more memory.
#' @param progress If true, a text progress bar is printed to the console. Progress
#' set to FALSE will find distances quicker if max_dist is not specified.
#'
#' @return A matrix where each row corresponds one-to-one with each row in
#' provided 'data'. Matrix columns are either named with regions from 'region_id'
#' column of 'regions' or the row numbers of 'regions' if 'region_id' is NULL.
#' Values are in kilometers.
#'
#' @seealso
#'   \code{\link{remap}} - uses redist for regional models.
#'
#'
#' @export
redist <- function(data, regions, region_id, max_dist, cores = 1,
                   progress = FALSE) {
  # Check input
  # ============================================================================
  check_input(data = data, cores = cores, regions = regions)

  # check if region_id is a character, if it is not, make it a character
  if (!missing(region_id) &&
      !tryCatch(is.character(region_id), error = function(e) FALSE)) {
    region_id <- deparse(substitute(region_id))
  }

  # process regions so only one line makes up a region
  regions <- process_regions(regions, region_id)
  id_list <- regions[[1]]

  # decide which helper distance function to use
  dist_fun <- ifelse(cores > 1, multi_core_dist, single_core_dist)


  # Check max_dist
  # ============================================================================
  if (!missing(max_dist)) {
    max_dist <- process_numbers(max_dist, "max_dist", id_list)

    # if data is longlat, change max_dist from km to degrees
    if (sf::st_is_longlat(data)) {
      max_lat <- max(abs(sf::st_coordinates(data)[, "Y"]))
      # using radius of earth at a pole for WGS-84 ellipsoid (rounded down)
      km_per_deg <- ((pi * 6350) / 180) * cos((pi * max_lat) / 180)
      # convert km to degrees (using conservative distance at max latitude)
      max_dist <- as.numeric(max_dist) / km_per_deg
      names(max_dist) <- id_list
      # set units to degrees and names
      units(max_dist) <- with(units::ud_units, "degrees")
      # check to make sure  the distance isn't too large
      if (as.numeric(max(max_dist)) + max_lat > 90) {
        warning("At least one 'data' point is too close to a pole for the",
                "requested 'max_dist'. Reverting back to finding all",
                "distances.")
        max_dist <- NULL
      }
    }
  }


  # Find distances between the data and each region
  # ============================================================================
  if (missing(max_dist) || is.null(max_dist)) {
    if (progress) cat("Finding regional distances...\n")

    distances <- dist_fun(points = data,
                          polygons = regions,
                          cores = cores,
                          progress = progress)
  } else {
    if (progress) cat("Using buffer to find nearest values...\n")

    # find buffer for regions based on max_dist
    regions_buffer <- suppressWarnings(
      sf::st_buffer(regions, max_dist)
    )

    # use buffer to find points close enough to a region to calculate distances
    # used alply rather than apply since alply will always return a list
    buffer_indices <- plyr::alply(
      suppressMessages(sf::st_within(data, regions_buffer, sparse = FALSE)),
      2, which
    )


    if (progress) cat("Finding regional distances...\n")

    distances <- dist_fun(points = data,
                          polygons = regions,
                          index = buffer_indices,
                          cores = cores,
                          progress = progress)

    # find all distances in places where all regions are out of buffer
    too_far <- apply(distances, 1, function(x) all(is.na(x)))

    distances[too_far, ] <- dist_fun(points = data[too_far, ],
                                     polygons = regions,
                                     cores = cores,
                                     progress = progress)

  }

  # Add names of regions to distances
  colnames(distances) <- id_list

  return(distances)
}


# single_core_dist
# ==============================================================================
# A function that finds the distance in km between points and polygons.
# Input:
#   points - an sf object containing points.
#   polygons - an sf or sfc object containing polygons or multipolygons.
#   index - an optional list of indices corresponding to which distances
#     need to be found for each point and polygon.
#   progress - whether to show a progress bar.
# Output:
#   A matrix of distances in km where the ith column is the distance between
#   the points and the ith polygon.
# ==============================================================================
single_core_dist <- function(points, polygons, index, progress, ...) {

  polygons <- sf::st_geometry(polygons)

  # Procedure with no progress and no index matrix
  # ============================================================================
  if (!progress && missing(index)) {
    # simply find distances if there is no progress bar or index matrix
    d <- distance_wrapper(points, polygons)

  # Procedure when called with index matrix or progress updates
  # ============================================================================
  } else {
    # allocate distances
    d <- matrix(as.numeric(NA),
                nrow = nrow(points),
                ncol = length(polygons))

    # add progress bar
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(polygons), style = 3)
    }

    # compute distances by column
    for (i in 1:length(polygons)) {

      # add to distance matrix
      if (missing(index)) {
        d[, i] <- distance_wrapper(points, polygons[i])
      } else {
        d[index[[i]], i] <- distance_wrapper(points[index[[i]], ], polygons[i])
      }

      # update progress
      if (progress) utils::setTxtProgressBar(pb, i)
    }

    # add units to entire matrix
    units(d) <- with(units::ud_units, km)
  }

  if (progress) cat("\n")

  return(d)
}



# multi_core_dist
# ==============================================================================
# A function that finds the distance in km between points and polygons.
# Input:
#   points - an sf object containing points.
#   polygons - an sf or sfc object containing polygons or multipolygons.
#   index - an optional list of indices corresponding to which distances
#     need to be found for each point and polygon.
#   cores - number of cores for parallel computing.
# Output:
#   A matrix of distances in km where the ith column is the distance between
#   the points and the ith polygon.
# ==============================================================================
multi_core_dist <- function(points, polygons, index, cores, ...) {

  polygons <- sf::st_geometry(polygons)

  # set up parallel computing
  clusters <- parallel::makeCluster(cores)

  parallel::clusterExport(cl = clusters,
                          varlist = "distance_wrapper",
                          envir = environment())

  # Compute distances by polygon with no index
  # ============================================================================
  if (missing(index)) {
    d <- parallel::parLapply(
      cl = clusters,
      X = as.list(1:length(polygons)),
      fun = function(x) distance_wrapper(points, polygons[x])
    )
  # Compute distances by polygon with index
  # ============================================================================
  } else {
    d <- parallel::parLapply(
      cl = clusters,
      X = 1:length(polygons),
      fun = function(x) {
        col <- rep(as.numeric(NA), nrow(points))
        col[index[[x]]] <- distance_wrapper(points[index[[x]], ], polygons[x])
        return(col)
      }
    )
  }

  # stop parallel process and turn distances into matrix
  parallel::stopCluster(clusters)

  d <- matrix(unlist(d), ncol = length(polygons))
  units(d) <- with(units::ud_units, km)

  return(d)
}



# distance_wrapper
# ==============================================================================
# A function that finds the distance in km between points and polygons.
# Input:
#   points - an sf object containing points.
#   polygons - an sf or sfc object containing polygons or multipolygons.
# Output:
#   A matrix of distances in km where the ith column is the distance between
#   the points and the ith polygon.
# ==============================================================================
distance_wrapper <- function(points, polygons) {
  # find distances
  d <- sf::st_distance(points, polygons)

  # convert to km
  units(d) <- tryCatch({
    with(units::ud_units, km)
  },
  error = function(e) {
    stop("Distances returned by sf::st_distance is not a unit convertible",
         "to kilometers. Try transforming 'data' and 'regions' object",
         "using sf::st_transform.")
  })

  return(d)
}


