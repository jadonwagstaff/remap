#' Get distances between data and regions.
#'
#' Finds distances in km between data provided as sf dataframe with point geometry
#' and regions provided as sf dataframe with polygon geometry.
#'
#'
#' @param data An sf data frame with point geometry.
#' @param regions An sf dataframe with polygon geometry.
#' @param max_dist a maximum distance that is needed for future calculations.
#' (Set equal to maximum 'smooth' when predicting on new observations.)
#' @param region_id Optional name of column in 'regions' that contains the id
#' that each region belongs to (no quotes). If null, it will be assumed that
#' each polygon is its own region (no regions have more than one polygon).
#' @param progress If true, a text progress bar is printed to the console.
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
redist <- function(data, regions, region_id, max_dist, progress = TRUE) {

  # Check input
  # ============================================================================
  if (!"sf" %in% class(data)) stop("data must be class 'sf'.")
  if (!"sf" %in% class(regions)) stop("regions must be class 'sf'.")
  if (sf::st_crs(data) != sf::st_crs(regions)) {
    stop("data and regions must have the same CRS.",
         "See sf::st_transform() for help.")
  }
  if (!missing(max_dist)) {
    if (!is.numeric(max_dist)) stop("max_dist must be numeric")
    units(max_dist) <- with(units::ud_units, km)
  }

  # Create list of regions to check (if region_id is NULL, then each polygon
  #   is a separate region)
  if (!missing(region_id) &&
      !tryCatch(is.character(region_id), error = function(e) FALSE)) {
    region_id <- deparse(substitute(region_id))
  }
  regions <- process_regions(regions, region_id)

  # Find distances between the data and each region
  # ============================================================================
  if (missing(max_dist)) {
    distances <- distance_wrapper(data, regions, progress)
  } else {
    if (progress) cat("Using bounding boxes to find nearest values...\n")

    # find bounding boxes
    bboxes <- sapply(
      sf::st_geometry(regions),
      function(x) sf::st_as_sfc(sf::st_bbox(x))
    )
    bboxes <- sf::st_as_sfc(bboxes,  crs = sf::st_crs(regions))

    # use distance to bounding box to decide whether to find distance to regions
    bbox_index <- distance_wrapper(data, regions, progress) <= max_dist

    # for any point not within max_dist of any bounding box, find all distances
    bbox_index[!apply(bbox_index, 1, any),] <- TRUE

    # initialize distance matrix
    distances <- matrix(as.numeric(NA),
                        nrow = nrow(bbox_index),
                        ncol = ncol(bbox_index))
    units(distances) <- with(units::ud_units, km)

    # add progress bar
    if (progress) {
      cat("\nFinding distances within max_dist of bounding boxes...\n")
      pb <- utils::txtProgressBar(min = 0, max = ncol(bbox_index), style = 3)
    }

    # find distances if it is within the max_dist of the bbox
    for (i in 1:ncol(bbox_index)) {
      if (any(bbox_index[, i])) {
        distances[bbox_index[, i], i] <- distance_wrapper(data[bbox_index[, i], ],
                                                          regions[i, ],
                                                          FALSE)
      }

      if (progress) utils::setTxtProgressBar(pb, i)
    }
  }

  colnames(distances) <- regions[[1]]

  if (progress) cat("\n")

  return(distances)
}


distance_wrapper <- function(points, polygons, progress) {

  polygons <- sf::st_geometry(polygons)

  if (!progress) {
    # simply find distances if there is no progress bar
    d <- sf::st_distance(points, polygons)
    units(d) <- with(units::ud_units, km)

  } else {
    # allocate distances
    d <- matrix(as.numeric(NA),
                nrow = nrow(points),
                ncol = length(polygons))

    # add progress bar
    pb <- utils::txtProgressBar(min = 0, max = length(polygons), style = 3)

    # compute distances by column
    for (i in 1:length(polygons)) {
      col <- sf::st_distance(points, polygons[i])
      units(col) <- with(units::ud_units, km)
      d[, i] <- col

      # update progress
      utils::setTxtProgressBar(pb, i)
    }

    # add units to entire matrix
    units(d) <- with(units::ud_units, km)
  }

  return(d)
}





