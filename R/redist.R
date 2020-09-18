#' Get distances between data and regions.
#'
#' Finds distances in km between data provided as sf dataframe with point geometry
#' and regions provided as sf dataframe with polygon geometry.
#'
#'
#' @param data An sf data frame with longitude, latitude, and columns required
#' for modeling.
#' @param lon Name of longitude column (no quotes).
#' @param lat Name of latitude column (no quotes).
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
redist <- function(data, lon, lat, regions, max_dist = NULL,
                   region_id = NULL, progress = TRUE) {

  # Check input
  # ============================================================================
  if (!"data.frame" %in% class(data)) stop("data must be class 'data.frame'")
  if (!"sf" %in% class(regions)) stop("regions must be class 'sf'")

  if (!tryCatch(is.character(lon), error = function(e) FALSE)) {
    lon <- deparse(substitute(lon))
  }
  if (!tryCatch(is.character(lat), error = function(e) FALSE)) {
    lat <- deparse(substitute(lat))
  }

  sf_data <- sf::st_as_sf(data,
                          coords = c(lon, lat),
                          crs = sf::st_crs(regions))

  # Create list of regions to check (if region_id is NULL, then each polygon
  #   is a separate region)
  if (missing(region_id)) {
    id_list <- 1:nrow(regions)
  } else {
    if (!tryCatch(is.character(region_id), error = function(e) FALSE)) {
      region_id <- deparse(substitute(region_id))
    }

    id_list <- sort(unique(as.character(regions[[region_id]])))
  }

  # Find distances between the data and each region
  # ============================================================================

  if (is.null(max_dist)) {
    # initialize distance matrix
    distances <- matrix(as.numeric(NA),
                        nrow = nrow(data),
                        ncol = length(id_list),
                        dimnames = list(NULL, id_list))

    # add progress bar
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(id_list), style = 3)
      i <- 1
    }

    for (id in id_list) {
      distances[, id] <- apply(
        sf::st_distance(sf_data, regions[regions[[region_id]] %in% id, ]),
        1,
        min
      )

      # update progress bar
      if (progress) {
        utils::setTxtProgressBar(pb, i)
        i <- i + 1
      }
    }
  } else {
    if (progress) cat("Using buffer to find nearest values...")

    # find points within close to regions
    regions_buffer <- suppressMessages(suppressWarnings(
      sf::st_buffer(regions, max_dist / 65)
    ))

    close <- apply(
      suppressMessages(sf::st_within(sf_data, regions_buffer, sparse = FALSE)),
      2, which
    )

    # initialize distance matrix
    distances <- matrix(as.numeric(NA),
                        nrow = nrow(data),
                        ncol = length(id_list),
                        dimnames = list(NULL, id_list))

    # add progress bar
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(id_list), style = 3)
      i <- 1
    }

    for (id in id_list) {
      id_indices <- regions[[region_id]] %in% id
      id_close <- unique(unlist(close[id_indices]))

      # find distances if it is within buffer
      if (length(id_close) > 0) {
        distances[id_close, id] <- apply(
          sf::st_distance(sf_data[id_close, ], regions[id_indices, ]),
          1,
          min
        )
      }

      if (progress) {
        utils::setTxtProgressBar(pb, i)
        i <- i + 1
      }
    }

    # add distances for points that are not close to any region
    rm(close)
    not_close <- apply(distances, 1, function(x) all(is.na(x)))
    if (sum(not_close) > 0) {
      for (id in id_list) {
        distances[not_close, id] <- apply(
          sf::st_distance(sf_data[not_close, ], regions[regions[[region_id]] %in% id, ]),
          1,
          min
        )
      }
    }
  }

  if (progress) cat("\n")

  return(distances / 1000)
}
