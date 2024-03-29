#' Build separate models for mapping multiple regions.
#'
#' Separate models are built for each given region and combined into one S3
#' object that can be used to predict on new data using generic function
#' predict().
#'
#' If a model fails for a region, a warning is given but the modeling
#' process will continue.
#'
#' A description of the methodology can be found in Wagstaff and Bean (2023)
#' "remap: Regionalized Models with Spatially Smooth Predictions"
#' <doi:10.32614/RJ-2023-004>.
#'
#'
#' @param data An sf data frame with point geometry.
#' @param regions An sf dataframe with polygon or multipolygon geometry.
#' @param region_id Optional name of column in 'regions' that contains the id
#' that each region belongs to (no quotes). If null, it will be assumed that
#' each row of 'regions' is its own region.
#' @param model_function A function that can take a subset of 'data' and
#' output a model that can be used to predict new values when passed to generic
#' function predict().
#' @param buffer The length of the buffer zone around each region in km where
#' observations are included in the data used to build models for each region.
#' (Can be a named vector with different values for each unique 'region_id' in
#' 'region'.)
#' @param min_n The minimum number of observations to use when building a model.
#' If there are not enough observations in the region and buffer, then the
#' closest min_n observations are used. \code{min_n} must be at least 1.
#' @param distances An optional matrix of distances between 'data' and 'regions'
#' generated by \code{redist()} function (calculated internally if not
#' provided). Note that unless you know that you have min_n within a certain
#' distance, no max_dist parameter should be used in \code{redist()}.
#' @param cores Number of cores for parallel computing. 'cores' above
#' default of 1 will require more memory.
#' @param progress If true, a text progress bar is printed to the console.
#' (Progress bar only appears if 'cores' = 1.)
#' @param ... Extra arguments to pass to 'model_function' function.
#'
#' @return A \emph{remap} S3 object containing:
#' \describe{
#'   \item{\emph{models}}{A list of models containing a model output by
#'   'model_function' for each region.}
#'   \item{\emph{regions}}{'regions' object passed to the function (used for
#'   prediction). The first column is 'region_id' or the row number of 'regions'
#'   if 'region_id is missing. The second column is the region geometry.}
#'   \item{\emph{call}}{Shows the parameters that were passed to the function.}
#' }
#'
#' @seealso
#'   \code{\link{predict.remap}} - used for predicting on new data.
#'   \code{\link{redist}} - used for pre-computing distances.
#'
#' @examples
#' library(remap)
#' data(utsnow)
#' data(utws)
#'
#' # We will keep these examples simple by only modeling non-zero values of
#' # snow water equivalent (WESD)
#'
#' utsnz <- utsnow[utsnow$WESD > 0, ]
#'
#' # Build a remap model with lm that has formula WESD ~ ELEVATION
#' # The buffer to collect data around each region is 30km
#' # The minimum number of observations per region is 10
#' remap_model <- remap(data = utsnz,
#'                      regions = utws,
#'                      region_id = HUC2,
#'                      model_function = lm,
#'                      formula = log(WESD) ~ ELEVATION,
#'                      buffer = 20,
#'                      min_n = 10,
#'                      progress = TRUE)
#'
#' # Resubstitution predictions
#' remap_preds <- exp(predict(remap_model, utsnz, smooth = 10))
#' head(remap_preds)
#'
#' @export
remap <- function(data, regions, region_id, model_function, buffer, min_n = 1,
                  distances, cores = 1, progress = FALSE,  ...) {
  # Check input
  # ============================================================================
  check_input(data = data,
              cores = cores,
              regions = regions,
              distances = distances)

  # check if region_id is a character, if it is not, make it a character
  if (!missing(region_id) &&
      !tryCatch(is.character(region_id), error = function(e) FALSE)) {
    region_id <- deparse(substitute(region_id))
  }

  # process regions so only one line makes up a region
  regions <- process_regions(regions, region_id)
  region_id <- names(regions)[[1]]
  id_list <- as.character(regions[[1]])

  # check numbers
  buffer <- process_numbers(x = buffer, name = "buffer", id_list = id_list)
  if (min_n < 1) stop("'min_n' needs to be an integer >= 1.")

  # Find distances between the data and each region
  # ============================================================================
  if (missing(distances)) {
    distances <- redist(data,
                        regions = regions,
                        region_id = region_id,
                        cores = cores,
                        progress = progress)
  }

  # Make function for building models
  # ============================================================================
  make_model <- function(id) {
    # get indices for building model
    indices <- which(distances[, id] <= buffer[[id]])
    if (length(indices) < min_n) {
      indices <- order(distances[, id])[1:min_n]
    }

    # correct data if running in parallel
    if (cores > 1) {
      newdata <- parallel_hack(data[indices, ], sf::st_crs(data))
    } else {
      newdata <- data[indices, ]
    }

    # try building a model and warn if one fails
    tryCatch({
      model_function(newdata, ...)
    },
    error = function(e) {
      warning("Error in model for region ", id, ":\n", e)
      NULL
    })
  }

  # Find models for each region id (parallel)
  # ============================================================================
  if (progress) cat("Building models...\n")
  if (cores > 1) {

    clusters <- parallel::makeCluster(cores)

    parallel::clusterExport(clusters,
                            "model_function",
                            envir = environment())

    models <- parallel::parLapply(
      cl = clusters,
      X = as.list(id_list),
      fun = make_model
    )

    names(models) <- id_list

    parallel::stopCluster(clusters)

    # Find models for each region id (single core)
    # ============================================================================
  } else {
    models <- list()

    # add progress bar
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(id_list), style = 3)
      i <- 1
    }

    for (id in id_list) {
      models[[id]] <- make_model(id)

      # update progress bar
      if (progress) {
        utils::setTxtProgressBar(pb, i)
        i <- i + 1
      }
    }

    if (progress) cat("\n")
  }

  # Create output
  # ============================================================================
  if (length(models) == 0) stop("Modeling function failed in every region.")

  # remove regions where model failed
  models <- models[sapply(models, function(x) !is.null(x))]
  id_list <- id_list[id_list %in% names(models)]
  regions <- regions[regions[[region_id]] %in% id_list, ]

  output <- list(models = models, regions = regions, call = match.call())

  class(output) <- "remap"
  return(output)
}





#` \emph{remap} prediction function.
#'
#' Make predictions given a set of data and smooths predictions at region borders.
#' If an observation is outside of all regions and smoothing distances, the
#' closest region will be used to predict.
#'
#' @param object \emph{} S3 object output from remap.
#' @param data An sf dataframe with point geometry.
#' @param smooth The distance in km within a region where a smooth transition
#' to the next region starts. If smooth = 0, no smoothing occurs between regions
#' unless an observation falls on the border of two or more polygons. (Can be a
#' named vector with different values for each unique object$region_id' in '
#' object$region'.)
#' @param distances An optional matrix of distances between 'data' and
#' 'object$regions' generated by \code{redist()} function (calculated
#' internally if not provided).
#' @param cores Number of cores for parallel computing. 'cores' above
#' default of 1 will require more memory.
#' @param progress If TRUE, a text progress bar is printed to the console.
#' (Progress bar only appears if 'cores' = 1.)
#' @param se If TRUE, predicted values are assumed to be standard errors and
#' an upper bound of combined model standard errors are calculated at each
#' prediction location. Should stay FALSE unless predicted values from remap are
#' standard error values.
#' @param ... Arguments to pass to individual model prediction functions.
#'
#' @return Predictions in the form of a numeric vector. If se is TRUE,
#' upper bound for combined standard errors in the form of a numeric vector.
#'
#' @seealso \code{\link{remap}} building a regional model.
#'
#' @export
predict.remap <- function(object, data, smooth, distances, cores = 1,
                          progress = FALSE, se = FALSE, ...) {
  # Check input
  # ============================================================================
  check_input(data = data, cores = cores, distances = distances)
  id_list <- names(object$models)

  # check smooth
  smooth <- process_numbers(smooth, "smooth", id_list)

  # Find distances between the data and each region
  # ============================================================================
  if (missing(distances)) {
    distances <- redist(data,
                        regions = object$regions[],
                        region_id = names(object$regions)[[1]],
                        max_dist = smooth,
                        cores = cores,
                        progress = progress)
  } else {
    # remove any extra columns in to prevent errors in the next step
    distances <- distances[, id_list]
  }

  # make sure all values have a distance with non-zero weight
  distances[t(apply(distances, 1, function(x) {
    x >= as.numeric(smooth) & x == min(x, na.rm = TRUE) & !is.na(x)
  }))] <- 0

  # Do predictions in parallel if specified
  # ============================================================================
  if (progress) cat("Predicting...\n")
  if (cores > 1) {
    clusters <- parallel::makeCluster(cores)

    parallel::clusterExport(clusters,
                            c(unlist(lapply(search(), function(x) {
                              objects(x, pattern = "predict")
                            }))),
                            envir = environment())

    pred_list <- parallel::parLapply(
      clusters,
      as.list(id_list),
      function(id) {
        indices <- which(distances[, id] <= smooth[[id]] &
                           !is.na(distances[, id]))

        # correct data to run in parallel
        newdata <- parallel_hack(data[indices, ], sf::st_crs(data))

        stats::predict(object$models[[id]], newdata, ...)
      }
    )

    parallel::stopCluster(clusters)

    names(pred_list) <- id_list
  }

  # Make predictions (if single core) and smooth to final output
  # ============================================================================
  output <- rep(0, nrow(data))
  weightsum <- rep(0, nrow(data))
  if (se) wsesum <- rep(0, nrow(data))

  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = length(id_list), style = 3)
    i <- 1
  }

  # get weighted sum
  for (id in id_list) {
    # only consider values within smoothing range
    indices <- distances[, id] <= smooth[[id]] & !is.na(distances[, id])

    # make predictions
    if (cores > 1) {
      preds <- pred_list[[id]]
    } else {
      preds <- stats::predict(object$models[[id]], data[indices, ], ...)
    }

    # update weights
    if (as.numeric(smooth[[id]]) == 0) {
      weight <- rep(1, sum(indices))
    } else {
      weight <- as.numeric(
        ((smooth[[id]] - distances[indices, id]) / smooth[[id]])^2
      )
    }
    weightsum[indices] <- weightsum[indices] + weight

    # update output
    if (se) {
      if (any(preds < 0)) {
        warning(sum(preds < 0), " standard error values less than 0 returned ",
                "for region ", id, ". These values will be assumed to be 0.")
        preds[preds < 0] <- 0
      }
      # https://github.com/jadonwagstaff/remap/blob/main/support_docs/se_algorithm.pdf
      wse <- weight * preds
      output[indices] <- output[indices] + wse * (wse + 2 * wsesum[indices])
      wsesum[indices] <- wsesum[indices] + wse
    } else {
      output[indices] <- output[indices] + weight * preds
    }

    # update progress bar
    if (progress) {
      utils::setTxtProgressBar(pb, i)
      i <- i + 1
    }
  }

  # get weighted average
  if (se) output <- sqrt(output)
  output <- output / weightsum

  # make sure 0 weightsum values are NA
  output[weightsum == 0] <- NA_real_

  if (progress) cat("\n")
  if (se) cat("Upper bound for standard error calculated at each location.",
              "\nReminder: make sure that the predict function outputs",
              "a vector of standard error values for each regional model in",
              "your remap object.\n")

  return(output)
}



#' Print method for remap object.
#'
#' @param x \emph{} S3 object output from remap.
#' @param ... Extra arguments.
#'
#' @return No return value, a description of the remap object is printed in the
#' console.
#'
#' @export
print.remap <- function(x, ...) {
  cat(paste("remap model with",
            length(x$models),
            "regional models\n"))
}



#' Summary method for remap object.
#'
#' @param object \emph{} S3 object output from remap.
#' @param ... Extra arguments to pass to regional models.
#'
#' @return No return value, a brief summary of the remap object is printed in
#' the console. This includes the class(es) of the regional models, the
#' CRS of the regions, and the bounding box of the regions.
#'
#' @export
summary.remap <- function(object, ...) {

  # Get classes
  classes <- unique(lapply(object$models, class))

  # Get bounding box
  bbox <- sf::st_bbox(object$regions)

  cat(paste(
    "Regional models:\n",
    length(object$models), "regional models of class(es)", classes, "\n\n"
  ))
  cat(paste(
    "Regions:\n",
    "Regions have CRS", sf::st_crs(object$regions)$input, "with:\n",
    "xmin =", bbox[1], "\n",
    "ymin =", bbox[2], "\n",
    "xmax =", bbox[3], "\n",
    "ymax =", bbox[4], "\n"
  ))
}



#' Plot method for remap object.
#'
#' Plots the regions used for modeling.
#'
#' @param x \emph{} S3 object output from remap.
#' @param ... Arguments to pass to regions plot.
#'
#' @return A list that plots a map of the regions used for modeling.
#'
#' @export
plot.remap <- function(x, ...) {
  graphics::plot(x$regions, ...)
}



# parallel_hack
# ==============================================================================
# This is a hack for a bug that only comes up occasionally when running
# parallel code. For some reason when remap is run in parallel, the geometry
# column of the subset data object gets stripped of its class and returns
# to a "list" object. The workaround is to convert the geometry list to
# an sfc object and reassign the crs.
# Input:
#   data_ss - subset of data points
#   crs - the correct crs returned by sf::st_crs
# Output:
#   A corrected sf object
# ==============================================================================
parallel_hack <- function(data_ss, crs) {
  geom_col <- attr(data_ss, "sf_column")
  data_ss[[geom_col]] <- sf::st_sfc(data_ss[[geom_col]])
  sf::st_crs(data_ss) <- crs
  return(data_ss)
}






