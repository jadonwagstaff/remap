#' Snowpack at weather stations in utah on April 1st, 2011.
#'
#' Water equivalent of snow density (WESD) in mm of water at various
#' location within and surrounding the state of Utah. WESD are
#' measured at weather stations within the Daily Global Historical
#' Climatology Network. April first measurements are used to
#' estimate snowpack for the state of Utah.
#'
#' @format An sf points object with 394 rows and 9 variables:
#' \describe{
#'   \item{ID}{Weather station identification code.}
#'   \item{STATION_NAME}{Weather station name.}
#'   \item{LATITUDE}{Latitude of weather station.}
#'   \item{LONGITUDE}{Longitude of weather station.}
#'   \item{ELEVATION}{Elevation of weather station.}
#'   \item{HUC2}{Largest watershed area containing this weather station
#'   (see utws data).}
#'   \item{WESD}{Water equivalent of snow density in mm of water.}
#'   \item{geometry}{sfc points in lonlat coordinates.}
#' }
#' @source \url{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily}
"utsnow"



#' Watershed polygons within the state of Utah.
#'
#' Watersheds are defined by the United States Geological Survey.
#' Only the largest defines watersheds are used.
#'
#' @format An sf object with 394 rows and 2 variables:
#' \describe{
#'   \item{HUC2}{Largest watershed ID's defined by the USGS.}
#'   \item{geometry}{sfc multipolygons in lonlat coordinates.}
#' }
#' @source \url{https://www.usgs.gov/core-science-systems/ngp/national-hydrography/watershed-boundary-dataset?qt-science_support_page_related_con=4#qt-science_support_page_related_con}
"utws"




