delayedAssign("utsnow", local({
  try(
    {
      utsnow <- sf::st_read(
        system.file("extdata/utsnow.shp", package = "remap"),
        quiet = TRUE
      )
      names(utsnow) <- c(
        "ID", "STATION_NAME", "LATITUDE", "LONGITUDE", "ELEVATION", "HUC2",
        "WESD", "geometry"
      )
      utsnow
    },
    silent = TRUE
  )
}))
