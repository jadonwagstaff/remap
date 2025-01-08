delayedAssign("utws", local({
  try(
    sf::st_read(
      system.file("extdata/utws.shp", package = "remap"),
      quiet = TRUE
    ),
    silent = TRUE
  )
}))
