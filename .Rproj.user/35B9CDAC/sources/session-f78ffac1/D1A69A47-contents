# This script reads raw OpenWeather data and stores a processed version
# into the package data/ directory.

library(jsonlite)
library(usethis)

weather_path <- "data-raw/weather_full.json"

weather_raw <- fromJSON(weather_path, simplifyVector = FALSE)

weather_processed <- lapply(weather_raw, function(city) {
  main <- city$main
  list(
    temperature = main$temp[[1]],
    humidity    = main$humidity[[1]],
    pressure    = main$pressure[[1]]
  )
})

# save as package data
use_data(weather_processed, overwrite = TRUE)
