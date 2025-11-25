#' CityWeather class constructor
#'
#' @param city name of the city
#' @param temperature numeric temperature (e.g., in °C)
#' @param humidity numeric humidity in percent (0–100)
#' @param pressure numeric pressure in hPa
#'
#' @return an object of class City_Weather
#' @export
CityWeather <- function(city, temperature, humidity, pressure) {
  obj <- list(
    city        = city,
    temperature = temperature,
    humidity    = humidity,
    pressure    = pressure
  )
  class(obj) <- "City_Weather"
  obj
}

#' Print method for City_Weather
#'
#' @param x City_Weather object
#' @param ... unused
#'
#' @export
print.City_Weather <- function(x, ...) {
  cat("Weather for", x$city, ":\n")
  cat("  Temperature:", x$temperature, "\n")
  cat("  Humidity   :", x$humidity, "%\n")
  cat("  Pressure   :", x$pressure, "hPa\n")
  invisible(x)
}
#' Expected number of guests from weather
#'
#' Uses the link:
#'   lambda = exp(0.5 + 0.5 T - 3 H + 0.001 P)
#' where H is rescaled from percent to [0, 1].
#'
#' @param city_weather City_Weather object
#'
#' @return numeric expected number of guests (lambda)
#' @export
expected_lambda <- function(city_weather) {
  T <- city_weather$temperature
  H <- city_weather$humidity / 100  # convert % to [0,1]
  P <- city_weather$pressure

  exp(0.5 + 0.5 * T - 3 * H + 0.001 * P)
}
