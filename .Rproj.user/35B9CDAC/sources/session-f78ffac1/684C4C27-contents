#' Simulate a single party
#'
#' Internal helper: simulate champagne consumption for one party
#' given weather conditions and a glass profile.
#'
#' @param city_weather CityWeather object
#' @param glass Glass_Profile object
#' @param a lower filling level (default: glass$a)
#' @param bottle_L bottle size in liters
#'
#' @return list with guests, glasses, total_L, bottles
#' @keywords internal
simulate_party_one <- function(city_weather,
                               glass,
                               a = glass$a,
                               bottle_L = 0.75) {
  # expected number of guests from weather
  lambda <- expected_lambda(city_weather)

  # number of guests: G ~ Poisson(lambda)
  G <- stats::rpois(1, lambda = lambda)

  if (G <= 0) {
    return(list(
      guests  = 0L,
      glasses = 0L,
      total_L = 0,
      bottles = 0L
    ))
  }

  # total glasses: Poisson(G * 1.4)
  total_glasses <- stats::rpois(1, lambda = G * 1.4)

  if (total_glasses <= 0) {
    return(list(
      guests  = G,
      glasses = 0L,
      total_L = 0,
      bottles = 0L
    ))
  }

  vols_cm3 <- numeric(total_glasses)
  sd_b <- sqrt(0.25)  # sd = 0.5

  for (k in seq_len(total_glasses)) {
    # truncated normal: b > a
    b_k <- a
    while (b_k <= a) {
      b_k <- stats::rnorm(1, mean = 14, sd = sd_b)
    }
    vols_cm3[k] <- volume_between(glass, lower = a, upper = b_k)
  }

  total_cm3 <- sum(vols_cm3)
  total_L   <- total_cm3 / 1000
  bottles   <- ceiling(total_L / bottle_L)

  list(
    guests  = as.integer(G),
    glasses = as.integer(total_glasses),
    total_L = total_L,
    bottles = as.integer(bottles)
  )
}


#' Simulate many champagne parties
#'
#' Simulate \code{N} parties given weather conditions and a glass shape.
#'
#' @param city_weather City_Weather object
#' @param glass Glass_Profile object
#' @param N number of simulated parties
#' @param bottle_L bottle size in liters
#' @param seed integer random seed
#'
#' @return data.frame with columns guests, glasses, total_L, bottles
#' @export
#'
#' @examples
#' \dontrun{
#' glass <- GlassProfile(
#'   a = 12.6, b = 22.5,
#'   x_1 = 1.1, x_2 = 12.6, x_3 = 18.8, x_4 = 22.5,
#'   r_foot = 5.8, r_stem = 0.25, r_bowl = 7.5, r_rim = 7
#' )
#'
#' bern <- CityWeather("Bern", temperature = 20, humidity = 60, pressure = 1013)
#' simulate_parties(bern, glass, N = 10)
#' }
simulate_parties <- function(city_weather,
                             glass,
                             N = 1000L,
                             bottle_L = 0.75,
                             seed = 123L) {
  set.seed(seed)

  res <- vector("list", N)
  for (i in seq_len(N)) {
    res[[i]] <- simulate_party_one(
      city_weather = city_weather,
      glass = glass,
      a = glass$a,
      bottle_L = bottle_L
    )
  }

  df <- do.call(rbind, lapply(res, function(x) {
    data.frame(
      guests  = x$guests,
      glasses = x$glasses,
      total_L = x$total_L,
      bottles = x$bottles
    )
  }))

  rownames(df) <- NULL
  df
}
