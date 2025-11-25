#' Easing function S(t)
#'
#' @param t numeric, between 0 and 1
#' @return numeric value of the easing function
#' @export
S <- function(t) {
  0.5 - 0.5 * cos(pi * t)
}


#' Radius profile r(t)
#'
#' @param t position along the glass axis
#' @param x_1,x_2,x_3,x_4 breakpoints
#' @param r_foot,r_stem,r_bowl,r_rim radii
#'
#' @return numeric radius at t
#' @export
r <- function(t, x_1, x_2, x_3, x_4, r_foot, r_stem, r_bowl, r_rim) {
  if (t < 0) return(0)
  if (t < x_1) return(r_foot)
  if (t < x_2) return(r_stem)
  if (t < x_3) {
    z <- (t - x_2) / (x_3 - x_2)
    return(r_stem * (1 - S(z)) + r_bowl * S(z))
  }
  if (t <= x_4) {
    z <- (t - x_3) / (x_4 - x_3)
    return(r_bowl * (1 - S(z)^2) + r_rim * S(z)^2)
  }
  return(0)
}


#' Vectorized radius for integration
#'
#' @param t numeric vector
#' @param ... parameters passed to r()
#' @return numeric vector of radius
#' @export
r_vec_for_integrate <- function(t, ...) {
  vapply(t, r, numeric(1), ...)
}
