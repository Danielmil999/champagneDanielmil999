#' GlassProfile constructor
#'
#' @param a,b numeric limits for champagne level
#' @param x_1,x_2,x_3,x_4 breakpoints along the glass
#' @param r_foot,r_stem,r_bowl,r_rim radii at key points
#'
#' @return an object of class Glass_Profile
#' @export
GlassProfile <- function(a, b,
                         x_1, x_2, x_3, x_4,
                         r_foot, r_stem, r_bowl, r_rim) {

  obj <- list(
    a = a, b = b,
    x_1 = x_1, x_2 = x_2, x_3 = x_3, x_4 = x_4,
    r_foot = r_foot, r_stem = r_stem,
    r_bowl = r_bowl, r_rim = r_rim
  )

  class(obj) <- "Glass_Profile"
  obj
}


#' radius_cone method
#'
#' @param x Glass_Profile object
#' @param t numeric position
#'
#' @return radius at t
#' @export
radius_cone <- function(x, t) {
  r(t,
    x_1 = x$x_1, x_2 = x$x_2, x_3 = x$x_3, x_4 = x$x_4,
    r_foot = x$r_foot, r_stem = x$r_stem,
    r_bowl = x$r_bowl, r_rim = x$r_rim
  )
}


#' volume_between method
#'
#' @param x Glass_Profile object
#' @param lower lower integration limit (default: x$a)
#' @param upper upper integration limit (default: x$b)
#'
#' @return numeric volume between lower and upper (cm^3)
#' @export
volume_between <- function(x, lower = x$a, upper = x$b) {
  res <- integrate(
    f = function(t)
      pi * r_vec_for_integrate(
        t,
        x_1 = x$x_1, x_2 = x$x_2, x_3 = x$x_3, x_4 = x$x_4,
        r_foot = x$r_foot, r_stem = x$r_stem,
        r_bowl = x$r_bowl, r_rim = x$r_rim
      )^2,
    lower = lower,
    upper = upper
  )

  res$value
}

