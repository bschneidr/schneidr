#' @name Integrals
#' @rdname integrals
#' @title Numerical integration using adaptive quadrature
#'
#' @param fn A function that takes two or three numeric arguments (for double_integrate or triple_integrate, respectively) and which returns a single number
#' @param x1_lims A length-two numeric vector giving the lower and upper bound of integration (e.g. c(0, 100))
#' @param x2_lims A length-two numeric vector giving the lower and upper bound of integration (e.g. c(0, 2*pi))
#' @param x3_lims A length-two numeric vector giving the lower and upper bound of integration (e.g. c(-Inf, Inf))
#'
#' @return A single number, representing the numeric integral calculated using adaptive quadrature.

NULL

#' @rdname integrals
#'
#' @examples
#'
#' # Area of a cylinder!
#' # Radius 2, height 5
#'
#'   perimeter_of_circle <- function(r) {
#'      2*pi*r
#'   }
#'
#'   double_integrate(function(r, height) perimeter_of_circle(r),
#'                    x1_lims = c(0, 2),
#'                    x2_lims = c(0, 5))
#'
#' @export


double_integrate <- function(fn, x1_lims, x2_lims) {
    InnerIntegral = Vectorize(
      function(x_2) {
        integrate(Vectorize(function(x_1) fn(x_1, x_2)),
                  x1_lims[1], x1_lims[2])$value}
    )
    integrate(InnerIntegral , x2_lims[1], x2_lims[2])$value
}

#' @rdname integrals
#' @examples
#' # Joint density of three standard normal variables
#'   d_x1x2x3 <- function(x1, x2, x3) {
#'      dnorm(x1)*dnorm(x2)*dnorm(x3)
#'   }
#'
#'   triple_integrate(d_x1x2x3, c(-Inf, Inf), c(-Inf, Inf), c(-Inf, Inf))
#'
#' # Probability that sum of three squared standard normal variables
#'   is less than 9
#'
#'    supported_density <- function(k, x1, x2, x3) {
#'       sum_squares <- sum(c(x1, x2, x3)^2)
#'       if (sum_squares <= k) {
#'          dnorm(x1)*dnorm(x2)*dnorm(x3)
#'       } else {
#'          0
#'       }
#'    }
#'
#'    triple_integrate(fn = function(x1, x2, x3) {
#'                             supported_density(k = 9, x1, x2, x3)
#'                     },
#'                     x1_lims = c(-Inf, Inf),
#'                     x2_lims = c(-Inf, Inf),
#'                     x3_lims = c(-Inf, Inf))
#'
#'   # Compare to the analytic result (Chi-squared distribution)
#'
#'     pchisq(9, df = 3)
#'
#'
#' @export
#'

triple_integrate <- function(fn, x1_lims, x2_lims, x3_lims) {

  InnerIntegral = Vectorize(
    function(x_3) {
      double_integrate(function(x_1, x_2) fn(x_1, x_2, x_3),
                       x1_lims = x1_lims, x2_lims = x1_lims)
  })
  integrate(InnerIntegral , x3_lims[1], x3_lims[2])$value
}
