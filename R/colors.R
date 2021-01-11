#' @name Colors
#'
#' @rdname colors
#'
#' @title Functions for easily accessing preferred colors
#'
#' @description Functions that return a color (or set of colors) of interest for use in data visualization.
#'
#' @param alpha A single number between 0 and 1
#'
#' @return A character vector with elements of 7 or 9 characters, "#" followed by the red, blue, green and optionally alpha values in hexadecimal.
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' mtcars[['gear']] <- factor(mtcars[['gear']])
#' ggplot(mtcars) +
#' geom_point(aes(x = wt, y = mpg),
#'            color = schneidr_purple(),
#'            size = 3) +
#' labs(title = "Fuel economy and weight",
#'      subtitle = "Data from the 1974 Motor Trend US magazine.",
#'      x = "Weight (1000 lbs)",
#'      y = "Fuel economy (mpg)")

schneidr_purple <- function(alpha = 1) {
  if (!is.numeric(alpha) || length(alpha) != 1) {
    if (alpha > 1 | alpha < 0) {
      stop("`alpha` must be a single number between 0 and 1")
    }
  }
  rgb(red = 49, green = 26, blue = 77,
      maxColorValue = 255,
      alpha = alpha*255)
}
