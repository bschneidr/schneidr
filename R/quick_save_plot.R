#' Save and preview a plot
#'
#' @description Convenience function to quickly save a plot to an image file and immediately inspect it. Based on the function `ggpreview` by Tristan (TJ) Mahr.
#'
#' @param plot A plot object. If unspecified, uses the most recent printed plot.
#' @param fname A filename for the plot to be saved. Uses a temporary file in the user's temporary folder if unspecified.
#' @param inspect A logical defaulting to `TRUE` indicating whether to immediately open the file for inspection.
#' @param device Device to use. Can either be a device function (e.g. png()), or one of "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only). Defaults to 'png'.
#' @param dpi Plot resolution. Also accepts a string input: "retina" (320), "print" (300), or "screen" (72). Applies only to raster output types.
#' @param scale Multiplicative scaling factor.
#' @param \dots Additional arguments passed on to the `ggsave` function.
#' @return Saves a plot to a temporary file in the user's temp directory
#' @export
#'
#' @examples
#' ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' quick_save_plot()

quick_save_plot <- function(plot = ggplot2::last_plot(),
                            fname = tempfile(fileext = paste0(".", device)),
                            inspect = TRUE,
                            device = "png", dpi = 320, scale = 1, ...) {

  fname <- fname
  ggplot2::ggsave(plot = plot,
                  filename = fname,
                  device = device,
                  dpi = dpi, scale = scale,
                  ...)
  if (inspect) {
    system2("open", fname)
  }
  invisible(NULL)
}
