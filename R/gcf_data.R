#' Contour plot from data
#' 
#' Makes filled contour plot from data without sidebar by interpolating 
#' with a Gaussian process model.
#' This is the same as `cf_data` except it will use ggplot2 to
#' make the plot.
#' 
#' @param x  either just x data, x and y data, or x, y and z data
#' @param y  either y data, z data, or null
#' @param z  either z data or null
#' @param xlim  x limits for the contour plot, will be set to data limits +- 5\% if not specified
#' @param ylim  y limits for the contour plot, will be set to data limits +- 5\% if not specified
#' @param xylim x and y limits for the contour plot
#' @param fit Method to fit a model with. Current options are laGP (default)
#' and mlegp. laGP is faster but might cause trouble.
#' @param gg If FALSE, will use base graphics by calling cf_func()
#' @param ...  passed to cf_func
#' @importFrom utils capture.output
#' @examples 
#' x <- runif(20)
#' y <- runif(20)
#' z <- exp(-(x-.5)^2-5*(y-.5)^2)
#' gcf_data(x,y,z)
#' @export
gcf_data <- function(x, y=NULL, z=NULL,
                    xlim=NULL, ylim=NULL, xylim=NULL,
                    fit="",
                    gg=TRUE,
                    ...) {
  cf_data(x=x, y=y, z=z, xlim=xlim, ylim=ylim, xylim=xylim,
          fit=fit, gg=gg, ...)
}