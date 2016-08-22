#' Makes filled contour plot from data without sidebar by interpolating with Gaussian process, uses contourfilled
#' @param x  either just x data, x and y data, or x, y and z data
#' @param y  either y data, z data, or null
#' @param z  either z data or null
#' @param xcontlim  x limits for the contour plot
#' @param ycontlim  y limits for the contour plot
#' @param ...  passed to contourfilled.func
#' @importFrom mlegp mlegp
#' @importFrom mlegp predict.gp
#' @importFrom utils capture.output
#' @examples 
#' x <- runif(20)
#' y <- runif(20)
#' z <- exp(-(x-.5)^2-5*(y-.5)^2)
#' contourfilled.data(x,y,z)
#' @references
#' [1] filled.contour R function, copied function but removed part for sidebar
#' @references
#' [2] http://stackoverflow.com/questions/16774928/removing-part-of-a-graphic-in-r, answer by P Lapointe
#' @export
contourfilled.data <- function(x,y=NULL,z=NULL,xcontlim=NULL,ycontlim=NULL,...) {
  # Function that creates a contour plot from a data set
  # using a Gaussian process interpolation from mlegp
  #  x,y,z: three dimensional data, can be given only in x, in x and y, or in all three
  #  xcontlim,ycontlim: contour limits will be set to data limits +- 5% if not specified
  #  ... parameters passed to contourfilled.func
  # Created 5/23/16 by Collin Erickson
  #require(mlegp)
  # This section parses data into x, y, and z
  if (is.null(y) & !is.null(z)) {
    if(dim(x)[2]!=2) {stop('Either give y or x must be matrix')}
    y <- x[,2]
    x <- x[,1]
  } else if (!is.null(y) & is.null(z)) {
    if(dim(x)[2]!=2) {stop('Either give y or x must be matrix')}
    z <- y
    y <- x[,2]
    x <- x[,1]
  } else if (is.null(y) & is.null(z)) {
    if (dim(x)[2]!=3) {stop('If only giving x it must have three columns')}
    z <- x[,3]
    y <- x[,2]
    x <- x[,1]
  }
  # Fits a Gaussian process model that interpolates perfectly, ie no smoothing
  co <- capture.output(mod <- mlegp::mlegp(X=data.frame(x,y),Z=z,verbose=0))
  pred.func <- function(xx) {mlegp::predict.gp(mod,xx)}
  minx <- min(x);maxx <- max(x);miny <- min(y);maxy <- max(y)
  if(is.null(xcontlim)) {xcontlim <- c(minx-.05*(maxx-minx),maxx+.05*(maxx-minx))}
  if(is.null(ycontlim)) {ycontlim <- c(miny-.05*(maxy-miny),maxy+.05*(maxy-miny))}
  # Passes prediction function to contourfilled.func
  contourfilled.func(fn0 = pred.func,xcontlim=xcontlim,ycontlim=ycontlim,...)
  # Adds points to show where data came from
  points(x,y,pch=19)
}