#' Contour plot from data
#' 
#' Makes filled contour plot from data without sidebar by interpolating 
#' with a Gaussian process model.
#' The model is passed to cf_func to make the contour plot.
#' 
#' @param x  either just x data, x and y data, or x, y and z data
#' @param y  either y data, z data, or null
#' @param z  either z data or null
#' @param xlim  x limits for the contour plot, will be set to data limits +- 5\% if not specified
#' @param ylim  y limits for the contour plot, will be set to data limits +- 5\% if not specified
#' @param xylim x and y limits for the contour plot
#' @param fit Method to fit a model with. Current options are laGP (default)
#' and mlegp. laGP is faster but might cause trouble.
#' @param gg If TRUE, will use ggplot2 by calling gcf_func
#' @param ...  passed to cf_func
#' @importFrom utils capture.output
#' @importFrom stats predict
#' @examples 
#' x <- runif(20)
#' y <- runif(20)
#' z <- exp(-(x-.5)^2-5*(y-.5)^2)
#' cf_data(x,y,z)
#' @export
cf_data <- function(x, y=NULL, z=NULL,
                    xlim=NULL, ylim=NULL, xylim=NULL,
                    fit="",
                    gg=FALSE,
                    ...) {
  # Function that creates a contour plot from a data set
  # using a Gaussian process interpolation from mlegp
  #  x,y,z: three dimensional data, can be given only in x, in x and y, or in all three
  #  xlim,ylim: contour limits will be set to data limits +- 5% if not specified
  #  ... parameters passed to cf_func
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
  # Fits a Gaussian process model that interpolates perfectly, i.e., no smoothing
  if (fit == "mlegp") {
    co <- capture.output(mod <- mlegp::mlegp(X=data.frame(x,y),Z=z,verbose=0))
    pred.func <- function(xx) {mlegp::predict.gp(mod,xx)}
  } else {
    X <- data.frame(x, y)
    da <- laGP::darg(list(mle=TRUE), X=X)
    ga <- laGP::garg(list(mle=TRUE), y=z)
    mod1 <- laGP::newGPsep(X=X, Z=z, d=da$start, g=ga$start, dK = TRUE)
    laGP::jmleGPsep(gpsepi = mod1, drange=c(da$min, da$max),
                    grange=c(ga$min, ga$max),
                    dab=da$ab, gab=ga$ab, verb=0, maxit=1000)
    
    pred.func <- function(xx) {laGP::predGPsep(mod1, xx, lite=TRUE)$mean}
  }
  
  minx <- min(x);maxx <- max(x);miny <- min(y);maxy <- max(y)
  if (!is.null(xylim)) {xlim <- ylim <- xylim}
  if(is.null(xlim)) {xlim <- c(minx-.05*(maxx-minx),maxx+.05*(maxx-minx))}
  if(is.null(ylim)) {ylim <- c(miny-.05*(maxy-miny),maxy+.05*(maxy-miny))}
  # Passes prediction function to cf_func
  if (gg) {
    gcf_func(fn0 = pred.func,xlim=xlim,ylim=ylim, pts=cbind(x,y), batchmax=500, ...)
  } else {
    cf_func(fn0 = pred.func,xlim=xlim,ylim=ylim, pts=cbind(x,y), batchmax=500, ...)
  }
}