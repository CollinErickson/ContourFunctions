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
#' @param show_points Whether the input data points should be shown on the plot.
#' If missing, is TRUE when there are more than 300 points.
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
                    show_points,
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
  # Check fit name given
  if (fit == "") {
    if (length(x) > 200) {
      fit <- "locfit"
      message("Fitting with locfit since n > 200")
    } else {
      fit <- "lagp"
      message("Fitting with laGP since n <= 200")
    }
  }
  # Fits a Gaussian process model that interpolates perfectly, i.e., no smoothing
  if (fit == "mlegp") {
    co <- capture.output(mod <- mlegp::mlegp(X=data.frame(x,y),Z=z,verbose=0))
    pred.func <- function(xx) {mlegp::predict.gp(mod,xx)}
  } else if (fit %in% c("lagp")) {
    X <- data.frame(x, y)
    da <- laGP::darg(list(mle=TRUE), X=X)
    ga <- laGP::garg(list(mle=TRUE), y=z)
    mod1 <- laGP::newGPsep(X=X, Z=z, d=da$start, g=ga$start, dK = TRUE)
    laGP::jmleGPsep(gpsepi = mod1, drange=c(da$min, da$max),
                    grange=c(ga$min, ga$max),
                    dab=da$ab, gab=ga$ab, verb=0, maxit=1000)
    
    pred.func <- function(xx) {laGP::predGPsep(mod1, xx, lite=TRUE)$mean}
  } else if (fit == "locfit") {
    # browser()
    X <- data.frame(x, y, z)
    lfmod <- locfit::locfit(z ~ x + y, data=X)
    pred.func <- function(xx) {
      # browser()
      predict(lfmod, data.frame(x=xx[,1], y=xx[,2]))
    }
  } else if (fit == "gam") {
    # browser()
    X <- data.frame(x=x, y=y, z=z) # Need new names?
    gammod <- mgcv::gam(z ~ te(x, y), data=X)
    print(gammod)
    pred.func <- function(xx) {
      # browser()
      predict(gammod, data.frame(x=xx[,1], y=xx[,2]))
    }
  } else {
    stop(paste0("fit is unknown"))
  }
  
  minx <- min(x);maxx <- max(x);miny <- min(y);maxy <- max(y)
  if (!is.null(xylim)) {xlim <- ylim <- xylim}
  if(is.null(xlim)) {xlim <- c(minx-.05*(maxx-minx),maxx+.05*(maxx-minx))}
  if(is.null(ylim)) {ylim <- c(miny-.05*(maxy-miny),maxy+.05*(maxy-miny))}
  # Make pts to pass that will be shown on plot
  if (missing(show_points)) {
    show_points <- if (length(x)>300) {F} else {T}
  }
  if (show_points) {
    pts <- cbind(x,y)
  } else {
    pts <- NULL
  }
  # Passes prediction function to cf_func
  if (gg) {
    gcf_func(fn0 = pred.func,xlim=xlim,ylim=ylim, pts=pts, batchmax=500, ...)
  } else {
    cf_func(fn0 = pred.func,xlim=xlim,ylim=ylim, pts=pts, batchmax=500, ...)
  }
}
