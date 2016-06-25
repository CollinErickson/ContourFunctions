#' Makes filled contour plot without sidebar, essentially filled.contour function
#' @param x  x values, must form grid with y
#' @param y  y values, must form grid with x
#' @param z  z values at grid locations
#' @param xlim  x limits for the plot.
#' @param ylim  y limits for the plot.
#' @param zlim  z limits for the plot.
#' @param levels  a set of levels which are used to partition the range of z. Must be strictly increasing (and finite). Areas with z values between consecutive levels are painted with the same color.
#' @param nlevels  if levels is not specified, the range of z, values is divided into approximately this many levels.
#' @param color.palette  a color palette function to be used to assign colors in the plot.
#' @param col  an explicit set of colors to be used in the plot. This argument overrides any palette function specification. There should be one less color than levels
#' @param plot.title  statements which add titles to the main plot.
#' @param plot.axes  statements which draw axes (and a box) on the main plot. This overrides the default axes.
#' @param key.title  statements which add titles for the plot key.
#' @param key.axes  statements which draw axes on the plot key. This overrides the default axis.
#' @param asp  the y/x aspect ratio, see plot.window.
#' @param xaxs  the x axis style. The default is to use internal labeling.
#' @param yaxs  the y axis style. The default is to use internal labeling.
#' @param las  the style of labeling to be used. The default is to use horizontal labeling.
#' @param axes  logical indicating if axes should be drawn, as in plot.default.
#' @param frame.plot  logical indicating if a box should be drawn, as in plot.default.
#' @param ...  additional graphical parameters, currently only passed to title().
#' @importFrom grDevices cm.colors
#' @importFrom graphics .filled.contour
#' @importFrom graphics Axis 
#' @importFrom graphics box
#' @importFrom graphics plot.new 
#' @importFrom graphics plot.window 
#' @importFrom graphics points 
#' @importFrom graphics title
#' @examples 
#' x <- y <- seq(-4*pi, 4*pi, len = 27)
#' r <- sqrt(outer(x^2, y^2, "+"))
#' contourfilled(cos(r^2)*exp(-r/(2*pi)))
#' @references
#' [1] filled.contour R function, copied function but removed part for sidebar
#' @references
#' [2] http://stackoverflow.com/questions/16774928/removing-part-of-a-graphic-in-r, answer by P Lapointe
#' @export
contourfilled <-
  function (x = seq(0, 1, length.out = nrow(z)), 
            y = seq(0, 1,length.out = ncol(z)), z, xlim = range(x, finite = TRUE),
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE),
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors,
            col = color.palette(length(levels) - 1), plot.title, plot.axes,
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1,
            axes = TRUE, frame.plot = axes, ...)
  {
    # filled.contour gives unnecessary legend, this function removes it
    # Used P Lapointe's solution from here: http://stackoverflow.com/questions/16774928/removing-part-of-a-graphic-in-r
    #   also had to changed .Internal(fillcontour) to .filled.contour
    #   and change layout to layout(matrix(c(1, 1), ncol = 1L), widths = c(1, lcm(w)))
    # Created 3/28/16 by Collin Erickson
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
      stop("increasing 'x' and 'y' values expected")
    #mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    #on.exit(par(par.orig))
    # Comment out everything that was for legend
    #w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    #layout(matrix(c(1, 1), ncol = 1L), widths = c(1, lcm(w)))
    #par(las = las)
    #mar <- mar.orig
    #mar[4L] <- mar[2L]
    #mar[2L] <- 1
    #par(mar = mar)
    #plot.new()
    #plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i",
    #            yaxs = "i")
    #    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    #    if (missing(key.axes)) {
    #        if (axes)
    #            axis(4)
    #    }
    #    else key.axes
    #    box()
    #if (!missing(key.title))
    #  key.title
    #mar <- mar.orig
    #mar[4L] <- 1
    #par(mar = mar)
    # Changing the margin to get bigger and square
    mar <- mar.orig <- par()$mar
    mar[1] <- 2.2 # bottom
    mar[2] <- 2.5 # left
    mar[3] <- 1.3 # top
    mar[4] <- 1 # right
    par(mar = mar)
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L)
      stop("no proper 'z' matrix specified")
    if (!is.double(z))
      storage.mode(z) <- "double"
    #.Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels),
    #                        col = col))
    .filled.contour(as.double(x), as.double(y), z, as.double(levels),
                    col = col)
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 2)
      }
    }
    else plot.axes
    if (frame.plot)
      box()
    if (missing(plot.title))
      title(...)
    else plot.title
    invisible()
}
#' Makes filled contour plot from function without sidebar, uses contourfilled
#' @param fn0  function to plot, first argument must be two-dimensional
#' @param n  number of points in each dimension
#' @param xcontlim  x limits for the contour plot
#' @param ycontlim  y limits for the contour plot
#' @param mainminmax  whether the min and max values should be shown in the title of plot
#' @param batchmax  number of datapoints that can be computed at a time
#' @param out.col.name  if a column needs to be selected from the function, specify it
#' @param ...  Passed to contourfilled
#' @examples 
#' contourfilled.func(function(x){x[1]*x[2]})
#' contourfilled.func(function(x)(exp(-(x[1]-.5)^2-5*(x[2]-.5)^2)))
#' @references
#' [1] filled.contour R function, copied function but removed part for sidebar
#' @references
#' [2] http://stackoverflow.com/questions/16774928/removing-part-of-a-graphic-in-r, answer by P Lapointe
#' @export
contourfilled.func <- function(fn0,n=100,xcontlim=c(0,1),ycontlim=c(0,1),mainminmax=T,batchmax=1,out.col.name=NULL,...) {
  if(is.null(out.col.name)) {fn <- fn0} else {fn <- function(xx){fn0(xx)[,out.col.name]}}
  x <- seq(xcontlim[1],xcontlim[2],length.out = n)
  y <- seq(ycontlim[1],ycontlim[2],length.out = n)
  z <- matrix(NA,n,n)
  if(batchmax<=1) { # calculate single Z value at a time
    for(xi in 1:n) for(yi in 1:n) {z[xi,yi] <- fn(c(x[xi],y[yi]))}
  } else {
    inbatch <- 0
    for(xi in 1:n) {
      for(yi in 1:n) {
        if(inbatch==0) XYi <- matrix(c(xi,yi),ncol=2)
        else XYi <- rbind(XYi,matrix(c(xi,yi),ncol=2))
        inbatch <- inbatch + 1
        if(inbatch == batchmax | (xi==n & yi==n)) {
          Zbatch <- fn(matrix(c(x[XYi[,1]],y[XYi[,2]]),ncol=2,byrow=F))
          for(rowbatch in 1:length(Zbatch)) {
            z[XYi[rowbatch,1],XYi[rowbatch,2]] <- Zbatch[rowbatch]
          }
          inbatch <- 0
          rm(XYi)
        }
      }
    }
  }
  if(mainminmax) {
    #contourfilled(x,y,z,main=paste('min = ',signif(min(z),3),', max = ',signif(max(z),3)),...)
    #contourfilled(x,y,z,main=paste('(min, max) = (',signif(min(z),3),', ',signif(max(z),3),')'),...)
    contourfilled(x,y,z)
    multicolor.title(c('(','min',', ','max',') = (',signif(min(z),3),', ',signif(max(z),3),')'),c(1,"#80FFFFFF",1,"#FF80FFFF",1,1,1,1,1))
    #contourfilled(x,y,z,main=paste('abcde','abc'),...)
  } else {
    contourfilled(x,y,z,...)
  }
}
#' Makes filled contour plot from data without sidebar by interpolating with Gaussian process, uses contourfilled
#' @param x  either just x data, x and y data, or x, y and z data
#' @param y  either y data, z data, or null
#' @param z  either z data or null
#' @param xcontlim  x limits for the contour plot
#' @param ycontlim  y limits for the contour plot
#' @param ...  passed to contourfilled.func
#' @importFrom mlegp mlegp
#' @importFrom mlegp predict.gp
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
  # Fits a Gaussian process model that interpolates perfectly
  mod <- mlegp::mlegp(X=data.frame(x,y),Z=z,verbose=0)
  pred.func <- function(xx) {mlegp::predict.gp(mod,xx)}
  minx <- min(x);maxx <- max(x);miny <- min(y);maxy <- max(y)
  if(is.null(xcontlim)) {xcontlim <- c(minx-.05*(maxx-minx),maxx+.05*(maxx-minx))}
  if(is.null(ycontlim)) {ycontlim <- c(miny-.05*(maxy-miny),maxy+.05*(maxy-miny))}
  # Passes prediction function to contourfilled.func
  contourfilled.func(fn0 = pred.func,xcontlim=xcontlim,ycontlim=ycontlim,...)
  # Adds points to show where data came from
  points(x,y,pch=19)
}

#' Makes plot title using specified colors for the text
#' @param main  Text to put in main title of plot
#' @param col.main  Colors to use for the text
#' @param collapse  What to put between elements of main, defaults to "" but " " might be appropriate
#' @examples 
#' plot(1:4)
#' multicolor.title(c('Black, ','red, ','green'),c(1,2,3))
#' @export
multicolor.title <- function(main,col.main,collapse='') {
  if (length(main) != length(col.main)) {stop('main and col must have same length')}
  n <- length(main)
  if(n==1) {print('n is 1')
    title(bquote(.(main[1])),col.main=col.main[1])
  } else {
    # print first
    title(bquote(.(main[1]) * phantom(.(paste0(main[2:n],collapse=collapse)))),col.main=col.main[1])
    
    # print middle
    if(n > 2) {
      for(i in 2:(n-1)) {
        title(bquote(phantom(.(paste0(main[1:(i-1)],collapse=collapse))) * .(main[i]) * phantom(.(paste0(main[(i+1):n],collapse=collapse)))),col.main=col.main[i]) 
      }
    }
    
    # print last
    title(bquote(phantom(.(paste0(main[1:(n-1)],collapse=collapse))) * .(main[n])),col.main=col.main[n])
  }
}