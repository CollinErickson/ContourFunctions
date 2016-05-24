contour.filled <-
  function (x = seq(0, 1, length.out = nrow(z)), 
            y = seq(0, 1,length.out = ncol(z)), z, xlim = range(x, finite = TRUE),
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE),
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors,
            col = color.palette(length(levels) - 1), plot.title, plot.axes,
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1,
            axes = TRUE, frame.plot = axes, ...)
  {
    # filled.contour gives unnecessary legend, this function removes it
    # Used P Lapointe's solution from here:http://stackoverflow.com/questions/16774928/removing-part-of-a-graphic-in-r
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

contour.filled.func <- function(fn0,n=100,xcontlim=c(0,1),ycontlim=c(0,1),mainminmax=T,batchmax=1,out.col.name=NULL,...) {
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
        if(inbatch == batchmax | (xi==n & yi==n)) {#browser()
          Zbatch <- fn(matrix(c(x[XYi[,1]],y[XYi[,2]]),ncol=2,byrow=F))
          #z[XYi[,1],XYi[,2]] <- fn(c(x[xi],y[yi]))
          for(rowbatch in 1:length(Zbatch)) {
            z[XYi[rowbatch,1],XYi[rowbatch,2]] <- Zbatch[rowbatch]
          }
          inbatch <- 0
          rm(XYi)
        }
      }
    }
  }
  if(mainminmax)
    contour.filled(x,y,z,main=paste(signif(c(min(z),max(z)),3)),...)
  else
    contour.filled(x,y,z,...)
}
contour.filled.data <- function(x,y=NULL,z=NULL,xcontlim=NULL,ycontlim=NULL,...) {
  # Function that creates a contour plot from a data set
  # using a Gaussian process interpolation from mlegp
  #  x,y,z: three dimensional data, can be given only in x, in x and y, or in all three
  #  xcontlim,ycontlim: contour limits will be set to data limits +- 5% if not specified
  #  ... parameters passed to contour.filled.func
  # Created 5/23/16 by Collin Erickson
  require(mlegp)
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
  mod <- mlegp(X=data.frame(x,y),Z=z,verbose=0)
  pred.func <- function(xx) {predict.gp(mod,xx)}
  minx <- min(x);maxx <- max(x);miny <- min(y);maxy <- max(y)
  if(is.null(xcontlim)) {xcontlim <- c(minx-.05*(maxx-minx),maxx+.05*(maxx-minx))}
  if(is.null(ycontlim)) {ycontlim <- c(miny-.05*(maxy-miny),maxy+.05*(maxy-miny))}
  # Passes prediction function to contour.filled.func
  contour.filled.func(fn0 = pred.func,xcontlim=xcontlim,ycontlim=ycontlim,...)
  # Adds points to show where data came from
  points(x,y,pch=19)
}