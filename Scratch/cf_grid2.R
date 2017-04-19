#' Makes filled contour plot without sidebar, essentially filled.contour function.
#' This was the original version of cf_grid, before I tried adding the bar option.
#' I used it when I had trouble adding the bar to the function, but now that it works
#' I don't need this.
#' The legend part is commented out so `bar` does nothing.
#' Will no longer export since it doesn't do anything useful anymore.
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
#' @param bar Should a bar showing the output range and colors be shown on the right?
#' @param pts Points to plot on top of contour
#' @param reset.par Should the graphical parameters be reset before exiting? Usually should be.
#' @param ...  additional graphical parameters, currently only passed to title().
#' @importFrom grDevices cm.colors
#' @importFrom graphics .filled.contour
#' @importFrom graphics Axis 
#' @importFrom graphics box
#' @importFrom graphics plot.new 
#' @importFrom graphics plot.window 
#' @importFrom graphics points 
#' @importFrom graphics title
#' @importFrom graphics par
#' @examples 
#' x <- y <- seq(-4*pi, 4*pi, len = 27)
#' r <- sqrt(outer(x^2, y^2, "+"))
#' cf_grid2(cos(r^2)*exp(-r/(2*pi)))
#' @references
#' [1] filled.contour R function, copied function but removed part for sidebar
#' @references
#' [2] http://stackoverflow.com/questions/16774928/removing-part-of-a-graphic-in-r, answer by P Lapointe
#' @export
cf_grid2 <-
  function (x = seq(0, 1, length.out = nrow(z)), 
            y = seq(0, 1,length.out = ncol(z)), z, xlim = range(x, finite = TRUE),
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE),
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors,
            col = color.palette(length(levels) - 1), plot.title, plot.axes,
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1,
            axes = TRUE, frame.plot = axes, bar=NULL, pts=NULL, reset.par=NULL,...)
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
    
    if (!is.null(pts)) {
      points(pts, pch=19)
    }
    
    invisible()
  }
