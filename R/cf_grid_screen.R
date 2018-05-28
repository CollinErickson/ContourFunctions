#' Create a contour plot from a grid of data
#' 
#' Makes filled contour plot with an optional sidebar, essentially filled.contour function.
#' This version uses the split.screen() function to add the sidebar if bar is TRUE.
#' By default it won't show the bar but will show the min and max values in the plot title
#' along with their colors.
#' Using this function will make other functions such as points() called afterwards not put points
#' where you expect. Pass anything you want added to the plot area to afterplotfunc
#' as a function to get it to work properly.
#' 
#' @param x  x values, must form grid with y. If not given, it is assumed to be from 0 to 1.
#' @param y  y values, must form grid with x. If not given, it is assumed to be from 0 to 1.
#' @param z  z values at grid locations
#' @param xlim  x limits for the plot.
#' @param ylim  y limits for the plot.
#' @param zlim  z limits for the plot.
#' @param levels  a set of levels which are used to partition the range of z. Must be strictly increasing (and finite). Areas with z values between consecutive levels are painted with the same color.
#' @param nlevels  if levels is not specified, the range of z, values is divided into approximately this many levels.
#' @param color.palette  a color palette function to be used to assign colors
#' in the plot. Defaults to cm.colors. Other options include rainbow,
#' heat.colors, terrain.colors, topo.colors, and function(x) {gray((1:x)/x)}.
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
#' @param reset.par Should the graphical parameters be reset before exiting? Usually should be
#' unless you need to add something to the plot afterwards and bar is TRUE.
#' @param pretitle Text to be preappended to end of plot title
#' @param posttitle Text to be appended to end of plot title
#' @param main Title for the plot
#' @param mainminmax  whether the min and max values should be shown in the title of plot
#' @param mainminmax_minmax Whether [min,max]= should be shown in title or just the numbers
#' @param afterplotfunc Function to call after plotting, such as adding points or lines.
#' @param cex.main The size of the main title. 1.2 is default.
#' @param par.list List of options to pass to par
#' @param xaxis Should x axis be added?
#' @param yaxis Should y axis be added?
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
#' @importFrom graphics axis layout lcm rect
#' @importFrom graphics split.screen screen close.screen
#' @examples 
#' x <- y <- seq(-4*pi, 4*pi, len = 27)
#' r <- sqrt(outer(x^2, y^2, "+"))
#' cf_grid(cos(r^2)*exp(-r/(2*pi)))
#' cf_grid(r, color.palette=heat.colors, bar=TRUE)
#' cf_grid(r, color.palette=function(x) {gray((1:x)/x)}, bar=TRUE)
#' @references
#' [1] filled.contour R function, copied function but removed part for sidebar
#' @references
#' [2] http://stackoverflow.com/questions/16774928/removing-part-of-a-graphic-in-r, answer by P Lapointe
#' @export
cf_grid <-
  function (x = seq(0, 1, length.out = nrow(z)), 
            y = seq(0, 1,length.out = ncol(z)), z, xlim = range(x, finite = TRUE),
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE),
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors,
            col = color.palette(length(levels) - 1), plot.title, plot.axes,
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1,
            axes = TRUE, frame.plot = axes, bar=F, pts=NULL, reset.par=TRUE,
            pretitle="", posttitle="", main=NULL,
            mainminmax=!bar, mainminmax_minmax=TRUE,
            afterplotfunc=NULL,
            cex.main=par()$cex.main,
            par.list=NULL,
            xaxis=TRUE, yaxis=TRUE,
            ...)
  {#browser()
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
    
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    #if (reset.par) {on.exit({par(par.orig);close.screen(1)})}#all=TRUE)})}
    
    # This allows user to pass in par.list and it will return it after plotting
    par.names.to.save <- c("mar", "las", "mfrow", names(par.list))
    mar.orig <- (par.orig <- par(par.names.to.save))$mar
    if (!is.null(par.list)) {
      par(par.list)
    }
    
    #on.exit(close.screen(all=TRUE))
    w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    #layout(matrix(c(if(bar) 2 else 1, 1), ncol = 2L), widths = c(1, lcm(w)))
    par(las = las)
    start.screen.number <- screen()
    if (bar) {#browser()
      #split.screen(c(1,2))
      screen.numbers <- split.screen(matrix(c(0,.85,0,1,.85,1,0,1), ncol=4, byrow=T))
      screen1 <- screen.numbers[1]
      screen2 <- screen.numbers[2]
      screen(screen2)
      mar <- mar.orig
      mar[4L] <- 2.5#mar[2L] # right
      mar[1] <- 2.2 # bottom
      mar[3] <- if (mainminmax | !is.null(main)) 1.3 else .3 #1.3#1.3 # top
      mar[2L] <- 0#1 # left
      par(mar = mar)
      plot.new()
      plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
                  yaxs = "i")
      rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
      if (missing(key.axes)) {
        if (axes) 
          axis(4)
      }
      else key.axes
      box()
      if (!missing(key.title))
        key.title
      mar <- mar.orig
      mar[4L] <- 1 # right # Why is this here?
      close.screen(screen2)
      screen(screen1)
      mar[1L] <- 2.2 # bottom
      mar[2L] <- 2.5 # left
      mar[3L] <- if (mainminmax | !is.null(main)) 1.3 else .3 #1.3# 1.3 # top
    }
    if (!bar) {
      # Using screen even with 1 screen to avoid error. Adding points after didn't show up properly
      screen.numbers <- split.screen(c(1,1)) 
      screen1 <- screen.numbers[1]
      screen(screen1)
      # Changing the margin to get bigger and square
      mar <- mar.orig #<- par()$mar
      mar[1] <- 2.2 # bottom
      mar[2] <- 2.5 # left
      mar[3] <- if (mainminmax | !is.null(main)) 1.3 else .3 # top
      mar[4] <- 1 # right
      
      if (!missing(plot.axes) && plot.axes == FALSE) {
        # TODO I shouldn't use plot.axes like this, FIX THIS
        mar[1] <- .3 # bottom
        mar[2] <- .3 # left
        mar[4] <- .3 # 1 # right
      }
      if (!xaxis && !yaxis) {
        mar[1] <- .3 # bottom
        mar[2] <- .3 # left
        mar[3] <- if (mainminmax | !is.null(main)) 1.3 else .3 # top
        mar[4] <- .3 # right
      } else if (!xaxis) {
        mar[1] <- 1 # bottom
        mar[4] <- .3 # right
      } else if (!yaxis) {
        mar[2] <- 1 # left
        mar[3] <- if (mainminmax | !is.null(main)) 1.3 else .3 # top
      }
    }
    par(mar = mar)
    # par(cex.axis = 2)
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
    # Something like this will remove axis numbers and ticks
    # Axis(x, side=1, labels=F, tick=F)
    if (missing(plot.axes) || isTRUE(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        if (xaxis) Axis(x, side = 1)
        if (yaxis) Axis(y, side = 2)
      }
    }
    else plot.axes
    if (frame.plot)
      box()
    #if (missing(plot.title))
    #  title(...)
    #else plot.title
    
    if (mainminmax | !is.null(main)) {
      make.multicolor.title(main=main, z=z, pretitle=pretitle, posttitle=posttitle, mainminmax_minmax=mainminmax_minmax, cex.main=cex.main)
    }
    
    if (!is.null(pts)) {
      if (!is.matrix(pts)) { # if not a matrix, make it a matrix by row
        if (is.numeric(pts) && (length(pts)%%2==0)) {
          pts <- matrix(pts, ncol=2, byrow = T)
        }
      }
      points(pts, pch=19)
    }
    
    if (!is.null(afterplotfunc)) {
      afterplotfunc()
    }
    
    reset.par.func <- function() {
      if (T) {close.screen(screen1)}
      if (start.screen.number != FALSE) {screen(start.screen.number, new=FALSE)}
      par(par.orig)
    }
    if (reset.par) {# Either reset parameters
      reset.par.func()
      invisible()
    } else { # or return it so user can do it later
      return(reset.par.func)
    }
  }
