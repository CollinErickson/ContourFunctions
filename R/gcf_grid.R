#' Create contour plot from grid data using ggplot2
#' 
#' The same as cf_grid_screen but uses ggplot2 for the plot.
#'
#' @param x  x values, must form grid with y.
#' If not given, it is assumed to be from 0 to 1.
#' @param y  y values, must form grid with x.
#' If not given, it is assumed to be from 0 to 1.
#' @param z  z values at grid locations
#' @param xlim  x limits for the plot.
#' @param ylim  y limits for the plot.
#' @param zlim  z limits for the plot.
#' @param with_lines Should lines be added on top of 
#' contour to show contours?
#' @param lines_only Should no fill be used, only contour lines?
#' @param bins Number of lines used when using `with_lines` or `lines_only`
#' @param interpolate Will smooth out contours
#' @param levels  a set of levels which are used to partition the range of z.
#' Must be strictly increasing (and finite). Areas with z values between
#' consecutive levels are painted with the same color.
#' @param nlevels  if levels is not specified, the range of z, values is
#' divided into approximately this many levels.
#' @param color.palette  a color palette function to be used to assign colors
#' in the plot. Defaults to cm.colors. Other options include rainbow,
#' heat.colors, terrain.colors, topo.colors, and function(x) \{gray((1:x)/x)\}.
#' @param col  an explicit set of colors to be used in the plot. This argument
#' overrides any palette function specification. There should be one less
#' color than levels
#' @param asp  the y/x aspect ratio, see plot.window.
#' @param las  the style of labeling to be used. The default is to use
#' horizontal labeling.
#' @param bar Should a bar showing the output range and colors be shown on the right?
#' @param pts Points to plot on top of contour
#' @param reset.par Should the graphical parameters be reset before exiting?
#' Usually should be unless you need to add something to the plot
#' afterwards and bar is TRUE.
#' @param pretitle Text to be preappended to end of plot title
#' @param posttitle Text to be appended to end of plot title
#' @param main Title for the plot
#' @param mainminmax  whether the min and max values should be shown
#' in the title of plot
#' @param mainminmax_minmax Whether [min,max]= should be shown in title
#' or just the numbers
#' @param afterplotfunc Function to call after plotting, such as adding
#' points or lines.
#' @param cex.main The size of the main title. 1.2 is default.
#' @param ...  additional graphical parameters, currently only passed to title().
#' 
#' @return ggplot2 object
#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot
#'
#' @examples
#' x <- y <- seq(-4*pi, 4*pi, len = 27)
#' r <- sqrt(outer(x^2, y^2, "+"))
#' gcf_grid(cos(r^2)*exp(-r/(2*pi)))
#' gcf_grid(r, color.palette=heat.colors, bar=TRUE)
#' gcf_grid(r, color.palette=function(x) {gray((1:x)/x)}, bar=TRUE)
gcf_grid <-  function (x = seq(0, 1, length.out = nrow(z)), 
                       y = seq(0, 1,length.out = ncol(z)), z,
                       xlim = range(x, finite = TRUE),
                       ylim = range(y, finite = TRUE),
                       zlim = range(z, finite = TRUE),
                       with_lines=FALSE,
                       lines_only=FALSE,
                       bins=8, # number of contour lines
                       interpolate=TRUE,
                       levels = pretty(zlim, nlevels), nlevels = 20,
                       color.palette = cm.colors.strong,
                       col = color.palette(length(levels) - 1),
                       asp = NA, las = 1,
                       bar=F, pts=NULL, reset.par=TRUE,
                       pretitle="", posttitle="", main=NULL,
                       mainminmax=!bar, mainminmax_minmax=TRUE,
                       afterplotfunc=NULL,
                       cex.main=par()$cex.main,
                       ...) {
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      } else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    } else {
      stop("no 'z' matrix specified")
    }
  } else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) {
    stop("increasing 'x' and 'y' values expected")
  }
  
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) {
    stop("increasing 'x' and 'y' values expected")
  }
  
  
  t2 <- cbind(expand.grid(x, y), tz=c(z))
  p <- ggplot2::ggplot() +
    # scale_fill_gradientn(colours=c("cyan","white","magenta"))
    ggplot2::scale_fill_gradientn(colours=col)
  if (!lines_only) {
    p <- p + ggplot2::geom_raster(ggplot2::aes(x=.data$Var1, y=.data$Var2,
                                               fill = .data$tz),
                                  t2, interpolate=interpolate)
  }
  
  # Add contour lines
  if (with_lines) {
    p <- p + ggplot2::geom_contour(ggplot2::aes(x=.data$Var1, y=.data$Var2, 
                                                z = .data$tz),
                                   bins=bins,
                                   color="black",
                                   t2)
  }
  if (lines_only) { # Only color lines if only lines
    p <- p + ggplot2::geom_contour(ggplot2::aes(x=.data$Var1, y=.data$Var2,
                                                z = .data$tz,
                                                color=.data$..level..),
                                   bins=bins,
                                   t2)
  }
  
  if (bar) {
    # Remove title on legend
    p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
  }
  if (!bar) {
    p <- p + ggplot2::theme(legend.position = "none") 
  }
  
  # Remove axes labels
  p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank()) + 
    ggplot2::theme(axis.title.y = ggplot2::element_blank())
  
  # Cut off extra space
  # p <- p + ggplot2::coord_cartesian(xlim=c(min(x), max(x)), ylim=c(min(y), max(y)), expand=F)
  p <- p + ggplot2::coord_cartesian(xlim=xlim, ylim=ylim, expand=F)
  
  
  if (mainminmax | !is.null(main)) {
    # Unable to put colors in title
    # p <- p + gg_make_multicolor_title()
    gtitle <- pretitle
    if (mainminmax_minmax) {
      gtitle <- paste0(gtitle, "[min,max]=")
    }
    gtitle <- paste0(gtitle, "[",signif(min(z),3),",", signif(max(z),3),"]", posttitle)
    p <- p + ggplot2::ggtitle(gtitle) + 
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    # Max/min annotations since colors don't work
    minind <- arrayInd(which.min(z), dim(z))
    xminz <- x[minind[1]]
    yminz <- y[minind[2]]
    xminz <- min(max(xminz, xlim[1]+.025*(xlim[2]-xlim[1])), xlim[2]-.025*(xlim[2]-xlim[1]))
    yminz <- min(max(yminz, ylim[1]+.015*(ylim[2]-ylim[1])), ylim[2]-.015*(ylim[2]-ylim[1]))
    p <- p + ggplot2::annotate("text", x = xminz, y = yminz, label = signif(min(z),3))
    maxind <- arrayInd(which.max(z), dim(z))
    xmaxz <- x[maxind[1]]
    ymaxz <- y[maxind[2]]
    xmaxz <- min(max(xmaxz, xlim[1]+.025*(xlim[2]-xlim[1])), xlim[2]-.025*(xlim[2]-xlim[1]))
    ymaxz <- min(max(ymaxz, ylim[1]+.015*(ylim[2]-ylim[1])), ylim[2]-.015*(ylim[2]-ylim[1]))
    p <- p + ggplot2::annotate("text", x = xmaxz, y = ymaxz, label = signif(max(z),3))
  }
  
  if (!is.null(pts)) {
    if (!is.matrix(pts)) { # if not a matrix, make it a matrix by row
      if (is.numeric(pts) && (length(pts)%%2==0)) {
        pts <- matrix(pts, ncol=2, byrow = T)
      }
    }
    # points(pts, pch=19)
    pts <- as.data.frame(pts)
    colnames(pts) <- c("V1", "V2")
    p <- p + ggplot2::geom_point(ggplot2::aes(.data$V1, .data$V2), data=pts)
  }
  
  if (!is.null(afterplotfunc)) {
    afterplotfunc()
  }
  
  if (reset.par) {# Either reset parameters
  } else { # or return it so user can do it later
    stop("Can't reset later, this returns the plot")
    # return(reset.par.func)
  }
  p
}

