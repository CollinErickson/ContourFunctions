#' Plot 2D contour slices of four dimensional functions
#' 
#' Plots a grid of contour plots.
#' Each contour plot is a contour over two dimensions with the remaining
#' two dimensions set to a value.
#' See cf_highdim for functions with more than 4 dimensions.
#'
#' @param func A four-dimensional function to plot contours of
#' @param over Indices of the dimensions used for the outer grid
#' @param nover Number of grid points for the outer grid dimensions
#' @param nover1 Number of grid points for the first outer grid dimension
#' @param nover2 Number of grid points for the second outer grid dimension
#' @param low Low input value for each dimension
#' @param high High input value for each dimension
#' @param n Number of points in grid on each dimension
#' @param same_scale Should all contour plots be on the same scale?
#' Takes longer since it has to precalculate range of outputs.
#' @param batchmax  number of datapoints that can be computed at a time
#' @param pts Matrix of points to show on plot
#' @param nlevels Number of levels in contour scale
#' @param color.palette Color palette used for contour plots
#' @param var_names Variable names to add to plot
#' @param bar Should a bar be added on right when all on same_scale?
#' @param key.axes key for bar plot
#' @param key.title  statements which add titles for the plot key.
#' @param axes axes
#' @param edge_width How wide should edges with variable names be? As proportion of full screen.
#' @param cex.var_names Size of var_names printed on edges.
#' @param ... Arguments passed to cf_func, and then probably through to cf_grid
#'
#' @importFrom graphics contour mtext
#' @return NULL
#' @export
#'
#' @examples
#' cf_4dim(
#'   function(x) {x[1] + x[2]^2 + sin(2*pi*x[3])}
#' )
cf_4dim <- function(func,
                    over=c(1,2),
                    nover=5, nover1=nover, nover2=nover,
                    # over1=seq(0,1,length.out=nover1),
                    # over2=seq(0,1,length.out=nover2),
                    low=rep(0,4), high=rep(1,4),
                    same_scale=TRUE,
                    n=20,
                    batchmax=1,
                    var_names=c(expression(), 
                                lapply(1:4,
                                       function(ti) bquote(x[.(ti)]))),
                    pts=NULL,
                    axes=TRUE, key.axes, key.title,
                    nlevels=20,
                    color.palette=cm.colors,
                    edge_width=.04, cex.var_names=1.3,
                    bar=TRUE,
                    ...) {
  
  d1 <- (1:4)[-over][1] # d1 and d2 are the dimensions of the contour plots
  d2 <- (1:4)[-over][2] # over are the dimensions for the outer set of plots
  over1=seq(low[over[1]],high[over[1]],length.out=nover1)
  over2=seq(low[over[2]],high[over[2]],length.out=nover2)
  
  begin_screen <- screen()
  if (!is.null(pts)) {
    if (ncol(pts) != 4) {stop("pts must have 4 columns")}
  }
  # Make version of function for just two dimensions
  tfij <- function(x2, over1x, over2x) {
    mid2 <- rep(NaN, 4)
    mid2[-over] <- x2
    mid2[over[1]] <- over1[i]
    mid2[over[2]] <- over2[j]
    func(mid2)
  }
  # Get this function as a function
  get_tfij <- function(over1x,over2x) {
    function(x) {
      tfij(x2=x, over1x=over1x, over2x=over2x)
    }
  }
  if (same_scale) {
    # Put all plots on same scale, need to know max and min values before
    #  beginning plot, so it is twice as slow.
    zmin <- Inf
    zmax <- -Inf
    zlist <- list()
    for (j in 1:nover2) {
      zlist[[j]] <- list()
      for (i in 1:nover1) {
        zij <- eval_over_grid_with_batch(seq(low[d1],high[d1], l=n),
                                         seq(low[d2],high[d2], l=n),
                                         fn=get_tfij(over1x=over1[i],
                                                     over2x=over2[j]),
                                         batchmax)
        zlist[[j]][[i]] <- zij
        zmin <- min(zmin, min(zij))
        zmax <- max(zmax, max(zij))
      }
    }
    zlim <- c(zmin, zmax)
    levels <- pretty(zlim, nlevels)
    col <- color.palette(length(levels) - 1)
  }
  
  outer_screens <- split.screen(
    matrix(c(0,edge_width,edge_width,1,
             edge_width,1,edge_width,1,
             0,edge_width,0,edge_width,
             edge_width,1,0,edge_width), byrow=T, ncol=4))
  screen(outer_screens[2])
  
  # TODO change bar size, do separate split screen first, make it taller/wider depending on D  
  if (bar && same_scale) {
    # Make bar in top right square
    # Messed up labels when this was below plots, no clue why
    bar_screens <- split.screen(matrix(c(3/4, 1, 2/3, 1), byrow=T, ncol=4))
    screen(bar_screens[1])
    # levels <- pretty(zlim, nlevels)
    # col <- color.palette(length(levels) - 1)
    okmar <- par()$mar
    kmar <- numeric(4) #mar.orig
    kmar[4L] <- 2.5#mar[2L] # right
    kmar[1] <- 2.2 # bottom
    kmar[3] <- .83 #if (mainminmax | !is.null(main)) 1.3 else .3 #1.3#1.3 # top
    kmar[2L] <- 3#0#1 # left
    par(mar = kmar)
    kmai <- par("mai")
    kdin <- par("din")
    # avail_left <- (1-edge_width) * kdin[1]/(D-1) - kmai[4]-.3 # When put into single box
    avail_left <- (1-edge_width) * kdin[1]/4 - kmai[4]-.3
    max_bar_width <- 0.5 # inches
    min_bar_width <- 0.1 # inches
    leftmai <- if (avail_left < min_bar_width) {0}
    else if (avail_left < min_bar_width + max_bar_width) {.1}
    else {avail_left - max_bar_width}
    kmai2 <- c(.1,
               # max(.5, min(par("mai")[2], par("din")[1]/2 - par("mai")[4]-0.3)),
               leftmai,
               .1,
               par("mai")[4])
    par(mai = kmai2)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
                yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    if (missing(key.axes)) {
      if (axes) 
        axis(4, las=1)
    }
    else key.axes
    box()
    if (!missing(key.title))
      key.title
    # mar <- mar.orig
    par(mar=okmar)
    close.screen(bar_screens)
    screen(outer_screens[2], new = FALSE)
  }
  
  # Split screen for grid of plots
  par(mar=c(1,1,1,1))
  screen.numbers <- split.screen(c(nover1, nover2), erase = FALSE)
  current_screen_index <- 1
  current_screen <- screen.numbers[current_screen_index]
  
  for (j in nover2:1) {
    for (i in 1:nover1) {
      screen(current_screen)
      if (same_scale) {
        # Already calculated values, so pass them to cf_grid
        cf_grid(x=seq(low[d1], high[d1], length.out=n),
                y=seq(low[d2], high[d2], length.out=n),
                z=zlist[[j]][[i]],
                mainminmax=FALSE, xaxis=F&&(j==4), yaxis=F&&(i==1), #plot.axes=F,
                xlim=c(low[d1],high[d1]), ylim=c(low[d2],high[d2]),
                zlim=zlim, pts=pts[,c(i,j)],
                nlevels=nlevels, levels=levels,
                color.palette=color.palette, col=col,
                ...)
      } else {
        cf_func(get_tfij(over1x=over1[i], over2x=over2[i]), batchmax=batchmax,
                mainminmax=FALSE, xaxis=F, yaxis=F, #plot.axes=F,
                xlim=c(low[d1],high[d1]), ylim=c(low[d2],high[d2]),
                pts=pts[,c(i,j)],
                ...)
      }
      current_screen_index <- current_screen_index + 1
      current_screen <- screen.numbers[current_screen_index]
    }
  }
  close.screen(n = screen.numbers)
  screen(outer_screens[1])
  left_screens <- split.screen(c(nover2, 1))
  for (j in nover2:1) {
    screen(left_screens[nover2+1-j])
    text_plot(bquote(.(a)==.(b), where=list(a=var_names[[over[2]]], b=over2[j])), cex=cex.var_names, srt=90)
  }
  close.screen(left_screens)
  screen(outer_screens[4])
  right_screens <- split.screen(c(1, nover1))
  for (i in 1:nover1) {
    screen(right_screens[i])
    text_plot(bquote(.(a)==.(b), where=list(a=var_names[[over[1]]], b=over1[i])), cex=cex.var_names)
  }
  close.screen(right_screens)
  
  # close outer
  close.screen(outer_screens)
  
  
  # Return to original screen
  screen(begin_screen, new=FALSE)
  
  invisible()
}

if (F) {
  csa()
  cf_4dim(function(x) {x[1] + x[2]^2 + sin(2*pi*x[3])}, bar=F)
}