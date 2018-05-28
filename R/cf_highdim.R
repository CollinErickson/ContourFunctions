#' Plot 2D contour slices of higher dimensional functions
#' 
#' Plots a grid of contour plots.
#' Each contour plot is a contour over two dimensions with the remaining
#' dimensions set to the baseline value.
#' Similar to plots created in Hwang et al. (2018).
#'
#' @param func Function to plot contours of
#' @param D Input dimension of function
#' @param low Low input value for each dimension
#' @param high High input value for each dimension
#' @param baseline Baseline input value for each dimension
#' @param n Number of points in grid on each dimension
#' @param batchmax  number of datapoints that can be computed at a time
#' @param same_scale Should all contour plots be on the same scale?
#' @param var_names Variable names to add to plot
#' Takes longer since it has to precalculate range of outputs.
#' @param pts Matrix of points to show on plot
#' @param average Should the background dimensions be averaged over instead of
#' set to baseline value? Much slower.
#' @param average_reps Number of points to average over when using average
#' @param key.title  statements which add titles for the plot key.
#' @param key.axes  statements which draw axes on the plot key. This overrides the default axis.
#' @param axes  logical indicating if axes should be drawn, as in plot.default.
#' @param nlevels  if levels is not specified, the range of z, values is
#' divided into approximately this many levels.
#' @param color.palette  a color palette function to be used to assign colors
#' in the plot. Defaults to cm.colors. Other options include rainbow,
#' heat.colors, terrain.colors, topo.colors, and function(x) {gray((1:x)/x)}.
#' @param ... Arguments passed to cf_func, and then probably through to cf_grid
#'
#' @importFrom graphics contour mtext
#' @return NULL
#' @export
#' @references Hwang, Yongmoon, Sang-Lyul Cha, Sehoon Kim, Seung-Seop Jin,
#' and Hyung-Jo Jung. "The Multiple-Update-Infill Sampling Method Using
#' Minimum Energy Design for Sequential Surrogate Modeling."
#' Applied Sciences 8, no. 4 (2018): 481.
#'
#' @examples
#' \dontrun{
#' # Only use 4 dims of 8 for borehole function
#' cf_highdim(function(x) TestFunctions::borehole(c(x,.5,.5,.5,.5)), 4)
#' # Add points
#' cf_highdim(function(x) TestFunctions::borehole(c(x,.5,.5,.5,.5)), 4,
#'            pts=matrix(c(.1,.3,.6,.9),1,4))
#' 
#' # Full 8D borehole function
#' cf_highdim(TestFunctions::borehole, 8)
#' 
#' # Putting each plot on separate scale
#' cf_highdim(TestFunctions::borehole, 8, n=10, same_scale = FALSE)
#' }
#' 
#' cf_highdim(function(x) {x[1]^2 + exp(x[2])}, D=3)
#' 
#' friedman <- function(x) {
#'   10*sin(pi*x[1]*x[2]) + 20*(x[3]-.5)^2 + 10*x[4] + 5*x[5]
#' }
#' cf_highdim(friedman, 5, color.palette=topo.colors)
#' cf_highdim(friedman, 5, 
#'            color.palette=function(x) {gray((1:x)/x)},
#'            nlevels=10)
#'            
#' \dontrun{
#' # Recreate Plate 1 or Figure 1.1 from Engineering Design via Surrogate
#' # Modelling by Forrester, Sobester, and Keane (2008).
#' cf_highdim(function(x)TestFunctions::wingweight(x, scale_it=FALSE),
#'   D=10, low = c(150,220,6,-10,16,.5,.08,2.5,1700,.025),
#'   high = c(200,300,10,10,45,1,.18,6,2500,.08),
#'   baseline=c(174,252,7.52,0,34,.672,.12,3.8,2000,.064),
#'   color.palette=topo.colors, 
#'   var_names=c('SW', 'Wtw', 'A', 'Lambda', 'q', 'lambda', 'tc', 'Nz', 'Wdg'))
#' }
#' 
#' # Average over background dimensions, use higher reps to reduce noise.
#' f1 <- function(x) {x[1] + x[2]^2 + x[3]^3}
#' cf_highdim(f1, 4, average=TRUE, average_reps=1e2, n=10)
#' f1b <- function(x) {x[,1] + x[,2]^2 + x[,3]^3}
#' cf_highdim(f1b, 4, average=TRUE, average_reps=1e2, n=10, batchmax=Inf)
#' cf_highdim(f1b, 4, average_reps=1e2, n=10, batchmax=Inf,
#'            color.palette = topo.colors, nlevels=3)
#' 
#' # This was giving bad result
#' csa()
#' split.screen(c(2,1))
#' screen(2)
#' cf_highdim(f1b, 4, n=10, batchmax=Inf)
#' csa()
cf_highdim <- function(func, D, low=rep(0,D), high=rep(1,D),
                       baseline=(low+high)/2, same_scale=TRUE,
                       n=20,
                       batchmax=1,
                       # var_names=paste0("x",1:D),
                       var_names=c(expression(), 
                                   lapply(1:D,
                                          function(ti) bquote(x[.(ti)]))),
                       pts=NULL,
                       average=FALSE, average_reps=1e4,
                       axes=TRUE, key.axes, key.title,
                       nlevels=20,
                       color.palette=cm.colors,
                       ...) {
  # TODO put var_names in sep row/col
  # To put them all on same scale, need range of values first
  begin_screen <- screen()
  if (!is.null(pts)) {
    if (ncol(pts) != D) {stop("pts must have D columns")}
  }
  # Make version of function for just two dimensions
  
  if (average) {
    # Average over hidden dimensions
    funcij <- function(xx, i, j) {
      # There's a double batch issue: matrix of points to eval,
      #  then I use matrices for the average. So instead I'll
      #  force the first into vectors
      if (is.matrix(xx)) {return(apply(xx, 1, funcij, i=i, j=j))}
      ds <- c(i, j)
      notds <- setdiff(1:D, ds)
      XX <- lhs::randomLHS(average_reps, D-2)
      X4 <- matrix(nrow=average_reps, ncol=4)
      X4[, ds[1]] <- xx[1]
      X4[, ds[2]] <- xx[2]
      X4[, notds] <- XX
      if (batchmax > nrow(X4)) {
        mean(func(X4))
      } else if (batchmax > 1) { # Tricky case, need to split into groups
        sum(sapply(1:ceiling(nrow(X4)/batchmax),
               function(k) {
                 sum(
                   func(X4[(1+(k-1)*batchmax):min(nrow(X4), k*batchmax),])
                 )
               })) / nrow(X4)
      } else { # batchmax == 1
        mean(apply(X4, 1, func))
      }
    }
  } else {
    funcij <- function(xx, i, j) {
      if (batchmax == 1) {
        mid2 <- baseline
        mid2[i] <- xx[1]
        mid2[j] <- xx[2]
      } else { # batchmax > 1, use matrix
        mid2 <- matrix(baseline, nrow=nrow(xx), ncol=D, byrow=T)
        mid2[,i] <- xx[,1]
        mid2[,j] <- xx[,2]
      }
      func(mid2)
    }
  }
  # Get this function as a function
  get_funcij <- function(i,j) {
    function(x) {
      funcij(xx=x, i=i, j=j)
    }
  }
  if (same_scale) {
    # Put all plots on same scale, need to know max and min values before
    #  beginning plot, so it is twice as slow.
    zmin <- Inf
    zmax <- -Inf
    zlist <- list()
    for (j in 2:D) {
      y <- seq(low[j], high[j], length.out=n)
      zlist[[j]] <- list()
      for (i in 1:(j-1)) {
        x <- seq(low[i], high[i], length.out=n)
        zij <- eval_over_grid_with_batch(x, y, fn=get_funcij(i=i,j=j), batchmax)
        zlist[[j]][[i]] <- zij
        zmin <- min(zmin, min(zij))
        zmax <- max(zmax, max(zij))
      }
    }
    zlim <- c(zmin, zmax)
  }
  
  par(mar=c(1,1,1,1))
  screen.numbers <- split.screen(c(D-1, D-1))
  current_screen_index <- 1
  current_screen <- screen.numbers[current_screen_index]
  
  if (same_scale) {
    # Make bar in top right square
    # Messed up labels when this was below plots, no clue why
    screen(screen.numbers[D-1])
    levels <- pretty(zlim, nlevels)
    col <- color.palette(length(levels) - 1)
    okmar <- par()$mar
    kmar <- numeric(4) #mar.orig
    kmar[4L] <- 2.5#mar[2L] # right
    kmar[1] <- 2.2 # bottom
    kmar[3] <- .83 #if (mainminmax | !is.null(main)) 1.3 else .3 #1.3#1.3 # top
    kmar[2L] <- 3#0#1 # left
    par(mar = kmar)
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
  }
  
  for (j in 2:D) {
    for (i in 1:(j-1)) {
      ds <- c(i, j)
      notds <- setdiff(1:D, ds)
      screen(current_screen)
      if (same_scale) {
        # Already calculated values, so pass them to cf_grid
        # cf_func(get_funcij(i=i,j=j), batchmax=batchmax,
        #         mainminmax=FALSE, plot.axes=F,
        #         xlim=c(low[i],high[i]), ylim=c(low[j],high[j]),
        #         zlim=zlim, pts=pts[,c(i,j)], ...)
        cf_grid(x=seq(low[i], high[i], length.out=n),
                y=seq(low[j], high[j], length.out=n),
                z=zlist[[j]][[i]],
                mainminmax=FALSE, xaxis=F, yaxis=F, #plot.axes=F,
                xlim=c(low[i],high[i]), ylim=c(low[j],high[j]),
                zlim=zlim, pts=pts[,c(i,j)],
                nlevels=nlevels, levels=levels,
                color.palette=color.palette, col=col,
                ...)
      } else {
        cf_func(get_funcij(i=i,j=j), batchmax=batchmax,
                mainminmax=FALSE, xaxis=F, yaxis=F, #plot.axes=F,
                xlim=c(low[i],high[i]), ylim=c(low[j],high[j]),
                pts=pts[,c(i,j)],
                ...)
      }
      current_screen_index <- current_screen_index + 1
      current_screen <- screen.numbers[current_screen_index]
    }
    if (j < D) {
      for (k in 1:(D - j)) {
        current_screen_index <- current_screen_index + 1
        current_screen <- screen.numbers[current_screen_index]
      }
    }
  }
  close.screen(n = screen.numbers)
  # Return to original screen
  screen(begin_screen, new=FALSE)
  
  # Add variable names
  for (j in 2:D) {
    mtext(var_names[j], 2, at=(D-j+.5)/(D-1)*1.14-.07)
  }
  for (i in 1:(D-1)) {
    mtext(var_names[i], 1, at=(i-.5)/(D-1)*1.14-.07)
  }
}
if (F) {
  close.screen(all.screens = TRUE)
  # cf_highdim(function(x) TestFunctions::borehole(c(x,.5,.5,.5,.5)), 4)
  cf_highdim(TestFunctions::borehole, 8)
}

# To get this to work, in cf_grid_screen, change Axis(x,1) part so that you can have it
#  only set one or other. Then only set values for y on left plots and x on right