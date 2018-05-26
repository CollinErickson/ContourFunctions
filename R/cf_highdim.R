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
#' cf_highdim(f1, 4, average=FALSE, average_reps=1e2, n=10)
#' f1b <- function(x) {x[,1] + x[,2]^2 + x[,3]^3}
#' cf_highdim(f1b, 4, average=FALSE, average_reps=1e2, n=10, batchmax=Inf)
cf_highdim <- function(func, D, low=rep(0,D), high=rep(1,D),
                       baseline=(low+high)/2, same_scale=TRUE,
                       n=20,
                       batchmax=1,
                       var_names=paste0("x",1:D),
                       pts=NULL,
                       average=FALSE, average_reps=1e4,
                       ...) {
  # To put them all on same scale, need range of values first
  if (!is.null(pts)) {
    if (ncol(pts) != D) {stop("pts must have D columns")}
  }
  # browser()
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
      # xD <- numeric(D)
      # xD[ds] <- xx
      XX <- lhs::randomLHS(average_reps, D-2)
      X4 <- matrix(nrow=average_reps, ncol=4)
      X4[, ds[1]] <- xx[1]
      X4[, ds[2]] <- xx[2]
      X4[, notds] <- XX
      # if (average_matrix) {
      #   mean(func(X4))
      # } else {
      #   mean(apply(X4, 1, func))
      # }
      if (batchmax > nrow(X4)) {
        mean(func(X4))
      } else if (batchmax > 1) {
        # warning("tricky case")
        
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
    # for (j in 2:D) {
    #   for (i in 1:(j-1)) {
    #     tfouter <- Vectorize(function(xa, xb) {
    #       mid2 <- baseline
    #       mid2[i] <- xa
    #       mid2[j] <- xb
    #       func(mid2)
    #     })
    #     tv <- outer(X = seq(low[i], high[i], length.out=n),
    #                 Y = seq(low[j], high[j], length.out=n),
    #                 tfouter)
    #     zmin <- min(zmin, min(tv))
    #     zmax <- max(zmax, max(tv))
    #   }
    # }
    # zlim <- c(zmin, zmax)
    # Use eval_over_grid_with_batch
    zlist <- list()
    for (j in 2:D) {
      # TODO Check i and j vs x and y
      y <- seq(low[j], high[j], length.out=n)
      zlist[[j]] <- list()
      for (i in 1:(j-1)) {
        x <- seq(low[i], high[i], length.out=n)
        # tfouter <- Vectorize(function(xa, xb) {
        #   mid2 <- baseline
        #   mid2[i] <- xa
        #   mid2[j] <- xb
        #   func(mid2)
        # })
        # if (batchmax == 1) {
        #   tfouter <- function(xx) {
        #     mid2 <- baseline
        #     mid2[i] <- xx[1]
        #     mid2[j] <- xx[2]
        #     func(mid2)
        #   }
        # } else {# batchmax > 1, use matrix
        #   tfouter <- function(xx) {browser()
        #     mid2 <- matrix(baseline, nrow=nrow(xx), ncol=D, byrow=T)
        #     mid2[,i] <- xx[,1]
        #     mid2[,j] <- xx[,2]
        #     func(mid2)
        #   }
        # }
        zij <- eval_over_grid_with_batch(x, y, fn=get_funcij(i=i,j=j), batchmax)
        zlist[[j]][[i]] <- zij
        zmin <- min(zmin, min(zij))
        zmax <- max(zmax, max(zij))
      }
    }
    zlim <- c(zmin, zmax)
  }
  # print(low);print(high); print(baseline)
  # opar <- par()
  # par(mfrow=c(D-1,D-1)
  #     , mar=c(1,1,1,1)
  # )
  par(mar=c(1,1,1,1))
  split.screen(c(D-1, D-1))
  current_screen <- 1
  # screen(1)
  # plot(rexp(5))
  # browser()
  for (j in 2:D) {
    for (i in 1:(j-1)) {
      ds <- c(i, j)
      notds <- setdiff(1:D, ds)
      screen(current_screen)
      # plot(rnorm(10), xlab=i, ylab=j)
      if (average) {
        # # Average over hidden dimensions
        # tf <- function(x2) {
        #   xD <- numeric(D)
        #   xD[ds] <- x2
        #   XX <- lhs::randomLHS(average_reps, D-2)
        #   X4 <- matrix(nrow=average_reps, ncol=4)
        #   X4[, ds[1]] <- x2[1]
        #   X4[, ds[2]] <- x2[2]
        #   X4[, notds] <- XX
        #   if (average_matrix) {
        #     mean(func(X4))
        #   } else {
        #     mean(apply(X4, 1, func))
        #   }
        # }
      } else { # Just use baseline
        # tf <- function(x2) {
        #   mid2 <- baseline
        #   mid2[i] <- x2[1]
        #   mid2[j] <- x2[2]
        #   func(mid2)
        # }
      }
      # browser()
      if (same_scale) {
        # TODO Need to check i and j vs x and y, flipped?
        # cf_func(get_funcij(i=i,j=j), batchmax=batchmax,
        #         mainminmax=FALSE, plot.axes=F,
        #         xlim=c(low[i],high[i]), ylim=c(low[j],high[j]),
        #         zlim=zlim, pts=pts[,c(i,j)], ...)
        # browser()
        cf_grid(x=seq(low[i], high[i], length.out=n),
                y=seq(low[j], high[j], length.out=n),
                z=zlist[[j]][[i]],
                mainminmax=FALSE, plot.axes=F,
                xlim=c(low[i],high[i]), ylim=c(low[j],high[j]),
                zlim=zlim, pts=pts[,c(i,j)], ...)
      } else {
        cf_func(get_funcij(i=i,j=j), batchmax=batchmax,
                mainminmax=FALSE, plot.axes=F,
                xlim=c(low[i],high[i]), ylim=c(low[j],high[j]),
                pts=pts[,c(i,j)],
                ...)
      }
      current_screen <- current_screen + 1
      # close.screen()
    }
    if (j < D) {
      for (k in 1:(D - j)) {
        # plot.new()
        current_screen <- current_screen + 1
        # close.screen()
      }
    }
  }
  close.screen(all.screens = TRUE)
  # par(mfrow=opar$mfrow, mar=opar$mar)
  
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