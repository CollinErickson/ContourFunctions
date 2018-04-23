#' Plot 2D contour slices of four dimensional functions
#' 
#' Plots a grid of contour plots.
#' Each contour plot is a contour over two dimensions with the remaining
#' two dimensions set to the baseline value.
#' See cf_highdim for functions with more than 4 dimensions.
#'
#' @param func Function to plot contours of
#' @param low Low input value for each dimension
#' @param high High input value for each dimension
#' @param baseline Baseline input value for each dimension
#' @param n Number of points in grid on each dimension
#' @param same_scale Should all contour plots be on the same scale?
#' @param var_names Variable names to add to plot
#' Takes longer since it has to precalculate range of outputs.
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
                       over1=seq(0,1,length.out=nover1),
                       over2=seq(0,1,length.out=nover2),
                       low=rep(0,4), high=rep(1,4),
                    nlevels=20,
                       # baseline=(low+high)/2,
                       same_scale=F,
                       n=20,
                       var_names=paste0("x",1:4),
                    bar=T,
                       ...) {
  # browser()
  # To put them all on same scale, need range of values first
  if (same_scale) {
    zmin <- Inf
    zmax <- -Inf
    for (j in 1:nover2) {
      for (i in 1:nover1) {
        tfouter <- Vectorize(function(xa, xb) {
          mid2 <- rep(NaN, 4)
          mid2[-over] <- c(xa, xb)
          mid2[over[1]] <- over1[i]
          mid2[over[2]] <- over2[j]
          func(mid2)
        })
        tv <- outer(X = over1,
                    Y = over2,
                    tfouter)
        zmin <- min(zmin, min(tv))
        zmax <- max(zmax, max(tv))
      }
    }
    zlim <- c(zmin, zmax)
  }
  # print(low);print(high); print(baseline)
  # opar <- par()
  # par(mfrow=c(D-1,D-1)
  #     , mar=c(1,1,1,1)
  # )
  if (bar) {browser()
    #split.screen(c(1,2))
    screen.numbers <- split.screen(matrix(c(0,.85,0,1,.85,1,0,1), ncol=4, byrow=T))
    screen1 <- screen.numbers[1]
    screen2 <- screen.numbers[2]
    screen(screen2)
    # mar <- mar.orig
    mar <- par()$mar
    mar[4L] <- 2.5#mar[2L] # right
    mar[1] <- 2.2 # bottom
    mar[3] <- .3 #if (mainminmax | !is.null(main)) 1.3 else .3 #1.3#1.3 # top
    mar[2L] <- 0#1 # left
    par(mar = mar)
    plot.new()
    levels = pretty(zlim, nlevels)
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
                yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    # if (missing(key.axes)) {
    #   if (axes) 
    #     axis(4)
    # }
    # else key.axes
    box()
    if (!missing(key.title))
      key.title
    mar <- mar.orig
    mar[4L] <- 1 # right # Why is this here?
    close.screen(screen2)
    screen(screen1)
  }
  par(mar=c(1,1,1,1))
  split.screen(c(nover1, nover2))
  current_screen <- 1
  # screen(1)
  # plot(rexp(5))
  # browser()
  d1 <- (1:4)[-over][1]
  d2 <- (1:4)[-over][2]
  for (j in nover2:1) {
    for (i in 1:nover1) {
      screen(current_screen)
      # plot(rnorm(10), xlab=i, ylab=j)
      tf <- function(x2) {
        mid2 <- rep(NaN, 4)
        mid2[-over] <- x2
        mid2[over[1]] <- over1[i]
        mid2[over[2]] <- over2[j]
        func(mid2)
      }
      # browser()
      if (same_scale) {
        cf_func(tf, batchmax=1, mainminmax=FALSE, plot.axes=F,
                xlim=c(low[d1],high[d1]), ylim=c(low[d2],high[d2]),
                zlim=zlim, ...)
      } else {
        cf_func(tf, batchmax=1, mainminmax=FALSE, plot.axes=F,
                xlim=c(low[d1],high[d1]), ylim=c(low[d2],high[d2]),
                ...)
      }
      current_screen <- current_screen + 1
      # close.screen()
    }
    # if (j < D) {
      # for (k in 1:(D - j)) {
        # plot.new()
        # current_screen <- current_screen + 1
        # close.screen()
      # }
    # }
  }
  close.screen(all.screens = TRUE)
  # par(mfrow=opar$mfrow, mar=opar$mar)
  
  # Add variable names
  for (i in 1:nover1) {
    mtext(paste0(var_names[over[1]], "=", over1[i]), 1, at=(i-.5)/(nover1)*1.14-.07)
  }
  for (j in 1:nover2) {
    mtext(paste0(var_names[over[2]], "=", over2[j]), 2, at=(j-.5)/(nover2)*1.14-.07)
  }
}
if (F) {
  close.screen(all.screens = TRUE)
  # cf_4dim(function(x) TestFunctions::borehole(c(x,.5,.5,.5,.5)), 4)
  cf_4dim(TestFunctions::borehole, 8)
  cf_4dim(
       function(x) {x[1] + x[2]^2 + sin(2*pi*x[3])}
    )
}

# To get this to work, in cf_grid_screen, change Axis(x,1) part so that you can have it
#  only set one or other. Then only set values for y on left plots and x on right