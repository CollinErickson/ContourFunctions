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
#' @param same_scale Should all contour plots be on the same scale?
#' Takes longer since it has to precalculate range of outputs.
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
cf_highdim <- function(func, D, low=rep(0,D), high=rep(1,D),
                       baseline=(low+high)/2, same_scale=TRUE,
                       n=20,
                       var_names=paste0("x",1:D),
                       ...) {
  # To put them all on same scale, need range of values first
  if (same_scale) {
    zmin <- Inf
    zmax <- -Inf
    for (j in 2:D) {
      for (i in 1:(j-1)) {
        tfouter <- Vectorize(function(xa, xb) {
          mid2 <- baseline
          mid2[i] <- xa
          mid2[j] <- xb
          func(mid2)
        })
        tv <- outer(X = seq(low[i], high[i], length.out=n),
                    Y = seq(low[j], high[j], length.out=n),
                    tfouter)
        zmin <- min(zmin, min(tv))
        zmax <- max(zmax, max(tv))
      }
    }
    zlim <- c(zmin, zmax)
  }
  print(low);print(high); print(baseline)
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
      screen(current_screen)
      # plot(rnorm(10), xlab=i, ylab=j)
      tf <- function(x2) {
        mid2 <- baseline
        mid2[i] <- x2[1]
        mid2[j] <- x2[2]
        func(mid2)
      }
      # browser()
      if (same_scale) {
        cf_func(tf, batchmax=1, mainminmax=FALSE, plot.axes=F,
                xlim=c(low[i],high[i]), ylim=c(low[j],high[j]),
                zlim=zlim, ...)
      } else {
        cf_func(tf, batchmax=1, mainminmax=FALSE, plot.axes=F,
                xlim=c(low[i],high[i]), ylim=c(low[j],high[j]),
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