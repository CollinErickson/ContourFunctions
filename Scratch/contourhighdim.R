#' Plot contour functions from 2D slices of higher dimensional functions
#' 
#' Plots a grid of contour plots.
#' Each contour plot is a contour over two dimensions with the remaining
#' dimensions set to the baseline value. 
#'
#' @param func Function to plot contours of
#' @param D Input dimension of function
#' @param low Low input value for each dimension
#' @param high High input value for each dimension
#' @param baseline Baseline input value for each dimension
#'
#' @return NULL
#' @export
#'
#' @examples
#' # Only use 4 dims of 8 for borehole function
#' contourhighdim(function(x) TestFunctions::borehole(c(x,.5,.5,.5,.5)), 4)
#' 
#' # Full 8D borehole function
#' contourhighdim(TestFunctions::borehole, 8)
contourhighdim <- function(func, D, low=rep(0,D), high=rep(1,D), baseline=(low+high)/2) {
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
      cf_func(tf, batchmax=1, mainminmax=FALSE, plot.axes=F) #(j==D || i==1))
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
    mtext(paste0("x",j), 2, at=(D-j+.5)/(D-1))
  }
  for (i in 1:(D-1)) {
    mtext(paste0("x",i), 1, at=(i-.5)/(D-1))
  }
}
close.screen(all.screens = TRUE)
# contourhighdim(function(x) TestFunctions::borehole(c(x,.5,.5,.5,.5)), 4)
contourhighdim(TestFunctions::borehole, 8)

# To get this to work, in cf_grid_screen, change Axis(x,1) part so that you can have it
#  only set one or other. Then only set values for y on left plots and x on right