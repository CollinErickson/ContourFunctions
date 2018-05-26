#' Evaluate function over grid of points
#' 
#' `batchmax` gives how many can be evaluated at a time.
#' If more than 1, then the input is given to the function
#' as rows of a matrix.
#' 
#'
#' @param x Vector of x values to evaluate
#' @param y Vector of y values to evaluate
#' @param fn Function that takes in a length two vector if `batchmax` is 1
#' or a matrix with two columns if greater than 1.
#' @param batchmax Number of points that can evaluated simultaneously.
#' If 1, points are passed to `fn` as a vector of length two.
#' If greater than 1, points are passed to `fn` as rows of a matrix.
#'
#' @return Matrix of size `length(x)` by `length(y)`
#' @export
#'
#' @examples
#' eval_over_grid_with_batch(c(0,.5,1), c(10,20,30), function(a)a[1]+a[2], batchmax=1)
#' eval_over_grid_with_batch(c(0,.5,1), c(10,20,30), function(a)a[,1]+a[,2], batchmax=Inf)
eval_over_grid_with_batch <- function(x, y, fn, batchmax) {
  nx <- length(x)
  ny <- length(y)
  if(batchmax<=1) { # calculate single Z value at a time
    #for(xi in 1:n) for(yi in 1:n) {z[xi,yi] <- fn(c(x[xi],y[yi]))}
    fn_outer <- Vectorize(function(xi, yi) {fn(c(x[xi], y[yi]))})
    z <- outer(1:nx, 1:ny, fn_outer)
  } else {
    z <- matrix(NA,nx,ny)
    inbatch <- 0
    for(xi in 1:nx) {
      for(yi in 1:ny) {
        if(inbatch==0) XYi <- matrix(c(xi,yi),ncol=2)
        else XYi <- rbind(XYi,matrix(c(xi,yi),ncol=2))
        inbatch <- inbatch + 1
        if(inbatch == batchmax | (xi==nx & yi==ny)) {
          Zbatch <- fn(matrix(c(x[XYi[,1]],y[XYi[,2]]),ncol=2,byrow=F))
          for(rowbatch in 1:length(Zbatch)) {
            z[XYi[rowbatch,1],XYi[rowbatch,2]] <- Zbatch[rowbatch]
          }
          inbatch <- 0
          rm(XYi)
        }
      }
    }
  }
  z
}

