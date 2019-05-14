#' Make contour plot from data or function using ggplot2
#' 
#' Simpler function for making contours with cf package.
#' Won't give argument completion, so all must be specified
#'
#' @param ... Arguments to be passed to cf_func or cf_data based on 
#' data type of first argument. If D is given as argument, then it
#' is passed to cf_highdim.
#'
#' @return Whatever is returned from other function, probably nothing.
#' Will be a ggplot2 object if using gg=TRUE.
#' @export
#'
#' @examples
#' gcf(function(x){x[1]^2 - x[2]})
#' x <- runif(20)
#' y <- runif(20)
#' z <- exp(-(x-.5)^2-5*(y-.5)^2)# + rnorm(20,0,.05)
#' gcf(x,y,z)
#' gcf(function(x){x[1]^2 - x[2]}, D=3)
gcf <- function(...) {
  cf(..., gg=TRUE)
}
