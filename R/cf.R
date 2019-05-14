#' Make contour plot from data or function
#' 
#' Simpler function for making contours with cf package.
#' Won't give argument completion, so all must be specified
#'
#' @param ... Arguments to be passed to cf_func or cf_data based on 
#' data type of first argument. If D is given as argument, then it
#' is passed to cf_highdim.
#' @param gg Should ggplot2 be used instead of base graphics?
#'
#' @return Whatever is returned from other function, probably nothing.
#' Will be a ggplot2 object if using gg=TRUE.
#' @export
#'
#' @examples
#' cf(function(x){x[1]^2 - x[2]})
#' x <- runif(20)
#' y <- runif(20)
#' z <- exp(-(x-.5)^2-5*(y-.5)^2)# + rnorm(20,0,.05)
#' cf(x,y,z)
#' cf(function(x){x[1]^2 - x[2]}, D=3)
cf <- function(..., gg=FALSE) {
  dots <- list(...)
  if (is.function(dots[[1]])) {
    if ("D" %in% names(dots)) {
      cf_highdim(...)
    } else {
      cf_func(..., gg=gg)
    }
  } else if (is.numeric(dots[[1]])) {
    cf_data(..., gg=gg)
  } else {
    stop("Data not recognized. Use cf_func for function or 
         cf_data for data or cf_grid for full grid of data.")
  }
}
