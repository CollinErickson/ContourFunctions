#' Close all open screens
#' 
#' Closes the screens open, which happens
#' when plotting with `split.screen` is interrupted.
#' It often happens when there is a error while plotting.
#' When you try to plot
#' the next thing it gives an error.
#' Running this function will reset the plot screen.
#' It just does `close.screen(all.screens=TRUE)` but is faster to type.
#' 
#' @param silent Should the output of `close.screen` not be returned?
#' 
#' @examples
#' # Split screen into fourths
#' split.screen(c(2,2))
#' hist(rnorm(100))
#' screen(2)
#' hist(runif(100))
#' # Use csa() to go back to normal plotting
#' csa()
#' hist(rexp(100))
#' @export
csa <- function(silent=FALSE) {
  if (silent) { # Suppresses "FALSE" if already closed
    invisible(close.screen(all.screens=TRUE))
  } else {
    close.screen(all.screens=TRUE)
  }
}
