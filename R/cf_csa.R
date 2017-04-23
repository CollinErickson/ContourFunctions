#' Closes the screens open if plotting with split.screen is interrupted.
#' Happens often when there is a plotting error, then when you try to plot
#' the next thing it gives an error. Running this function will reset.
#' It just does close.screen(all.screens=TRUE) but is faster to type.
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
csa <- function() {
  close.screen(all.screens=TRUE)
}