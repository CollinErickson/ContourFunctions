#' Make a plot with only text
#' 
#' 
#' @param p Text to put on a plot
#' @param x x-value of center of text, defaults to center
#' @param y y-value of center of text, defaults to center
#' @param cex Size of text
#' @param ... Arguments passed to plot
#' @export
#' @importFrom graphics plot text
#'
#' @references ZNK's answer on https://stackoverflow.com/questions/19918985/r-plot-only-text, retrieved 5/25/2018
#' @examples 
#' text_plot("Useful?", cex=5)
text_plot <- function(p, x=0.5, y=0.5, cex=2, ...) {
  omar <- par()$mar
  par(mar = c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  # text(x = 0.5, y = 0.5, paste("The following is text that'll appear in a plot window.\n",
  #                              "As you can see, it's in the plot window\n",
  #                              "One might imagine useful informaiton here"), 
  #      cex = 1.6, col = "black")
  text(x = x, y = y, p, cex=cex, ...)
  par(mar=omar)
}
