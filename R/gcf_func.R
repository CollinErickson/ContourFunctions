#' Makes filled contour plot from function 
#' 
#' A contour plot of the given function without sidebar by default.
#' It calls the function `cf_grid` to make the actual plot.
#' 
#' @param fn0  function to plot, first argument must be two-dimensional
#' @param n  number of points in each dimension
#' @param xlim  x limits for the contour plot
#' @param ylim  y limits for the contour plot
#' @param xylim  x and y limits for the contour plot, use when both are same
#' #@param mainminmax  whether the min and max values should be shown in the title of plot
#' @param batchmax  number of datapoints that can be computed at a time
#' @param out.col.name  if a column needs to be selected from the function, specify it
#' @param out.name Selects with a $ the name from output to be used, for lists and data frames
#' #@param pretitle Text to be preappended to end of plot title
#' #@param posttitle Text to be appended to end of plot title
#' #@param title Title for the plot
#' #@param mainminmax_minmax Whether [min,max]= should be shown in title or just the numbers
#' @param pts Points to plot on top of contour
#' @param use_lines If the contour should be made with lines. Otherwise is made
#' using colors. Defaults to colors.
#' @param ...  Passed to cf_grid
#' @examples 
#' gcf_func(function(x){x[1]*x[2]})
#' gcf_func(function(x)(exp(-(x[1]-.5)^2-5*(x[2]-.5)^2)))
#' gcf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, bar=TRUE, color.palette=terrain.colors)
#' gcf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, bar=TRUE, mainminmax=TRUE)
#' gcf_func(function(x)(exp(-(x[1]-.5)^2-5*(x[2]-.5)^2)), use_lines=TRUE)
#' @export
gcf_func <- function(fn0, n=100,
                    xlim=c(0,1), ylim=c(0,1), xylim=NULL,
                    batchmax=1, out.col.name=NULL,
                    out.name=NULL,
                    pts=NULL,
                    ...) {
  if(!is.null(out.col.name)) {
    fn <- function(xx){fn0(xx)[,out.col.name]}
  } else if (!is.null(out.name)) {
    fn <- function(xx){fn0(xx)[[out.name]]}
  } else {
    fn <- fn0
  }
  if (!is.null(xylim)) {xlim <- ylim <- xylim}
  x <- seq(xlim[1],xlim[2],length.out = n)
  y <- seq(ylim[1],ylim[2],length.out = n)
  z <- eval_over_grid_with_batch(x, y, fn, batchmax)
  
  gcf_grid(x,y,z, pts=pts, ...)
}
