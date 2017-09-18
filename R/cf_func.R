#' Makes filled contour plot from function 
#' 
#' A contour plot of the given function without sidebar by default.
#' It call the function `cf_grid` to make the actual plot.
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
#' @param ...  Passed to cf_grid
#' @examples 
#' cf_func(function(x){x[1]*x[2]})
#' cf_func(function(x)(exp(-(x[1]-.5)^2-5*(x[2]-.5)^2)))
#' cf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, bar=TRUE)
#' cf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, bar=TRUE, mainminmax=TRUE)
#' @references
#' [1] filled.contour R function, copied function but removed part for sidebar
#' @references
#' [2] http://stackoverflow.com/questions/16774928/removing-part-of-a-graphic-in-r, answer by P Lapointe
#' @export
cf_func <- function(fn0, n=100,
                               xlim=c(0,1), ylim=c(0,1), xylim=NULL,
                               #mainminmax=T, 
                               batchmax=1, out.col.name=NULL,
                               out.name=NULL,
                               #pretitle="", posttitle="", title=NULL,
                               #mainminmax_minmax=TRUE, 
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
  z <- matrix(NA,n,n)
  if(batchmax<=1) { # calculate single Z value at a time
    #for(xi in 1:n) for(yi in 1:n) {z[xi,yi] <- fn(c(x[xi],y[yi]))}
    fn_outer <- Vectorize(function(xi, yi) {fn(c(x[xi], y[yi]))})
    z <- outer(1:n, 1:n, fn_outer)
  } else {
    inbatch <- 0
    for(xi in 1:n) {
      for(yi in 1:n) {
        if(inbatch==0) XYi <- matrix(c(xi,yi),ncol=2)
        else XYi <- rbind(XYi,matrix(c(xi,yi),ncol=2))
        inbatch <- inbatch + 1
        if(inbatch == batchmax | (xi==n & yi==n)) {
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
  #if(mainminmax) {
    #cf_grid3(x,y,z, pts=pts, reset.par=T, ...)
    # cf_grid3(x,y,z, pts=pts, ...)
    cf_grid(x,y,z, pts=pts, ...)
    #par.reset.func <- cf_grid3(x,y,z, pts=pts, reset.par=F, ...)
    #on.exit(par.reset.func)#;browser()
    # make.multicolor.title(title=title, z=z, pretitle=pretitle, posttitle=posttitle, mainminmax_minmax=mainminmax_minmax)

    #cf_grid(x,y,z,main=paste('abcde','abc'),...)
  #} else {
  #  cf_grid3(x,y,z, pts=pts, ...)
  #}
  #if (!is.null(pts)) { # This is now done in cf_grid
  #  points(pts, pch=19)
  #}
}
