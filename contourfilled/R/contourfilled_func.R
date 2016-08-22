#' Makes filled contour plot from function without sidebar, uses contourfilled
#' @param fn0  function to plot, first argument must be two-dimensional
#' @param n  number of points in each dimension
#' @param xlim  x limits for the contour plot
#' @param ylim  y limits for the contour plot
#' @param xylim  x and y limits for the contour plot, use when both are same
#' @param mainminmax  whether the min and max values should be shown in the title of plot
#' @param batchmax  number of datapoints that can be computed at a time
#' @param out.col.name  if a column needs to be selected from the function, specify it
#' @param out.name Selects with a $ the name from output to be used, for lists and data frames
#' @param pretitle Text to be preappended to end of plot title
#' @param posttitle Text to be appended to end of plot title
#' @param title Title for the plot
#' @param mainminmax_minmax Whether [min,max]= should be shown in title or just the numbers
#' @param pts Points to plot on top of contour
#' @param ...  Passed to contourfilled
#' @examples 
#' contourfilled.func(function(x){x[1]*x[2]})
#' contourfilled.func(function(x)(exp(-(x[1]-.5)^2-5*(x[2]-.5)^2)))
#' @references
#' [1] filled.contour R function, copied function but removed part for sidebar
#' @references
#' [2] http://stackoverflow.com/questions/16774928/removing-part-of-a-graphic-in-r, answer by P Lapointe
#' @export
contourfilled.func <- function(fn0, n=100,
                               xlim=c(0,1), ylim=c(0,1), xylim=NULL,
                               mainminmax=T, batchmax=1, out.col.name=NULL,
                               out.name=NULL,
                               pretitle="", posttitle="", title=NULL,
                               mainminmax_minmax=TRUE, pts=NULL,
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
  if(mainminmax) {
    #contourfilled(x,y,z,main=paste('min = ',signif(min(z),3),', max = ',signif(max(z),3)),...)
    #contourfilled(x,y,z,main=paste('(min, max) = (',signif(min(z),3),', ',signif(max(z),3),')'),...)
    contourfilled(x,y,z)
    if(is.null(title)) {
      title_text <- c(pretitle)
      title_color <- c(1)
      if (mainminmax_minmax) {
        title_text  <- c(title_text, '[','min',      ', ','max',      '] = ')
        title_color <- c(title_color,1,  "#80FFFFFF",1,   "#FF80FFFF",1)
      }
      title_text  <- c(title_text, "[",signif(min(z),3),', ',signif(max(z),3),']',posttitle)
      title_color <- c(title_color,1,  1,               1,   1,               1,  1)
      multicolor.title(title_text,title_color)
    } else {
      multicolor.title(title, 1)
    }
    #contourfilled(x,y,z,main=paste('abcde','abc'),...)
  } else {
    contourfilled(x,y,z,...)
  }
  if (!is.null(pts)) {
    points(pts, pch=19)
  }
}