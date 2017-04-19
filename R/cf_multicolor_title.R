#' Makes plot title using specified colors for the text
#' @param main  Text to put in main title of plot
#' @param col.main  Colors to use for the text
#' @param collapse  What to put between elements of main, defaults to "" but " " might be appropriate
#' @param cex.main The size of the main title. 1.2 is default.
#' @examples 
#' plot(1:4)
#' multicolor.title(c('Black, ','red, ','green'),c(1,2,3))
#' @export
multicolor.title <- function(main,col.main, collapse='', cex.main=par()$cex.main) {
  if (length(main) != length(col.main)) {stop('main and col must have same length')}
  n <- length(main)
  if(n==1) {
    title(bquote(.(main[1])),col.main=col.main[1], cex.main=cex.main)
  } else {
    # print first
    title(bquote(.(main[1]) * phantom(.(paste0(main[2:n],collapse=collapse)))),col.main=col.main[1], cex.main=cex.main)
    
    # print middle
    if(n > 2) {
      for(i in 2:(n-1)) {
        title(bquote(phantom(.(paste0(main[1:(i-1)],collapse=collapse))) * .(main[i]) * phantom(.(paste0(main[(i+1):n],collapse=collapse)))),col.main=col.main[i], cex.main=cex.main) 
      }
    }
    
    # print last
    title(bquote(phantom(.(paste0(main[1:(n-1)],collapse=collapse))) * .(main[n])),col.main=col.main[n], cex.main=cex.main)
  }
}