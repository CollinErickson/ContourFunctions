# I think cm.colors is good but sometimes too light.

#' Strong version of cm.colors color palette
#' 
#' Altered version of cm.colors that uses
#' full saturation to get stronger colors.
#'
#' @param n Number of color groups
#' @param alpha Alpha level
#'
#' @return Character vector of colors
#' @export
#'
#' @examples
#' # Character string output
#' cm.colors.strong(5)
#' 
#' # Plot to show these
#' sl <- 21
#' sx <- seq(0,1,l=sl)
#' plot(sx,sin(2*pi*sx), cex=5, col=cm.colors.strong(sl), pch=19);points(sx,sin(2*pi*sx), cex=5)
#' plot(sx,sin(2*pi*sx), cex=5, col=cm.colors(sl),        pch=19);points(sx,sin(2*pi*sx), cex=5)
cm.colors.strong <- function (n, alpha = 1) {
  if ((n <- as.integer(n[1L])) > 0L) {
    even.n <- n%%2L == 0L
    k <- n%/%2L
    l1 <- k + 1L - even.n
    l2 <- n - k + even.n
    c(if (l1 > 0L) grDevices::hsv(h = 6/12, s = seq.int(0.99, ifelse(even.n, 
                                                         0.99/k, 0), length.out = l1), v = 1, alpha = alpha), 
      if (l2 > 1) grDevices::hsv(h = 10/12, s = seq.int(0, 0.99, length.out = l2)[-1L], 
                      v = 1, alpha = alpha))
  }
  else character()
}