#' scr4_9
#'
#' @description
#' The solution to the exercise 4_9 for Rizzo's Book.
#'
#' Refer to the full leafshape (DAAG) data set. Produce Andrews curves for each of the six
#' locations. Split the screen into six plotting areas, and display all six plots on one screen.
#' Set line type or color to identify leaf architecture.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#'
#' @return plot
#' @export
#' @import DAAG
#'
#'
#' @examples scr4_9()
#'
scr4_9<-function(){
  leafshape17<-DAAG::leafshape17
  attach(leafshape17)
  f <- function(a, v) {
    v/sqrt(2) + v*sin(a) + v*cos(a)
  }
  itm<-function(x){n <- nrow(x)
  mins <- apply(x, 2, min)
  maxs <- apply(x, 2, max)
  r <- maxs - mins
  y <- sweep(x, 2, mins)
  y <- sweep(y, 2, r, "/")
  x <- 2 * y - 1
  plot(0, 0, xlim = c(-pi, pi), ylim = c(-3,3),
       xlab = "t", ylab = "Andrews Curves",
       main = "", type = "n")
  a <- seq(-pi, pi, len=101)
  dim(a) <- length(a)
  for (i in 1:n) {
    g <- arch[i] + 1
    y <- apply(a, MARGIN = 1, FUN = f, v = x[i,])
    lines(a, y, lty = g)
  }}
  par(mar=rep(2,4))
  par(mfrow=c(2,3))
  itm(matrix(bladelen,ncol=1))
  itm(matrix(petiole,ncol=1))
  itm(matrix(bladewid,ncol=1))
  itm(matrix(logwid,ncol=1))
  itm(matrix(logpet,ncol=1))
  itm(matrix(loglen,ncol=1))
  detach(leafshape17)
}
