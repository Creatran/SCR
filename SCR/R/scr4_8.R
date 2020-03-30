#' scr4_8
#'
#' @description
#' The solution to the exercise 4_8 for Rizzo's Book.
#'
#' Create a plot of Andrews curves for the leafshape17 (DAAG) [185] data, using the logarithms of
#' measurements (logwid, logpet, loglen). Set line type to identify leaf architecture as in
#' Example 4.9. Compare with the plot in Figure 4.8.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#'
#' @return plot
#' @export
#' @import DAAG
#'
#'
#' @examples scr4_8()
scr4_8<-function(){
  leafshape17<-DAAG::leafshape17
  attach(leafshape17)
  f <- function(a, v) {
    v[1]/sqrt(2) + v[2]*sin(a) + v[3]*cos(a)
  }
  x <- cbind(logwid, logpet, loglen)
  n <- nrow(x)
  mins <- apply(x, 2, min)
  maxs <- apply(x, 2, max)
  r <- maxs - mins
  y <- sweep(x, 2, mins)
  y <- sweep(y, 2, r, "/")
  x <- 2 * y - 1
  plot(0, 0, xlim = c(-pi, pi), ylim = c(-3,3),
       xlab = "t", ylab = "Log-tran Andrews Curves",
       main = "", type = "n")
  a <- seq(-pi, pi, len=101)
  dim(a) <- length(a)
  for (i in 1:n) {
    g <- arch[i] + 1
    y <- apply(a, MARGIN = 1, FUN = f, v = x[i,])
    lines(a, y, lty = g)
  }
  legend(3, c("Orthotropic", "Plagiotropic"), lty = 1:2)
  detach(leafshape17)
}
