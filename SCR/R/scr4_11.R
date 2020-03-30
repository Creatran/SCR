#' scr4_11
#'
#' @description
#' The solution to the exercise 4_11 for Rizzo's Book.
#'
#' Refer to the full leafshape (DAAG) data set. Display a segment style stars plot for leaf
#' measurements at latitude 42 (Tasmania). Repeat using the logarithms of the measurements.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#'
#' @return plot
#' @export
#' @import MASS
#'
#'
#' @examples scr4_11()
#'
scr4_11<-function(){
  crabs<-MASS::crabs
  attach(crabs)
  x <- crabs
  x <- subset(x, sp == "B")
  a <- x$CW * x$CL
  x[4:8] <- x[4:8] / sqrt(a)
  palette(rainbow(6))
  stars(x[4:8], draw.segments = TRUE,
        labels = levels(x$sex), nrow = 4,
        ylim = c(-2,10), key.loc = c(3,-1))
  palette("default")
  detach(crabs)
}

