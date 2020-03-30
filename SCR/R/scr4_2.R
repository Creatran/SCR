#' scr4_2
#'
#' @description
#' The solution to the exercise 4_2 for Rizzo's Book.
#'
#' Add a fitted smoothcurve to each of the scatterplotsin Figure4.1 of Example 4.1.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n sample size
#'
#' @return plot
#' @export
#' @importFrom GGally ggpairs
#' @importFrom MASS mvrnorm
#'
#'
#' @examples scr4_2(n=200)
#'
scr4_2<-function(n=200){
  Sigma <- matrix(c(1,-.5,.5,-.5,1,-.5,.5,-.5,1),3,3)
  x<-mvrnorm(n, c(0,1,2), Sigma)
  x<-as.data.frame(x)
  pairs(x, panel = panel.smooth)
}
