#' scr4_4
#'
#' @description
#' The solution to the exercise 4_4 for Rizzo's Book.
#'
#' Construct a filled contour plot of the bivariate mixture in Exercise 4.3.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n sample size
#' @param p the probability of the first kind of normal distribution.
#'
#' @return plot
#' @export
#' @importFrom GGally ggpairs
#' @importFrom MASS mvrnorm
#' @importFrom mvtnorm dmvnorm
#' @importFrom lattice levelplot
#'
#'
#' @examples scr4_4(n=200,p=0.75)
#'
scr4_4<-function(n=200,p=0.75){
  mtnorm<-function(n=200,p=0.75){
    x <- NULL
    x1<-rnorm(n,0,3)
    x2<-rnorm(n,3,1)
    x[1:(n*p)]<-x1[1:(n*p)]
    x[(n*p+1):n]<-x2[(n*p+1):n]
    x
  }
  x<-mtnorm()
  y<-mtnorm()
  x<-sort(x)
  y<-sort(y)
  xy <- expand.grid(x, y)
  z <- (1/(2*pi)) * exp(-.5 * (xy[,1]^2 + xy[,2]^2))
  z<-matrix(z,nrow=200)
  filled.contour(z, color = terrain.colors, asp = 1)
  levelplot(z, scales = list(draw = FALSE), xlab = "", ylab = "")
}
