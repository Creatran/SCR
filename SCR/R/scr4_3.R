#' scr4_3
#'
#' @description
#' The solution to the exercise 4_3 for Rizzo's Book.
#'
#' Generate a bivariate random sample from the joint distribution of (X,Y)
#' and construct a contour plot.
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
#' @importFrom lattice contourplot
#'
#'
#' @examples scr4_3(n=200,p=0.75)
#'
scr4_3<-function(n=200,p=0.75){
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
  contourplot(z)
}

