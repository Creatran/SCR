#' scr3_2
#'
#' @description
#' The solution to the exercise 3_2 for Rizzo's Book.
#'
#' The standard Laplace distribution has density f(x)=1/2*e^-|x|, x \eqn{\in} R. Use the inverse transform
#' method to generate a random sample of size 1000 from this distribution.
#' Use one of the methods shown in this chapter to compare the generated sample to the target
#' distribution.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n sample size
#'
#' @return plot
#' @export
#' @importFrom ExtDist rLaplace
#'
#'
#' @examples scr3_2(n=1000)
#'
scr3_2<-function(n=1000){
  x <- rLaplace(n)
  hist(x, main = "Randomly Generated Data", prob = TRUE)
  qqnorm(x, main = "Sample quantiles vs theoretical quantiles")
}
