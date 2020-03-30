#' scr3_8
#'
#' @description
#' The solution to the exercise 3_8 for Rizzo's Book.
#'
#' Write a function to generate random variates from a Lognormal(\eqn{\mu,\sigma}) distribution
#' using a transformation method, and generate a random sample of size 1000. Compare
#' the histogram with the lognormal density curve given by the dlnorm function in R.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n sample size
#' @param mu mean of the distribution on the log scale with default values of 0 and 1.
#' @param sigma standard deviation of the distribution on the log scale with default values of 0 and 1.
#'
#' @return plot
#' @export
#'
#'
#' @examples scr3_8(n=1000,mu=0,sigma=1)
#'
scr3_8<-function(n=1000,mu=0,sigma=1){
  x <- rlnorm(n,mu,sigma)
  hist(x, prob=TRUE,main = "Lognorm density")
  x1 <- seq(-5,5,0.01)
  lines(x1,dlnorm(x1,mu,sigma))
}
