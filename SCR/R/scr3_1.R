#' scr3_1
#'
#' @description
#' The solution to the exercise 3_1 for Rizzo's Book.
#'
#' Write a function that will generate and return a randomsample of size n from the two-parameter
#' exponential distribution Exp(\eqn{\lambda,\eta}) for arbitraryn, \eqn{\lambda}, and \eqn{\eta}. (See Examples 2.3 and 2.6.)
#' Generate a large sample from Exp(\eqn{\lambda,\eta}) and compare the sample quantiles with the theoretical
#' quantiles.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n sample size.
#' @param lambda Vector of rates.
#' @param eta Vector of shifts.
#'
#' @return plot
#' @export
#' @importFrom tolerance r2exp
#'
#'
#' @examples scr3_1(n=100,lambda=0.3,eta=10)
#'
scr3_1<-function(n=100,lambda=0.3,eta=10){
  x <- r2exp(n = n, rate = 1/lambda, shift = eta)
  hist(x, main = "Randomly Generated Data", prob = TRUE)
  qqnorm(x, main = "Sample quantiles vs theoretical quantiles")
}

