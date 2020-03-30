#' scr5_3
#'
#' @description
#' The solution to Rizzo's book exercise 5.3.
#'
#' Compute a Monte Carlo estimate θ of \eqn{\thata = \int_0^{0.5}e^{-x}dx} by sampling from
#' Uniform(0, 0.5), and estimate the variance of \eqn{\hat{θ}}. Find another Monte Carlo estimator
#' {θ^∗} by sampling from the exponential distribution.
#' Which of the variances (of \eqn{\hat{θ}} and {θ^∗}) is smaller, and why?
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the iteration numbers. Defaults to 1000.
#'
#' @return a data.frame containing the true value of theta, the two Monte Carlo estimates
#' (theta.hat and theta.star), and their corresponding variance estimates.
#' @export
#'
#' @examples scr5_3(10000)
#'
#'

scr5_3 <- function(m = 1000){

  x <- runif(m,0,0.5)
  thetahat <- 0.5 * mean(exp(-x))
  var.hat <- var(exp(-x))/m

  y <- rexp(m)
  thetastar <-  mean(y <= 0.5)
  var.star <- var(y<=0.5)/m

  theta <- pexp(0.5)

  return(rbind(theta, thetahat, var.hat, thetastar, var.star))
}


