#' scr5_14
#' @description
#' The solution to Rizzo's book exercise 5.14.
#'
#' Obtain a Monte Carlo estimate of \eqn{\int_1^{\infinity}\frac{x^2}{\sqrt(2\pi)}e^{-x^2/2}dx}
#' by importance sampling.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the number of iterations. Defaults to 1000.
#'
#' @return data.frame containing the Monte Carlo estimate of theta hat by
#' importantce sampling, and the estimation variance.
#' @export
#'
#' @examples scr5_14(m = 5000)
#'
#'
#'
scr5_14 <- function(m = 1000){

  f <- function(x){
    x^2/sqrt(2*pi)*exp(-x^2/2)*(x>1)
  }

  x <- rnorm(m)
  i <- which(x<1)
  x[i] <- 0.5
  e <- f(x)/dnorm(x)
  thetahat <- mean(e)
  est.var <- var(e)

  return(rbind(thetahat, est.var))

}
