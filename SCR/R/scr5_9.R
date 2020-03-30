#' scr5_9
#' @description
#' The solution to the exercise 5_9 for Rizzo's Book.
#'
#' Implement a function to generate samples from a Rayleigh(σ) distribution,
#' using antithetic variables. What is the percent reduction in variance of (X+X')/2
#' compared with (X1 + X2)/2 for independent X1, X2 ?
#'
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the iteration numbers.
#' @return data.frame containing the variance of (X + X')/2 and (X1 + X2)/2,
#' and the variance reduction percent.
#' @export
#'
#' @examples scr5_9()
#'
#'

scr5_9 <- function(m = 1000){
  d <- 8

  u1 <- runif(m/2)
  u2 <- runif(m/2)

  x1 <- sqrt(-2*d^2*log(1-u1))
  x2 <- sqrt(-2*d^2*log(1-u2))

  x <- sqrt(-2*d^2*log(1-u1))
  x0 <- sqrt(-2*d^2*log(u1))

  R.var <- var((x1+x2)/2)
  Anti.var <- var((x+x0)/2)
  Reduction <-  (R.var - Anti.var)/R.var
  return(rbind(R.var, Anti.var, Reduction))

}
