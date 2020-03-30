#' scr5_10
#' @description
#' The solution to the exercise 5_10 for Rizzo's Book.
#'
#' Use Monte Carlo integration with antithetic variables to estimate
#' \eqn{\int_0^1 \frac{e^{-x}}{1 + x^2}}
#' and ﬁnd the approximate reduction in variance as a percentage of the variance
#' without variance reduction.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the iteration numbers. Defaults to 1000.
#'
#' @return data.frame containing the Monte Carlo estimate anf the Monte Carlo intergration with
#' antithetic variables estimate of the integral, and the variance reduction percentage.
#' @export
#'
#' @examples scr5_10()
#'
#'

scr5_10 <- function(m = 1000){
  mc <- function(m, anti = TRUE){
    u <- runif(m/2)
    if (anti) u <- c(u, 1-u) else
      u <- c(u, runif(m/2))

    e <- mean(exp(-u)/(1+u^2))
    e
  }

  e1 <- e2 <- numeric(m)
  for (i in 1: m){
    e1[i] <- mc(m)
    e2[i] <- mc(m, anti = FALSE)
  }

  v2 <- var(e2)
  v1 <- var(e1)
  reduction <- (v2-v1)/v2

  MC.est <- mc(m, anti = FALSE)
  MCanti.est <- mc(m)
  return(rbind(MC.est, MCanti.est, reduction))
}
