#' scr5_7
#' @description
#' The solution to the Rizzo's book exercise 5_7.
#'
#' Refer to Exercise 5.6. Use a Monte Carlo simulation to estimate θ by the
#' antithetic variate approach and by the simple Monte Carlo method.
#' Compute an empirical estimate of the percent reduction in variance using the
#' antithetic variate. Compare the result with the theoretical value from Exercise 5.6.
#'
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the iteration numbers. Defaults to 1000.
#'
#' @return data.frame containing the empirical estimate of the variance reduction using
#' the antithetic variate, and the theoretical value from Exercise 5.6.
#' @export
#'
#' @examples scr5_7()
#'
#'

scr5_7 <- function(m = 1000){
  e <- exp(1)
  var.mc <- ((e^2 - 1)/2 - (e - 1)^2)/m
  var.an <- (-3 * e^2 + 10 * e - 5)/2/m
  var.reduction <- (var.mc - var.an)/var.mc

  mc <- function(m=1000, anti = TRUE){
    u <- runif(m/2)
    if (anti) u <- c(u, 1-u) else
      u <- c(u, runif(m/2))

    e <- mean(exp(u))
    e
  }

  mcan.est <- replicate(m, mc())
  mc.est <- replicate(m, mc(anti = FALSE))

  var.mc <- var(mc.est)
  var.mcan <- var(mcan.est)
  var.reduction_est <- (var.mc - var.mcan)/var.mc

  return(rbind(var.reduction_est, var.reduction))

}
