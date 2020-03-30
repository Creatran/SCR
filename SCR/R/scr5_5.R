#' scr5_5
#' @description
#' The solution to Rizzo's book exercise 5_5.
#'
#' Compute (empirically) the eﬃciency of the sample mean Monte Carlo method of
#' estimation of the deﬁnite integral in Example 5.3 (Use the Monte Carlo approach
#' to estimate the standard normal cdf) relative to the “hit or miss” method.
#'
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the iteration numbers. Defaults to 1000.
#' @param x a list of number(s) containing the interested x value corresponding to the
#' problem.
#'
#' @return data.frame contains the true value of the standard normal cdf, the Monte Carlo
#' estimate, the Hit or Miss estimate, the variances corresponding to these two methods,
#' and their efficnency.
#'
#' @export
#'
#' @examples scr5_5()
#'
#'

scr5_5 <- function(m = 10000, x = seq(.1, 2.5, length = 10)){
  Phi <- pnorm(x)

  u <- runif(m)
  MC.var <- MC.est <- numeric(length(x))
  for (i in 1:length(x)) {
    g <- x[i] * exp(-(u * x[i])^2 / 2)
    MC.est[i] <- mean(g) / sqrt(2 * pi) + 0.5
    MC.var[i] <- var(g)/m
  }


  z <- rnorm(m)
  dim(x) <- length(x)
  HITorMISS.est <- apply(x, MARGIN = 1, FUN = function(x, z) {mean(z < x)}, z = z)
  HITorMISS.var <- apply(x, MARGIN = 1, FUN = function(x, z) {var(z < x)/m}, z = z)

  efficiency <- MC.var/HITorMISS.var

  ans <- data.frame(x, Phi, MC.est, HITorMISS.est, MC.var, HITorMISS.var, efficiency)
  ans[, c(1, 2, 3, 4, 7)] <- round(ans[, c(1, 2, 3, 4, 7)], 3)
  return(ans)
}
