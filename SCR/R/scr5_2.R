#' scr5_2
#' @description The solution to Rizzo's Book exercise 5.2.
#'
#' Compute a Monte Carlo estimate of the standard normal cdf,
#' by generating from the Uniform(0,x) distribution.
#' Compare your estimates with the normal cdf function pnorm.
#' Compute an estimate of the variance of your Monte Carlo estimate of Φ(2),
#' and a 95\% conﬁdence interval for Φ(2).
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#' @param x a list of number(s), corresponding to the x in the exercise description.
#' Defaults to 2.
#' @param m a number indicating the iteration numbers. Defaults to 1000.
#'
#' @return ans a data.frame consists with the Monte Carlo estimates of Φ(x) and the estimates
#' with normal cdf function `pnorm`.
#' @export
#' @examples scr5_2(1:5, 1000)
#'

scr5_2 <- function(x = 2, m = 1000){
  set.seed(135)
  cdf <- pnorm(x)
  u <- runif(m, min = 0, max = abs(x))
  MC.estimate <- numeric(length(x))
  CI.L <- numeric(length(x))
  CI.R <- numeric(length(x))

  for (i in 1:length(x)){
    MC.estimate[i] <- mean(x[i] * exp(-u^2/2))/sqrt(2 * pi) + 0.5
    if (x[i] < 0){
      MC.estimate[i] <- 1 - MC.estimate[i]
    }
    v <- x[i]^2/m * var(exp(-u^2/2))
    CI.L[i] <- MC.estimate[i] - 1.96 * sqrt(v)
    CI.R[i] <- MC.estimate[i] + 1.96 * sqrt(v)
  }
  print(round(rbind(x, cdf, MC.estimate, CI.L, CI.R), 3))
}
