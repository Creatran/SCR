#' scr9_7
#' @description
#' The solution to the exercise 9_7 for Rizzo's Book.
#'
#' Implement a Gibbs sampler to generate a bivariate normal chain
#' (Xt, Yt ) with zero means, unit standard deviations, and correlation 0.9.
#' Plot the generated sample after discarding a suitable burn-in sample.
#' Fit a simple linear regression model Y = β0 + β1 X to the sample and
#' check the residuals of the model for normality and constant variance.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param N a number indicating the length of the chain. Defaults to be 5000.
#' @param burn a number indicating the burn-in time. Defaults to be 1000.
#'
#' @return x a list of numbers containing the generated normal chain Xt.
#' @return y a list of numbers containing the generated normal chain Yt.
#' @return lm_reg lthe inear regression result of the model Y = β0 + β1 X.
#' @return fig a scatter plot of the generated sample.
#'
#' @examples scr9_7()
#' @export
#'
#'

scr9_7 <- function(N = 5000, burn = 1000){
  X <- matrix(0, N, 2)    #the chain, a bivariate sample

  rho <- 0.9             #correlation
  mu1 <- 0
  mu2 <- 0
  sigma1 <- 1
  sigma2 <- 1
  s1 <- sqrt(1-rho^2)*sigma1
  s2 <- sqrt(1-rho^2)*sigma2

  ###### generate the chain #####

  X[1, ] <- c(mu1, mu2)            #initialize

  for (i in 2:N) {
    x2 <- X[i-1, 2]
    m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
    X[i, 1] <- rnorm(1, m1, s1)
    x1 <- X[i, 1]
    m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
    X[i, 2] <- rnorm(1, m2, s2)
  }

  b <- burn + 1
  X <- X[b : N, ]
  x <- X[, 1]
  y <- X[, 2]
  fig <- plot(x, y, type = 'p', cex=0.1, main = "Generated sample",
       xlab = "x", ylab = "y")

  b1 <- cov(x, y) / var(y)
  b0 <- mean(x) - b1 * mean(y)

  lm_reg <- lm(x ~ y)
  return(list('x' = x, 'y' = y, 'lm_reg' = lm_reg))

}
