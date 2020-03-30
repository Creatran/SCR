#' scr9_8
#' @description
#' The solution to the exercise 9_8 for Rizzo's Book.
#'
#' Use the Gibbs sampler to generate a chain with target joint density f(x, y).
#' (f(x, y) can be found in Rizzo's book page 278).
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param N a number indicating the length of the chain. Defaults to be 5000.
#' @param burn a number indicating the burn-in time. Defaults to be 1000.
#'
#' @return plot showing the scatter plot of the chain generated from Gibbs sampler.
#' @export
#'
#' @examples scr9_8()
scr9_8 <- function(N = 5000, burn = 1000){
  X <- matrix(0, N, 2) #the chain, a bivariate sample

  n <- 100
  a <- 30
  b <- 60

  X[1, ] <- c(0, 0.5) #initialize

  for (i in 2:N) {
    x2 <- X[i-1, 2]
    X[i, 1] <- rbinom(1, n, x2)
    x1 <- X[i, 1]
    X[i, 2] <- rbeta(1, x1 + a, n - x1 + b)
  }
  x <- X[(burn + 1): N, ]

  plot(x, main = "Gibbs sampler", xlab = "x", ylab = "y")

}
