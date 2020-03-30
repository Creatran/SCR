#' scr9_2
#' @description
#' The solution to the exercise 9_2 for Rizzo's Book.
#'
#' Repeat Example 9.1 using the proposal distribution Y
#' (shape parameter Xt and rate parameter 1).
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the iteration numbers. Defaults to 10000.
#' @param sigma a number indicating to the sigma parameter related to Rayleigh distribution.
#' Defaults to 4.
#'
#' @return plot showing the chain generated with Metropolis-Hasting sampler
#' for the target Rayleigh's distribution with sigma (defaluts to 4).
#' @export
#'
#' @examples scr9_2()
#'
#'
#'

scr9_2 <- function(m = 10000, sigma = 4){
  f <- function(x, sigma) {
    if (any(x < 0)) return (0)
    stopifnot(sigma > 0)
    return((x / sigma^2) * exp(-x^2 / (2*sigma^2)))
  }

  x <- numeric(m)
  x[1] <- rgamma(1, 1)
  k <- 0
  u <- runif(m)

  for (i in 2:m) {
    xt <- x[i-1]
    y <- rgamma(1, xt)
    num <- f(y, sigma) * dgamma(xt, y)
    den <- f(xt, sigma) * dgamma(y, xt)
    if (u[i] <= num/den) x[i] <- y else {
      x[i] <- xt
      k <- k+1     #y is rejected
    }
  }

  index <- 5000:5500
  y1 <- x[index]
  plot(index, y1, type="l",
       main=paste0("Metropolis-Hastings sampler with sigma = ", sigma), ylab="x")

}
