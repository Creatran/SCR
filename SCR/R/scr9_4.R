#' scr9_4
#' @description
#' The solution to the exercise 9_4 for Rizzo's Book.
#'
#' Implement a random walk Metropolis sampler for generating the
#' standard Laplace distribution (see Exercise 3.2). For the increment,
#' simulate from a normal distribution.
#' Compare the chains generated when diﬀerent variances are used for
#' the proposal distribution. Also, compute the acceptance rates of each chain.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the iteration numbers. Defaults to 2000
#' @param sigma a list of number(s) indicating different variances used for the proposal
#' distribution. Defaults to [0.05, 0.5, 2, 16]
#' @param x0 a number indicating the first value set for the chain. Defaluts to 10
#' @details The plots show that random walk Metropolis sampler is very sensitive to the variance of
#' the proposal distribution. Only the third chain has a rejection rate in the range [0.15, 0.75].
#' In the ﬁrst plot with σ = 0.05, the acceptance rate tend to be large and almost every candidate
#' point is accepted. The increments are small and the chain is almost like a true random walk.
#' Chain 1 has not converged to the target in 2000 iterations.
#' The chain in the second plot generated with σ = 0.5 is converging very slowly and requires
#' a much longer burn-in period.
#' In the third plot (σ = 2) the chain is mixing well and converging to the target distribution
#' after a short burn-in period of about 500.
#' Finally, in the fourth plot, where σ = 16, the acceptance rate are smaller and most of the
#' candidate points are rejected. The fourth chain converges, but it is ineﬃcient.
#'
#' @return data.frame containing the acceptance rates of each chain with different sigmas.
#' @return plot showing the chains generated from Random Walk Metropolis Method with different
#' sigmas.
#' @export
#'
#' @examples scr9_4()
#'
#'
#'
scr9_4 <- function(m = 2000, sigma = c(.05, .5, 2, 16), x0 = 10){
  f <- function(x) {
    return(1/2 * exp(-abs(x)))
  }

  rw.Metropolis <- function(sigma, x0, m) {
    x <- numeric(m)
    x[1] <- x0
    u <- runif(m)
    k <- 0
    for (i in 2:m) {
      y <- rnorm(1, x[i-1], sigma)
      if (u[i] <= (f(y) / f(x[i-1])))
        x[i] <- y
      else {
        x[i] <- x[i-1]
        k <- k + 1
      }

    }
    plot(1:m, x, type="l",  ylab="x", xlab = "Index",
         main=paste0("rw.Metropolis sampler with sigma = ", sigma))
    return((m-k)/m)
  }

  accept_rate <- sapply(sigma, rw.Metropolis, x0 = x0, m = m)
  ans <- data.frame(sigma, accept_rate)
  return(ans)
}

