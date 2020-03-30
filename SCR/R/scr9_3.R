#' scr9_3
#' @description
#' The solution to the exercise 9_3 for Rizzo's Book.
#'
#' Use the Metropolis-Hastings sampler to generate random variables
#' from a standard Cauchy distribution. Discard the ﬁrst 1000 of the chain,
#' and compare the deciles of the generated observations with the deciles of
#' the standard Cauchy distribution (see qcauchy or qt with df=1).
#' The standard Cauchy has the Cauchy(θ = 1, η = 0) density.
#' (Note that the standard Cauchy density is equal to the Student t
#' density with one degree of freedom.)
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the iteration numbers. Defaults to 10000.
#'
#' @return data.frame containing the deciles of the generated observations and the
#' deciles of the standard Cauchy distribution.
#' @export
#'
#' @examples scr9_3()
#'
#'
#'
scr9_3 <- function(m = 10000){

  x <- numeric(m)
  x[1] <- rnorm(1, mean = 0, sd = 1)
  u <- runif(m)

  for (i in 2:m) {
    xt <- x[i-1]
    y <- rnorm(1, mean = xt, sd = 1)
    num <- dcauchy(y) * dnorm(xt, mean = y, sd = 1)
    den <- dcauchy(xt) * dnorm(y, mean = xt, sd = 1)
    if (u[i] <= num/den) x[i] <- y else {
      x[i] <- xt
    }
  }

  X <- x[1001 : m]
  decile <- seq(0.1, 0.9, 0.1)
  generated.observations <- quantile(X, decile)
  standard.CauchyDist <- qcauchy(decile)
  ans <- data.frame(generated.observations, standard.CauchyDist)
  return(ans)
}
