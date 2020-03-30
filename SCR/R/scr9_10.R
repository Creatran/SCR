#' scr9_10
#' @description
#' The solution to the exercise 9_10 for Rizzo's Book.
#'
#' Refer to Example 9.1. Use the Gelman-Rubin method to monitor convergence of
#' the chain, and run the chain until the chain has converged approximately to
#' the target distribution according to R < 1.2. (See Exercise 9.9.) Also use
#' the coda [212] package to check for convergence of the chain by the
#' Gelman-Rubin method.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n a number indicating the length of the chain. Defaults to be 15000.
#' @param b a number indicating the burn-in time. Defaults to be 1000.
#' @param x0 a list of number(s) indicating the overdispersed initial values.
#' Defaults to [2, 3, 4, 5]
#'
#' @return r.hat a list of numbers indicating the r hat
#' @return convergence_num a number indicating the length of the chain until it reaches the
#' convergence.
#' @return plot showing the changing in r hat.
#' @export
#'
#' @examples scr9_10()
#'
#'
#'
scr9_10 <- function(n = 2000, b = 500, x0 = c(2, 3, 4, 5), sigma = 4){
  Gelman.Rubin <- function(psi) {
    # psi[i,j] is the statistic psi(X[i,1:j])
    # for chain in i-th row of X
    psi <- as.matrix(psi)
    n <- ncol(psi)
    k <- nrow(psi)

    psi.means <- rowMeans(psi)
    B <- n * var(psi.means)
    psi.w <- apply(psi, 1, "var")
    W <- mean(psi.w)
    v.hat <- W*(n-1)/n + (B/n)
    r.hat <- v.hat / W
    return(r.hat)
  }

  f <- function(x, sigma) {
    if (any(x < 0)) return (0)
    stopifnot(sigma > 0)
    return((x / sigma^2) * exp(-x^2 / (2*sigma^2)))
  }

  rRayleigh <- function(s){
    u <- runif(1)
    s * sqrt(-2 * log(1-u))
  }

  # Generate the chain
  generate.chain <- function(m, sigma, x1 = 2) {
    x <- numeric(m)
    #x[1] <- rRayleigh(2)
    x[1] <- x1
    k <- 0
    u <- runif(m)

    for (i in 2:m) {
      xt <- x[i-1]
      y <- rRayleigh(xt)
      num <- f(y, sigma) * f(xt, y)
      den <- f(xt, sigma) * f(y, xt)
      if (u[i] <= num/den) x[i] <- y else {
        x[i] <- xt
        k <- k+1     #y is rejected
      }
    }
    return(x)
  }

  k = length(x0)

  #generate the chains
  X <- matrix(sapply(1:k, function(i) generate.chain(m = n, sigma = sigma, x1 = x0[i])),
              nrow = k, byrow = TRUE)

  psi <- t(apply(X, 1, cumsum))
  for (i in 1:nrow(psi))
    psi[i,] <- psi[i,] / (1:ncol(psi))

  r.hat <- numeric(n)
  for (i in (b + 1):n){
    r.hat[i] = Gelman.Rubin(psi[,1:i])
    if (r.hat[i] < 1.2)
      break()
  }

  plot((b + 1):n, r.hat[(b + 1):n], type="l", xlab = "Index", ylab = "R hat",
       main = "Convergence of the chain")
  abline(h=1.2)
  return(list('r.hat' = r.hat, convergence_num = i))

}
