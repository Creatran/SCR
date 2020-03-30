#' scr9_9
#' @description
#' The solution to the exercise 9_9 for Rizzo's Book.
#'
#' Modify the Gelman-Rubin convergence monitoring given in Example 9.8
#' so that only the ﬁnal value of R.hat is computed, and repeat the example,
#' omitting the graphs.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n a number indicating the length of the chain. Defaults to be 15000.
#' @param b a number indicating the burn-in time. Defaults to be 1000.
#' @param sigma a number inficating the parameter of the proposal distribution. Defaluts to be 0.2.
#' @param x0 a list of numbers indicating the overdispersed initial values.
#' Defaults to be [-10, -5, 5, 10].
#'
#' @return R.hat a number indicating the estimated R.
#' @export
#'
#' @examples scr9_9()
#'
#'
#'
scr9_9 <- function(n = 15000, b = 1000, sigma = .2, x0 = c(-10, -5, 5, 10)){
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

  # The M-H sampler
  normal.chain <- function(sigma, N, X1) {
    # generates a Metropolis chain for Normal(0,1)
    # with Normal(X[t], sigma) proposal distribution #and starting value X1
    x <- rep(0, N)
    x[1] <- X1
    u <- runif(N)

    for (i in 2:N) {
      xt <- x[i-1]
      y <- rnorm(1, xt, sigma) #candidate point
      r1 <- dnorm(y, 0, 1) * dnorm(xt, y, sigma)
      r2 <- dnorm(xt, 0, 1) * dnorm(y, xt, sigma)
      r <- r1 / r2
      if (u[i] <= r) x[i] <- y
      else x[i] <- xt
    }
    return(x)
  }

  # number of chains to generate
  k = length(x0)

  #generate the chains
  X <- matrix(0, nrow=k, ncol=n)
  for (i in 1:k)
    X[i, ] <- normal.chain(sigma, n, x0[i])

  #compute diagnostic statistics
  psi <- t(apply(X, 1, cumsum))
  for (i in 1:nrow(psi))
    psi[i,] <- psi[i,] / (1:ncol(psi))
  return(Gelman.Rubin(psi))

}
