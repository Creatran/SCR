#' scr9_11
#' @description
#' The solution to the exercise 9_11 for Rizzo's Book.
#'
#' Refer to Example 9.5. Use the Gelman-Rubin method to monitor convergence of the
#' chain, and run the chain until the chain has converged approximately to the
#' target distribution according to R < 1.2.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n a number indicating the length of the chain. Defaults to be 15000.
#' @param burn a number indicating the burn-in time. Defaults to be 1000.
#' @param w a list of numbers indicating the parameter of proposal distribution.
#' Defaluts to [0.1, 0.3, 0.5, 0.8]
#'
#' @return r.hat a list of numbers containing all the r hat.
#' @return convergence_num a number showing the length of the chain when it reaches the convergence.
#' @return fig a plot showing the r hat.
#' @export
#'
#' @examples scr9_11()
#'
#'
#'
scr9_11 <- function(n = 10000, burn = 50, w = c(0.1, 0.3, 0.5, 0.8)){
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

  # Generate the chain
  generate.chain <- function(m, b = 0.2, w = 0.25, sed = 135) {
    set.seed(135)
    days <- 250
    x <- numeric(m)

    i <- sample(1:5, size=days, replace=TRUE,
                prob=c(1, 1-b, 1-2*b, 2*b, b))
    win <- tabulate(i)

    prob <- function(y, win) { # computes (without the constant) the target density
      if (y < 0 || y >= 0.5) return (0)
      return((1/3)^win[1] *
               ((1-y)/3)^win[2] * ((1-2*y)/3)^win[3] *
               ((2*y)/3)^win[4] * (y/3)^win[5])
    }

    u <- runif(m)
    v <- runif(m, -w, w)
    x[1] <- .25
    for (i in 2:m) {
      #for accept/reject step #proposal distribution
      y <- x[i-1] + v[i]
      if (u[i] <= prob(y, win) / prob(x[i-1], win))
        x[i] <- y else x[i] <- x[i-1]
    }
    return(x)
  }

  k = length(w)

  #generate the chains
  X <- matrix(sapply(1:k, function(i) generate.chain(m = n, w = w[i])),
              nrow = k, byrow = TRUE)

  psi <- t(apply(X, 1, cumsum))
  for (i in 1:nrow(psi))
    psi[i,] <- psi[i,] / (1:ncol(psi))

  r.hat <- numeric(n)
  for (i in 2:burn)
    r.hat[i] = Gelman.Rubin(psi[,1:i])
  for (i in (burn - 1):n){
    r.hat[i] = Gelman.Rubin(psi[,1:i])
    if (r.hat[i] < 1.2)
      break()
  }

  plot(1:i, r.hat[1:i], type="l", xlab = "Index", ylab = "R hat",
       main = "Convergence of the chain")
  abline(h=1.2)
  return(list('r.hat' = r.hat[1:i], convergence_num = i))

}
