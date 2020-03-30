#' scr9_12
#' @description
#' The solution to the exercise 9_12 for Rizzo's Book.
#'
#' Refer to Example 9.6. Use the Gelman-Rubin method to monitor convergence of the
#' chain, and run the chain until the chain has converged approximately to the
#' target distribution according to R < 1.2.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n lenght of the chain. Defaults to be 15000.
#' @param burn burn-in time. Defaults to be 1000.=
#' @param x0 overdispersed initial values. Defaults to be [0.1, 0.2, 0.3, 0.4]
#'
#' @return r.hat a list of numbers indicating the r hat
#' @return convergence_num a number indicating the length of the chain until it reaches the
#' convergence.
#' @return plot showing the changing in r hat.
#' @export
#'
#' @examples scr9_12()
#'
#'
#'
scr9_12 <- function(n = 15000, burn = 50, x0 = c(0.1, 0.2, 0.3, 0.4)){
  sizes = c(125, 18, 20, 34)
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

  sd = 0.5
  min = -0.8
  max = 0.8

  # Generate the chain
  rg = function(p) {
    return(runif(1, min - abs(p), max + abs(p)))
    # return(rnorm(1, p, sd))
  }

  dg = function(x, p) {
    return(dunif(x, min - abs(p), max + abs(p)))
    # return(dnorm(x = x, mean = p, sd = sd))
  }
  prob.vector = function (theta) {
    return(c(2 + theta, (1-theta), (1-theta), theta) / 4)
  }
  prob.ratio = function (n, d) {
    #print(prob.vector(n)^sizes)
    #print(prob.vector(d)^sizes)
    return(prod(prob.vector(n)^sizes / prob.vector(d)^sizes))
  }
  generate.chain <- function(m, x1 = rg(0)) {
    sizes = c(125, 18, 20, 34)
    n = sum(sizes)
    is = (burn + 1):m

    # metropolis hastings



    x.mh = numeric(m)
    k.mh = 0
    u = runif(m)
    x.mh[1] = x1 #rg(0)
    for(i in 2:m) {
      xt = x.mh[i-1]
      y = rg(xt)
      r = min(prob.ratio(y, xt) * dg(xt, y) / dg(y, xt), 1)
      if (!is.na(r) && u[i] <= r) {
        x.mh[i] = y
      } else {
        x.mh[i] = xt
        k.mh = k.mh + 1
      }
    }

    return(x.mh)
  }

  k = length(x0)

  #generate the chains
  X <- matrix(sapply(1:k, function(i) generate.chain(m = n, x1 = x0[i])),
              nrow = k, byrow = TRUE)

  psi <- t(apply(X, 1, cumsum))
  for (i in 1:nrow(psi))
    psi[i,] <- psi[i,] / (1:ncol(psi))

  r.hat <- numeric(n)
  for (i in (burn + 1):n){
    r.hat[i] = Gelman.Rubin(psi[,1:i])
    if (r.hat[i] < 1.2)
      break()
  }

  plot((burn + 1):i, r.hat[(burn + 1):i], type="l", xlab = "Index", ylab = "R hat",
       main = "Convergence of the chain")
  abline(h=1.2, col = 3, lwd = 2)
  return(list('r.hat' = r.hat[1:i], convergence_num = i))

}
