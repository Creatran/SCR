#' scr6_7
#' @description
#' The solution to the exercise 6_7 for Rizzo's Book.
#' 
#' Estimate the power of the skewness test of normality against symmetric Beta(α, α) distributions and comment on the results.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#' @param n random samples size. Defaults to 20
#' @param m number of response vectors to simulate. Defaults to 1000
#' @param alpha significance level. Defaults to 0.05
#'
#' @export
#'
#' @examples scr6_7(n=20,m=1000,alpha=0.05)
#'
scr6_7<-function(n=20,m=1000,alpha=0.05){
  sk <- function(x) {
    xbar <- mean(x)
    m3 <- mean((x-xbar)^3)
    m2 <- mean((x-xbar)^2)
    return( m3 / m2^1.5 )
  }
  epsilon <- c(seq(0, .15, .01), seq(.15,1, .05))
  N <- length(epsilon)
  pwr <- numeric(N)
  cv <- qnorm(1 - alpha/2, 0, sqrt(6*(n-2) / ((n+1)*(n-3))))
  for(j in 1:N) {
    e <- epsilon[j]
    sktests <- numeric(m)
    for (i in 1:m) {
      sigma <- sample(c(1, 10), replace = TRUE, size = n, prob = c(1 - e, e))
      x <- rnorm(n, 0, sigma)
      sktests[i] <- as.integer(abs(sk(x)) >= cv)
    }
    pwr[j] <- mean(sktests)
  }
  se <- sqrt(pwr * (1 - pwr) / m)
  a<-c(mean=mean(pwr),se=mean(se))
  a
}

