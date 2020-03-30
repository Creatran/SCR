#' scr6_6
#' @description
#' The solution to the exercise 6_1 for Rizzo's Book.
#' 
#' Estimate the 0.025, 0.05, 0.95, and 0.975 quantiles of the skewness √b1 under normality by a Monte Carlo experiment. Compute the standard error of the estimates from (2.14) using the normal approximation for the density (with exact variance formula). Compare the estimated quantiles with the quantiles of the large sample approximation √b1 ≈ N(0, 6/n).
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#' @param n random samples size. Defaults to 20
#' @param m number of response vectors to simulate. Defaults to 1000
#'
#' @return matrix 
#' @export
#'
#' @examples scr6_6(n=20,m=1000)
#'
scr6_6<-function(n=20,m=1000){
  q <- c(0.025, 0.05, 0.95, 0.975)
  sk <- function(x){
    xbar <- mean(x)
    m3<- mean((x-xbar)^3)
    m2 <- mean((x-xbar)^2)
    return(m3/m2^1.5)
  }
  bk <- numeric(m)
  for (i in 1:m){
    x <- rnorm(n)
    bk[i] <- sk(x)
  }
  bk <- sort(bk)
  b <- numeric(length(q))
  
  for (i in 1:length(q)){
    b[i] <- bk[floor(m*q[i])]
  }
  print(b)
  y <- qnorm(q, 0, sqrt(6/n))
  print(y)
}

