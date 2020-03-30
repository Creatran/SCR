#' scr6_10
#' @description
#' The solution to the exercise 6_10 for Rizzo's Book.
#'
#' Construct an approximate 95% confidence interval for the Gini ratio γ = E[G] if X is lognormal with unknown parameters. Assess the coverage rate of the estimation procedure with a Monte Carlo experiment. 
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#' @param n random samples size. Defaults to 50
#' @param m number of response vectors to simulate. Defaults to 1000
#'
#' @export
#'
#' @examples scr6_10(n=50,m=1000)
#'
scr6_10<-function(n=50, m=1000){
  B <- 10
  sum <- 0
  mu <- 1
  v <- 4
  Eg <- 2*pnorm(sqrt(v/2))-1
  ghat <- function(){
    x <- sort(rlnorm(n, meanlog = mu, sdlog = sqrt(v)))
    sum(seq(1-n, n-1, 2)*x)/(n^2*mean(x))
  }
  for (k in 1:m){
    g <- replicate(B, ghat())
    mg = mean(g)
    a <- mean(g) - sd(g) * qnorm(0.975)
    b <- mean(g) + sd(g) * qnorm(0.975)
    if ((a < Eg)&(Eg < b)) {
      sum = sum+1
    }
  }
  r <- sum/m
  print(r)
}

