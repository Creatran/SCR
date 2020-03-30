#' scr5_4
#' @description
#' The solution to the Rizzo's book exercise 5_4.
#'
#' Write a function to compute a Monte Carlo estimate of the Beta(3, 3) cdf,
#' and use the function to estimate F(x) for x = 0.1, 0.2, . . ., 0.9.
#' Compare the estimates with the values returned by the pbeta function in R.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the iteration number. Defaults to 1000.
#'
#' @return a data.frame containing the Monte Carlo estimate of Beta(3, 3) cdf for x = 0.1,
#' 0.2, ..., 0.9, and the estimate with the values returned by pbeta function in R.
#' @export
#'
#' @examples scr5_4(m = 5000)
#'

scr5_4 <- function(m = 1000){
  betacdf <- function(x, a=3, b=3){
    c <- gamma(a+b)/gamma(a)/gamma(b)
    y <- runif(m)
    cdf <- mean(c*x*(y*x)^(a-1)*(1-y*x)^(b-1))
    cdf
  }

  x <- seq(0.1,0.9,0.1)

  ans <- data.frame('MC estimate' = sapply(x, betacdf),'pbeta estimate' = pbeta(x,3,3))
  return(ans)
}

