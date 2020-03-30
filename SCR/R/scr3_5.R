#' scr3_5
#'
#' @description
#' The solution to the exercise 3_5 for Rizzo's Book.
#'
#' Use the inverse transform method to generate a random sample of size 1000 from the
#' distribution of X. Construct a relative frequency table and compare the empirical
#' with the theoretical probabilities. Repeat using the R sample function.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n sample size
#'
#' @return plot
#' @export
#'
#'
#' @examples scr3_5(n=1000)
#'
scr3_5<-function(n=1000){
  u <- runif(n)
  x <- NULL
  for (i in 1:n){
    if (u[i]<=0.1)
    if ((0.1 < u[i]) & (u[i] <= 0.3))
      x[i] <- 1
    if ((0.3 < u[i]) & (u[i] <= 0.5))
      x[i] <- 2
    if ((0.5 < u[i]) & (u[i] <= 0.7))
      x[i] <- 3
    if ((0.7 < u[i]) & (u[i] <= 1))
      x[i] <- 4
  }
  hist(x, prob=TRUE,main = "Sample distribution")
  y=c(0,1,1,2,2,3,3,4,4,4)
  hist(sample(y,n,replace=TRUE), prob=TRUE,main = "Comparison")
}
