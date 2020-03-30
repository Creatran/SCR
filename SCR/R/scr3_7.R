#' scr3_7
#'
#' @description
#' The solution to the exercise 3_7 for Rizzo's Book.
#'
#' Write a function to generate a random sample of size n from the Beta(a,b)
#' distribution by the acceptance-rejection method. Generate a random sample of size 1000 from the
#' Beta(3,2) distribution. Graph the histogram of the sample with the theoretical
#' Beta(3,2) density superimposed.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n sample size
#' @param a non-negative parameters of the Beta
#' @param b distribution
#'
#' @return plot
#' @export
#'
#'
#' @examples scr3_7(n=1000,a=3,b=2)
#'
scr3_7<-function(n=1000,a=3,b=2){
  x <- NULL
  k <- 0
  while (k<n){
    y <- runif(1)
    u <- runif(1)
    if (u<((y^(a-1))*((1-y)^(b-1)))*4) {
      k <- k+1
      x[k] <- y
    }
  }
  hist(x, prob=TRUE,main=bquote(f(x)==Beta(3,2)))
  x1 <- seq(0,1,0.01)
  lines(x1,dbeta(x1,3,2))
}

