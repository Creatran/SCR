#' scr3_3
#' 
#' @description
#' The solution to the exercise 3_3 for Rizzo's Book.
#' 
#' Derive the probability inverse transformation F1(U) and use the inverse transform method to
#' simulate a random sample from the Pareto(2, 2) distribution. Graph the density histogram of the 
#' sample with the Pareto(2, 2) density superimposed for comparison
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#' 
#' @param n Sample size 
#' @param a The parameter of pareto.
#' @param b The parameter of pareto.
#'  
#' @return plot
#' @export
#' @importFrom sads dpareto
#'
#'
#' @examples scr3_3(n=1000,a=2,b=2)
#'
scr3_3<-function(n=1000,a=2,b=2){
  u <- runif(n, min=0, max=1)
  x <- 2/sqrt(1-u)
  hist(x, prob=TRUE, main = expression(F(x)==1-(2/x)^2))
  plot(x, dpareto(x,shape=a, scale=b), cex=0.1,ylab="Density")
}

