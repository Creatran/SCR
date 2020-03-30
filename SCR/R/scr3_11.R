#' scr3_11
#'
#' @description
#' The solution to the exercise 3_11 for Rizzo's Book.
#'
#'  Generate a random sample of size 1000 from a normal location mixture. The components
#'  of the mixture have N(0,1)and N(3,1)distributions with mixing probabilities p1 and p2
#'  =1-p1. Graph the histogram of the sample with density superimposed, for p1 =0 .75.
#'  Repeat with different values for p1 and observe whether the empirical distribution
#'  of the mixture appears to be bimodal. Make a conjecture about the values of p1
#'  that produce bimodal mixtures.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n sample size
#' @param p probability---p1
#'
#' @return plot
#' @export
#'
#'
#' @examples scr3_11(n=1000,p=0.75)
#'
scr3_11<-function(n=1000,p=0.75){
  x <- matrix(0, ncol=n)
  x1<-rnorm(n,0,3)
  x2<-rnorm(n,3,1)
  x[1:n*p]<-x1[1:n*p]
  x[n*p+1:n]<-x2[n*p+1:n]
  hist(x,prob=TRUE)
}
