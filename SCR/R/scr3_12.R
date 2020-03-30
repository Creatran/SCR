#' scr3_12
#'
#' @description
#' The solution to the exercise 3_12 for Rizzo's Book.
#'
#' Generate 1000 random observations from this mixture with r = 4 and \eqn{\beta} = 2.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n sample size
#' @param r an alternative way to specify the scale.
#' @param beta parmetor in gamma distribution.
#'
#' @return plot
#' @export
#'
#'
#' @examples scr3_12(n=1000,r=4,beta=2)
#'
scr3_12<-function(n=1000,r=4,beta=2){
  x<-NULL
  for (i in 1:n){
    u1 <- rgamma(1,r,beta)
    x[i]<-rexp(1,u1)
  }
  hist(x,prob=TRUE)
}
