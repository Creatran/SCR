#' scr3_9
#' 
#' @description
#' The solution to the exercise 3_9 for Rizzo's Book.
#' 
#'  Write a function to generate random variates from Fe, and construct the histogram 
#'  density estimate of a large simulated random sample.  
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#' 
#' @param n sample size 
#' @param a lower limits of the distribution. 
#' @param b upper limits of the distribution.
#'
#' @return plot
#' @export
#' @importFrom stats runif
#'
#'
#' @examples scr3_9(n=1000,a=-1,b=1)
#'
scr3_9<-function(n=1000,a=-1,b=1){
  x<-NULL
  for (i in 1:n){
    u1 <- runif(1,a,b)
    u2 <- runif(1,a,b)
    u3 <- runif(1,a,b)
    
    if ((abs(u3)>=abs(u2))&(abs(u3)>=abs(u1))) {x[i] <- u2} else {x[i] <- u3}
    
  }
  hist(x,prob=TRUE, main=bquote(f(x)==frac(3,4)(1-x^2)))
  y <- seq(-1,1,0.01)
  lines(y, 3/4*(1-y^2))
}

