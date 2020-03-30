#' scr3_17
#'
#' @description
#' The solution to the exercise 3_17 for Rizzo's Book.
#'
#' Compare the performance of the Beta generator of Exercise 3.7, Example 3.8 and the R generator
#' rbeta. Fix the parameters a =2,b= 2 and time each generator on 1000 iterations with sample size
#' 5000. (See Example 3.19.) Are the results different for different choices of a and b?
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n sample size
#'
#' @return list
#' @export
#' @importFrom GGally ggpairs
#' @importFrom MASS mvrnorm
#'
#'
#' @examples scr3_17(n=50,a=2,b=2,N=100)
scr3_17<-function(n=50,a=2,b=2,N=100){
  scr7<-function(n=1000,a=3,b=2){
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
    x
  }
  set.seed(100)
  system.time(for (i in 1:N)
    scr7(n,a,b))
  set.seed(100)
  system.time(for (i in 1:N)
    rlnorm(n,a,b))
  set.seed(100)
  system.time(for (i in 1:N)
    rbeta(n,a,b))
}
