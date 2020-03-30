#' scr3_14
#' 
#' @description
#' The solution to the exercise 3_14 for Rizzo's Book.
#' 
#' Use the R pairs plot to graph an array of scatter plots for each pair of variables.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#' 
#' @param n sample size 
#'
#' @return plot
#' @export
#' @importFrom GGally ggpairs
#' @importFrom MASS mvrnorm
#'
#'
#' @examples scr3_14(n=200)
#'
scr3_14<-function(n=200){
  Sigma <- matrix(c(1,-.5,.5,-.5,1,-.5,.5,-.5,1),3,3)
  x<-mvrnorm(n, c(0,1,2), Sigma)
  x<-as.data.frame(x)
  ggpairs(x)
}
