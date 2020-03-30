#' scr4_7
#'
#' @description
#' The solution to the exercise 4_7 for Rizzo's Book.
#'
#' Create a parallel coordinates plot of the crabs (MASS)[278] data using all 200
#' observations. Compare the plots before and after adjusting the measurements by the
#' size of the crab. Interpret the resulting plots.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n the number of observations
#' @param a the initial plot of the data when a=1, the adjusted data when a=0.
#'
#' @return plot
#' @export
#' @importFrom lattice parallel
#' @importFrom lattice trellis.device
#'
#'
#' @examples scr4_7(n=200)
#'
scr4_7<-function(n=200,a=1){
  library(MASS)
  library(lattice)
  if (a==1){
  trellis.device(color = FALSE)
  x <- crabs[seq(5, n, 5), ]
  parallelplot(~x[4:8] | sp*sex, x) }
  else{
  trellis.device(color = FALSE)
  x <- crabs[seq(5, n, 5), ]
  a <- x$CW * x$CL
  x[4:8] <- x[4:8] / sqrt(a)
  parallelplot(~x[4:8] | sp*sex, x)}
}

