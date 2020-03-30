#' scr3_4
#'
#' @description
#' The solution to the exercise 3_4 for Rizzo's Book.
#'
#' Develop an algorithm to generate random samples from a Rayleigh(\eqn{\sigma}) distribution. Generate
#' Rayleigh(\eqn{\sigma}) samples for several choices \eqn{\sigma}f\eqn{\sigma} > 0 and check that the mode of the generated
#' samples is close to the theoretical mode &R (check the histogram).
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n Sample size
#' @param sigma The parameter of Rayleigh.
#'
#' @return plot
#' @export
#' @importFrom VGAM rrayleigh
#'
#'
#' @examples scr3_4(n=1000,sigma=1)
#'
scr3_4<-function(n=1000,sigma=1){
  x <- rrayleigh(n, scale = sigma)
  hist(x, main = "Randomly Generated Data", prob = TRUE)
  qqnorm(x, main = "Sample quantiles vs theoretical quantiles")
}

