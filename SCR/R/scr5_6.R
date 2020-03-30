#' scr5_6
#' @description
#' The solution to Rizzo's book exercise 5_6.
#'
#' In Example 5.7 the control variate approach was illustrated for an Monte Carlo integration.
#' Now consider the antithetic variate approach. Compute Cov(e^U , e^{1−U}) and
#' Var(e^U + e^{1−U} ), where U ∼ Uniform(0,1).
#' What is the percent reduction in variance of θ that can be achieved using
#' antithetic variates (compared with simple MC)?
#'
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#'
#' @return data.frame contains the covariance and variance corresponding to the problem,
#' and the variance reduction percent.
#' @export
#'
#' @examples scr5_6()
#'
#'

scr5_6 <- function(){
  e <- exp(1)
  ans_cov <- e - (e - 1)^2
  ans_var <- -3 * e^2 + 10 * e - 5

  n <- 1000
  var.mc <- ((e^2 - 1)/2 - (e - 1)^2)/n
  var.an <- (-3 * e^2 + 10 * e - 5)/2/n
  var.reduction <- (var.mc - var.an)/var.mc
  return(round(rbind(ans_cov, ans_var, var.reduction), 3))

}
