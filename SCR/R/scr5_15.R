#' scr5_15
#' @description
#' The solution of Rizzo's book exercise 5.15.
#'
#' Obtain the stratiﬁed importance sampling estimate in Example 5.13
#' and compare it with the result of Example 5.10.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the iteration numbers. Defaults to 1000.
#'
#' @return data.frame containing the estimates of theta hat and estimation variances
#' corresponding to example 5.10 and 5.13 respectively.
#' @export
#'
#' @examples scr5_15(m = 5000)
#'
#'
#'
scr5_15 <- function(m = 1000){
  g <- function(x) {
    exp(-x - log(1+x^2)) * (x > 0) * (x < 1) }

  # The best result in Ex10
  u <- runif(m) #f3, inverse transform method
  x <- - log(1 - u * (1 - exp(-1)))
  fg <- g(x) / (exp(-x) / (1 - exp(-1)))
  theta.hat_ex10 <- mean(fg)
  var_ex10 <- var(fg)

  # Stratified importance sampling in Ex13
  theta.hat_ex13 <- 0
  var_ex13 <- 0
  for (j in 0:4){
    u <- runif(m/5, min = j/5, max = (j + 1)/5)
    x <- - log(1 - u * (1 - exp(-1)))
    fg <- g(x) / (exp(-x) / (1 - exp(-1)))/5
    theta.hat_ex13 <- theta.hat_ex13 + mean(fg)
    var_ex13 <- var_ex13 + var(fg)
  }
  var_ex13 <- var_ex13/5

  return(round(rbind(theta.hat_ex10, var_ex10, theta.hat_ex13, var_ex13), 6))
}
