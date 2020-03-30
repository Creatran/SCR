#' scr5_1
#' @description
#' The solution to Rizzo's book exercise 5.1.
#'
#' Compute a Monte Carlo estimate of \eqn{int_{0}^{\pi/3} sint dt} and compare
#' your estimate with the exact value of the integral.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#' @param m a number indicating iteration numbers. Defaults to 1000.
#'
#' @return ans a data.frame containing the the Monte Carlo estimate and the exact value of the
#' integral.
#' @export
#'
#' @examples scr5_1(10000)
#'


scr5_1 <- function(m = 1000){
  set.seed(135)
  x <- runif(m, min = 0, max = pi/3)

  theta.hat <- mean(sin(x)) * pi/3
  ans <- data.frame("Monte Carlo estimate"= round(theta.hat, 4),
                    "Eexact value" = 0.5)
  print(ans)
}

