#' scr9_5
#' @description
#' The solution to the exercise 9_5 for Rizzo's Book.
#'
#' What eﬀect, if any, does the width w have on the mixing of the chain
#' in Example 9.5? Repeat the simulation keeping the random number seed ﬁxed,
#' trying diﬀerent proposal distributions based on the random increments from
#' Uniform(−w, w), varying w.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the length of the chain. Defaults to be 5000.
#' @param b a number indicating the actual value of data. Defaults to be 0.2.
#' @param burn a number indicating the burn-in time. Defaults to be 1000.
#' @param w a number indicating the width of the uniform support set. Defaults to be 0.25.
#' @param sed a number indicating the random seed number. Defaults to be 135.
#'
#' @return sample_mean a number indicating the sample mean of the generated chain.
#' @return plot showing the generated chain.
#' @export
#'
#' @examples scr9_5()
#'
#'
#'
scr9_5 <- function(m = 5000, b = 0.2, burn = 1000, w = 0.25, sed = 135){
  set.seed(sed)
  days <- 250
  x <- numeric(m)

  i <- sample(1:5, size=days, replace=TRUE,
              prob=c(1, 1-b, 1-2*b, 2*b, b))
  win <- tabulate(i)

  prob <- function(y, win) { # computes (without the constant) the target density
    if (y < 0 || y >= 0.5) return (0)
    return((1/3)^win[1] *
             ((1-y)/3)^win[2] * ((1-2*y)/3)^win[3] *
             ((2*y)/3)^win[4] * (y/3)^win[5])
  }

  u <- runif(m)
  v <- runif(m, -w, w)
  x[1] <- .25
  for (i in 2:m) {
    #for accept/reject step #proposal distribution
    y <- x[i-1] + v[i]
    if (u[i] <= prob(y, win) / prob(x[i-1], win))
      x[i] <- y else x[i] <- x[i-1]
  }
  plot(burn:m, x[burn:m], type = "l", xlab = "Index", ylab = "x",
       main = paste("Generated chain with w = ", w))

  sample_mean <- mean(x[burn:m])
  return(sample_mean)
}
