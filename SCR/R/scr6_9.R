#' scr6_9
#' @description
#' The solution to the exercise 6_9 for Rizzo's Book.
#' 
#' Let X be a non-negative random variable with μ = E[X] < ∞. For a random sample x1, . . . , xn from the distribution of X. The Gini ratio is applied in economics to measure inequality in income distribution (see e.g. [163]). If the mean is unknown, let ˆG be the statistic G with μ replaced by ¯x. Estimate by simulation the mean, median and deciles of ˆG if X is standard lognormal. Repeat the procedure for the uniform distribution and Bernoulli(0.1). Also construct density histograms of the replicates in each case.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#' @param n random samples size. Defaults to 100
#' @param m number of response vectors to simulate. Defaults to 10000
#'
#' @export
#'
#' @examples scr6_9(n=100,m=10000)
#'
scr6_9<-function(n=100,m=10000){
  g <- numeric(m)
  for (i in 1:m){
    x <- sort(rlnorm(n))
    #x <- sort(runif(n))
    #x <- sort(rbinom(n, 1, 0.1))
    g[i] <- sum(seq(1-n, n-1, 2)*x)/(n^2*mean(x))
  }
  gmean <- mean(g)
  gmedian <- median(g)
  gdeciles <- sort(g)[floor(m*0.1)]
  cat('gmean: ', gmean, 'gmedian: ',gmedian, 'gdeciles: ', gdeciles)
  hist(g, prob = TRUE)
}

