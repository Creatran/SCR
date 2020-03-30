#' scr8_1
#' @description
#' The solution to the exercise 8_1 for Rizzo's Book.
#'
#'Implement the two-sample Cram´er-von Mises test for equal distributions as a permutation test. Apply the test to the data in Examples 8.1 and 8.2.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param rep number of replicates.  Defaults to 1000
#'
#' @importFrom cramer cramer.test
#'
#' @examples scr8_1(rep=1000)
#' @export
#'
scr8_1<-function(rep=1000){
  f1 = sort(chickwts$weight[chickwts$feed == "soybean"])
  f2 = sort(chickwts$weight[chickwts$feed == "linseed"])
  feed = c(f1, f2)
  n = length(f1) + length(f2)
  R = numeric(rep)
  for (i in 1:rep) {
    s = sample(1:n, length(f1), replace = FALSE)
    feed1 = feed[s]
    feed2 = feed[-s]
    R[i] = cramer.test(feed1, feed2)$statistic
  }
  hist(R)
}

