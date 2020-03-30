#' scr7_5
#' @description
#' The solution to the exercise 7_5 for Rizzo's Book.
#'
#'Refer to Exercise 7.4. Compute 95% bootstrap confidence intervals for the mean time between failures 1/λ by the standard normal, basic, percentile, and BCa methods. Compare the intervals and explain why they may differ.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param B number of replicates.  Defaults to 200
#'
#' @import boot
#'
#' @importFrom boot boot.ci
#'
#' @importFrom boot empinf
#'
#' @examples scr7_5(B=200)
#' @export
#'
scr7_5<-function(B=200){
  aircondit<-boot::aircondit
  hours<-aircondit$hours
  gaptime.hat<-mean(hours)
  boot<-boot(data<-aircondit, statistic=function(x,i){mean(x[i,])}, R=B )
  einf.jack<-empinf(boot, type='jack')
  print(boot.ci( boot, type=c('basic','perc','bca'), L=einf.jack ))
  hist(boot$t, main='', xlab=expression(1/lambda), prob=T)
  points(boot$t0, 0, pch = 19)
}
