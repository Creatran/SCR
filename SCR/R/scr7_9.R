#' scr7_9
#' @description
#' The solution to the exercise 7_9 for Rizzo's Book.
#'
#'Refer to Exercise 7.7. Compute 95% percentile and BCa confidence intervals for ˆθ.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param B number of replicates.  Defaults to 200
#'
#' @import bootstrap
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom boot empinf
#'
#' @examples scr7_9(B=200)
#' @export
#'
scr7_9<-function(B=200){
  scor<-bootstrap::scor
  theta.i<-function(x,i){
    eigen(cov(x[i,]))$values[1]/sum(eigen(cov(x[i,]))$values)
  }
  boot<-boot(data=scor,statistic=theta.i,R=B)
  einf.jack<-empinf(boot, type='jack')
  print(boot.ci(boot,type=c('perc','bca'),L=einf.jack))
}
