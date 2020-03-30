#' scr7_4
#' @description
#' The solution to the exercise 7_4 for Rizzo's Book.
#'
#' Refer to the air-conditioning data set aircondit provided in the boot package. The 12 observations are the times in hours between failures of airconditioning equipment [63, Example 1.1]: 3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487. Assume that the times between failures follow an exponential model Exp(λ). Obtain the MLE of the hazard rate λ and use bootstrap to estimate the bias and standard error of the estimate.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param B number of replicates.  Defaults to 200
#'
#' @import boot
#'
#' @examples scr7_4(B=200)
#' @export
#'
scr7_4<-function(B=200){
  aircondit<-boot::aircondit
  hours<-aircondit$hours
  n<-length(hours)
  lambda.hat<-n/sum(hours)
  lambda.hat
  bias.lambda.boot<-mean(hours)-mean(lambda.hat)
  theta.boot<-numeric(B)
  for(b in 1:B)
  {
    x<-sample(hours, size=n, replace=T)
    theta.boot[b]<-n/sum(x)
  }
  bias.lambda.boot<-mean(theta.boot)-1/mean(hours)
  se.lambda.boot<-sd(theta.boot)
  se.lambda.boot
  print(list(MLE=lambda.hat,bias=bias.lambda.boot,std.error=se.lambda.boot))
}

