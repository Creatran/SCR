#' scr7_3
#' @description
#' The solution to the exercise 7_3 for Rizzo's Book.
#'
#' Obtain a bootstrap t confidence interval estimate for the correlation statistic in Example 7.2 (law data in bootstrap).
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param B number of replicates.  Defaults to 200
#' @param alpha significance level
#'
#' @import bootstrap
#'
#' @importFrom  boot boot
#'
#' @examples scr7_3(B=200,alpha=0.05)
#' @export
#'
scr7_3<-function(B=200,alpha=0.05){
  law <- bootstrap::law
  n<-nrow(law)
  R<-numeric(B)
  for(b in 1:B){
    i <- sample(1:n, size = n, replace = TRUE)
    LSAT<-law$LSAT[i]
    GPA<-law$GPA[i]
    R[b]<-cor(LSAT, GPA)
  }
  se.R <- sd(R)
  r<-function(x,i){
    cor(x[i,1],x[i,2])
  }
  library(boot)
  obj<-boot(data=law, statistic=r, R=2000)
  y<-obj$t
  sd<-sd(y)
  CI<-c(se.R-qt(1-alpha/2,n-1)*sd,se.R+qt(1-alpha/2,n-1)*sd)
  CI
}

