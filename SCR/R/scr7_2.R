#' scr7_2
#' @description
#' The solution to the exercise 7_2 for Rizzo's Book.
#'
#' Refer to the law data (bootstrap). Use the jackknife-after-bootstrapmethod to estimate the standard error of the bootstrap estimate of se(R).
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param B number of replicates.  Defaults to 200
#'
#' @import bootstrap
#'
#' @examples scr7_2(B=200)
#' @export
#'
scr7_2<-function(B=200){
  law<-bootstrap::law
  n<-nrow(law)
  R<-numeric(B)
  for (b in 1:B) {
    i<-sample(1:n,size=n,replace=TRUE)
    LSAT<-law$LSAT[i]
    GPA<-law$GPA[i]
    R[b]<-cor(LSAT, GPA)
  }
  print(sd(R))
}


