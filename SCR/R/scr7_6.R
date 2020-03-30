#' scr7_6
#' @description
#' The solution to the exercise 7_6 for Rizzo's Book.
#'
#'Efron and Tibshirani discuss the scor (bootstrap) test score data on 88 students who took examinations in five subjects [84, Table 7.1], [188, Table 1.2.1]. The first two tests (mechanics, vectors) were closed book and the last three tests (algebra, analysis, statistics) were open book. Each row of the data frame is a set of scores (xi1, . . . , xi5) for the ith student. Use a panel display to display the scatter plots for each pair of test scores. Compare the plot with the sample correlation matrix. Obtain bootstrap estimates of the standard errors for each of the following estimates: ˆρ12 = ˆρ(mec, vec), ˆρ34 = ˆρ(alg, ana), ˆρ35 = ˆρ(alg, sta), ˆρ45 = ˆρ(ana, sta).
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param B number of replicates.  Defaults to 200
#'
#' @import bootstrap
#'
#' @examples scr7_6(B=200)
#' @export
#'
scr7_6<-function(B=200){
  scor<-bootstrap::scor
  pairs(scor,)
  boot<-boot(data=cbind(scor$mec,scor$vec), statistic=function(x,i){cor(x[i,1],x[i,2])}, R=B )
  print(list(correlation=cor(scor),mean=mean(boot$t), median=median(boot$t)))
}
