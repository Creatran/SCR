#' scr7_1
#' @description
#' The solution to the exercise 7_1 for Rizzo's Book.
#'
#' Compute a jackknife estimate of the bias and the standard error of the correlation statistic in Example 7.2.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @import bootstrap
#'
#' @examples scr7_1()
#' @export
#'
scr7_1<-function(){
  law<-bootstrap::law
  n<-nrow(law)
  theta.hat<-mean(law$LSAT)/mean(law$GPA)
  theta.hat
  theta.jack<-numeric(n)
  for(i in 1:n)
  {
    theta.jack[i]<-mean(law$LSAT[-i]/mean(law$GPA[-i]))
  }
  bias<-(n-1)*(mean(theta.jack)-theta.hat)
  bias
  cor.theta.jack<-numeric(n)
  for(i in 1:n)
  {
    cor.theta.jack[i]<-cor(law$LSAT[-i],law$GPA[-i])
  }
  se<-sqrt(((n-1))*mean((cor.theta.jack-mean(cor.theta.jack))^2))
  se
  print(list(bias=bias,std.error=se))
}

