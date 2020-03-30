#' scr7_8
#' @description
#' The solution to the exercise 7_8 for Rizzo's Book.
#'
#'Refer to Exercise 7.7. Obtain the jackknife estimates of bias and standard error of ˆθ.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param B number of replicates.  Defaults to 200
#'
#' @import bootstrap
#'
#' @examples scr7_8(B=200)
#' @export
#'
scr7_8<-function(B=200){
  scor<-bootstrap::scor
  n<-length(scor[,1])
  x<-as.matrix(scor)
  lambda.hat<-eigen(cov(x))$values
  theta.hat<-lambda.hat[1]/sum(lambda.hat)
  theta.jack<-numeric(n)
  theta<-function(x){
    eigen(cov(x))$values[1]/sum(eigen(cov(x))$values)
  }
  for(i in 1:n){
    theta.jack[i]=theta(x[-i,])
    }
  bias.jack<-(n-1)*(mean(theta.jack)-theta.hat)
  theta.bar<-mean(theta.jack)
  se.jack<-sqrt((n-1)*mean((theta.jack-theta.bar)^2))
  print(list(theta.hat=theta(scor), bias=bias.jack,std.error=se.jack))
}

