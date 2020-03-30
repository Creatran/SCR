#' scr7_7
#' @description
#' The solution to the exercise 7_7 for Rizzo's Book.
#'
#'Refer to Exercise 7.6. Efron and Tibshirani discuss the following example [84, Ch. 7]. The five-dimensional scores data have a 5 × 5 covariance matrix Σ, with positive eigenvalues λ1 > · · · > λ5. Let ˆλ1 > · · · > ˆλ5 be the eigenvalues of ˆΣ, where ˆΣ is the MLE of Σ. Compute the sample estimate of θ. Use bootstrap to estimate the bias and standard error of ˆθ.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param B number of replicates.  Defaults to 200
#'
#' @import bootstrap
#' @importFrom boot boot
#'
#' @examples scr7_7(B=200)
#' @export
#'
scr7_7<-function(B=200){
  scor<-bootstrap::scor
  lambda.hat<-eigen(cov(scor))$values
  theta.hat<-lambda.hat[1]/sum(lambda.hat)
  theta.i<-function(x,i){
    eigen(cov(x[i,]))$values[1]/sum(eigen(cov(x[i,]))$values)
  }
  boot<-boot(data=scor,statistic=theta.i,R=B)
  boot
}
