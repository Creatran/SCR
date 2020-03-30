#' scr3_15
#'
#' @description
#' The solution to the exercise 3_15 for Rizzo's Book.
#'
#' Write a function that will standardize a multivariate normal sample for arbitrary n and d. That
#' is, transform the sample so that the sample mean vector is zero and sample covariance is the
#' identity matrix. To check your results, generate multivariate normal samples and print the
#' sample mean vector and covariance matrix before and after standardization.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n sample size
#'
#' @return list
#' @export
#' @importFrom GGally ggpairs
#' @importFrom MASS mvrnorm
#'
#'
#' @examples scr3_15(n=100)
#'
scr3_15<-function(n=100){
  Sigma <- matrix(c(1,-.5,.5,-.5,1,-.5,.5,-.5,1),3,3)
  mu<-c(0,1,2)
  x<-mvrnorm(n, mu, Sigma)
  m1<-mean(x[,1])
  m2<-mean(x[,2])
  m3<-mean(x[,3])
  mu1<-c(m1,m2,m3)
  A<-cbind((x[,1]-m1),(x[,2]-m2),(x[,3]-m3))
  s_b<-(t(A) %*% (A))
  x1<-A%*%solve(t(chol(Sigma)))
  m11<-mean(x1[,1])
  m12<-mean(x1[,2])
  m13<-mean(x1[,3])
  mu2<-c(m11,m12,m13)
  A2<-cbind((x1[,1]-m11),(x1[,2]-m12),(x1[,3]-m13))
  s_a<-(t(A) %*% (A))
  r<-list(sample_mean_before=mu1,sample_covariance_before=s_b,sample_mean_after=mu2,sample_covariance_after=s_a)
  r
}
