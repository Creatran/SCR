#' scr3_20
#'
#' @description
#' The solution to the exercise 3_20 for Rizzo's Book.
#'
#' A compound Poisson process is a stochastic process {X(t),t \eqn{\ge} 0} that can be represented as the
#' random sum X(t), where {N(t),t\eqn{\ge} 0} is a Poisson process and Y1,Y2,... are iid
#' and independent of {N(t),t???0}.Write a program to simulate a compound Poisson(\eqn{\lambda})-Gamma
#' process (Y has a Gamma distribution). Estimate the mean and the variance of X(10) for several
#' choices of the parameters and compare with the theoretical values.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n the number of times the parameter sits changed.
#'
#' @return value
#' @export
#'
#'
#' @examples scr3_20(n=200)
#'
scr3_20<-function(n=100){
  spl<-function(t=10,lambda=1,a=1,b=1){
    n<-qpois(1-1e-8, lambda = lambda * t)
    s<-sum(rgamma(n,shape=a,scale=b))
    s
  }
  a<-runif(n,0,100)
  b<-runif(n,0,100)
  lambda<-runif(n,0,100)
  x<-NULL
  for(i in 1:n){
    x[i]<-spl(10,lambda[i],a[i],b[i])
  }
  mx<-mean(x)
  vx<-var(x)
  tm<-NULL
  for(i in 1:n){
    tm[i]<-lambda[i]*a[i]*b[i]
  }
  tv<-NULL
  for(i in 1:n){
    tv[i]<-(a[i]+1)*a[i]*b[i]
  }
  mt<-mean(tm)
  vt<-mean(tv)
  L<-list(mean=mx,mean_t=mt,variance=vx,variance_t=vt)
  L
}
