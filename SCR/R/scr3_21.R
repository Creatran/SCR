#' scr3_21
#'
#' @description
#' The solution to the exercise 3_21 for Rizzo's Book.
#'
#' A nonhomogeneous Poisson process has mean value function m(t)=t2 +2t, t \eqn{\ge} 0. Determine the
#' intensity function \eqn{\lambda} (t) of the process, and write a program to simulate the process on the
#' interval [4,5]. Compute the probability distributionof N(5)-N(4),and compare it to the
#' empirical estimate obtained by replicating the simulation.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param n sample size
#'
#' @return list
#' @export
#'
#'
#' @examples scr3_21(n=100)
scr3_21<-function(n=100,in_min=4,in_max=5){
  spl<-function(lambda=3,upper=100){
    N <- rpois(1, lambda * upper)
    Tn <- rexp(N, lambda)
    Sn <- cumsum(Tn)
    Un <- runif(N)
    keep <- (Un <= Sn^2+2*Sn)
    Sn[keep]
    l<-sum(Sn[keep] <= in_min)
    u<-sum(Sn[keep] <= in_max)
    interval<-u-l
    interval
  }
  l<-runif(n,0,10)
  y<-NULL
  for(i in 1:n){
    y[i]<-spl(l[i],100)
  }
  plot(1:n,y,type='l',xlab = "index",ylab = "interval")
}
