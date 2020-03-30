#' scr6_4
#' @description
#' The solution to the exercise 6_4 for Rizzo's Book.
#'
#' Suppose that X1, . . . , Xn are a random sample from a from a lognormal distribution with unknown parameters. Construct a 95% confidence interval for the parameter μ. Use aMonte Carlomethod to obtain an empirical estimate of the confidence level.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#' @param n random samples size generated from a lognormal distribution. Defaults to 1000
#' @param mu sample mean. Defaults to 0.
#' @param sigma ssample standard deviation. Defaults to 1.
#' @param m number of response vectors to simulate. Defaults to 1000
#' @param alpha significance level. Defaults to 0.05
#'
#' @examples scr6_4(n=1000,mu=500,m=1000,alpha=0.05)
#' @export
#'
scr6_4<-function(n=1000,mu=0,sigma=1,m=1000,alpha=0.05){
  Ub<-numeric(m)
  Lb<-numeric(m)
  for(i in 1:m){
    x<-rlnorm(n, meanlog=mu, sdlog=sigma)
    Ub[i]<-mean(log(x))+qt(alpha/2,df=n-1,lower.tail=F)*sd(log(x))/sqrt(n)
    Lb[i]<-mean(log(x))-qt(alpha/2,df=n-1,lower.tail=F)*sd(log(x))/sqrt(n)
  }
  CL<-c(mean(Lb),mean(Ub))
  CL
}


