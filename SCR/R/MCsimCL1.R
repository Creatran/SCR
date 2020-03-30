#########################################################
##Monte Carlo experiment to estimate a confidence level##
#########################################################

#' Monte Carlo experiment to estimate a confidence level
#' @description
#' Monte Carlo method to assess the confidence level in an estimation procedure.
#' Empirical confidence level is an estimate of the confidence level obtained by simulation.
#' For the simulation experiment, repeat the steps above a large number of times,
#' and compute the proportion of intervals that contain the target parameter.
#'
#' @usage
#' MCsimCL1(...)
#' MCsimCL1(alpha=0.05,n=20,m=1000,type=c("mean","var"), interval = c("two.sided", "left", "right"), data=c("norm","chisq"),mu=0,sd=1,nv=1)
#'
#'
#' @param alpha a number indicating significance level. Defaults to 0.05.
#' @param n a number indicating random samples size generated from a lognormal distribution. Defaults to 20.
#' @param m a number indicating number of response vectors to simulate. Defaults to 1000.
#' @param type a character string specifying the parameter to estimate, must be one of "mean"(default),"var".
#' @param interval a character string specifying the interval type, must be one of "two.sided"(default) , "right" or "left".
#' @param data a character string specifying the data distribution, must be one of "norm"(default),"chisq".
#' @param mu a number indicating sample mean of normal distributed data. Defaults to 0.
#' @param sd a number indicating sample standard deviation of normal distributed data. Defaults to 1.
#' @param nv a number indicating degree freedom of chisq distributed data. Defaults to 1.
#'
#' @details
#' The formula interface is only applicable for the one sample simulation
#'
#' If type="norm", parameter mu and sd are used to simulate normal distributed sample. If type="chisq", parameter nv is used to simulate chi-square distributed sample
#'
#' interval="left" is the interval from one-sided lower confidence bound to positive infinite.
#'
#' @return
#' A list containing the following components:
#'
#' sample estimates:	the value of the estimated confidence level.
#'
#' method a character string indicating what type interval was simulated.
#'
#' data.name	a character string giving the name(s) of the data.
#'
#' @author
#' Yinuo Liu< smashingzing at gmail.com >
#'
#' @examples
#' MCsimCL1(0.01,100,1000,type="mean",interval="two.sided",data="norm")
#' MCsimCL1(type="mean",interval="two.sided",data="chisq")
#' @export


MCsimCL1<-function(alpha=0.05,n=20,m=1000,type=c("mean","var"),
                   interval = c("two.sided", "left", "right"),
                   data=c("norm","chisq"),mu=0,sd=1,nv=1,df=n-1){
  type<-match.arg(type)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu)))
    stop("'mu' must be a single number")
  if (!missing(alpha) && (length(alpha) != 1 || !is.finite(alpha) ||
                          alpha < 0 || alpha > 1))
    stop("'alpha' must be a single number between 0 and 1")
  UCL<-numeric(m)
  LCL<-numeric(m)
  method<-"a"
  if(type[1]=="mean"){
    if(data[1]=="norm"){
      data="norm"
      if(interval[1]=="left"){
        method<-"One-sided left t interval for normal distributed one sample μ"
        for(i in 1:m){
          x<-rnorm(n,mu,sd)
          LCL[i]<-mean(x)-qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
        }
        int<-mean(LCL<mu)
      }else if(interval[1]=="right"){
        method<-"One-sided right t interval for normal distributed one sample μ"
        for(i in 1:m){
          x<-rnorm(n,mu,sd)
          UCL[i]<-mean(x)+qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
        }
        int<-mean(UCL>mu)
      }else{
        method<-"Two-sided t interval for normal distributed one sample μ"
        interval="two.sided"
        for(i in 1:m){
          x<-rnorm(n,mu,sd)
          UCL[i]<-mean(x)+qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
          LCL[i]<-mean(x)-qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
        }
        int<-mean(LCL<mu&UCL>mu)
      }
    }else{
      data="chisq"
      if(interval[1]=="left"){
        method<-"One-sided left t interval for chisq distributed one sample μ"
        for(i in 1:m){
          x<-rchisq(n,nv)
          LCL[i]<-mean(x)-qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
        }
        int<-mean(LCL<nv)
      }else if(interval[1]=="right"){
        method<-"One-sided right t interval for chisq distributed one sample μ"
        for(i in 1:m){
          x<-rchisq(n,nv)
          UCL[i]<-mean(x)+qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
        }
        int<-mean(UCL>nv)
      }else{
        interval="two.sided"
        method<-"Two-sided t interval for chisq distributed one sample μ"
        for(i in 1:m){
          x<-rchisq(n,nv)
          UCL[i]<-mean(x)+qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
          LCL[i]<-mean(x)-qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
        }
        int<-mean(LCL<nv&UCL>nv)
      }

    }
  }else{
    type="var"
    if(data[1]=="norm"){
      data="norm"
      if(interval[1]=="left"){
        method<-"One-sided left interval of variance for normal distributed one sample σ2"
        for(i in 1:m){
          x<-rnorm(n,mu,sd)
          LCL[i]<-mean(x)-qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
        }
        int<-mean(LCL<sd^2)
      }else if(interval[1]=="right"){
        method<-"One-sided right interval of variance for normal distributed one sample σ2"
        for(i in 1:m){
          x<-rnorm(n,mu,sd)
          UCL[i]<-mean(x)+qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
        }
        int<-mean(UCL>sd^2)
      }else{
        interval="two.sided"
        method<-"Two-sided interval of variance for normal distributed one sample σ2"
        for(i in 1:m){
          x<-rnorm(n,mu,sd)
          UCL[i]<-mean(x)+qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
          LCL[i]<-mean(x)-qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
        }
        int<-mean(LCL<sd^2&UCL>sd^2)
      }
    }else{
      data="chisq"
      if(interval[1]=="left"){
        method<-"One-sided left interval of variance for chisq distributed one sample σ2"
        for(i in 1:m){
          x<-rchisq(n,nv)
          LCL[i]<-(n-1)*var(x)/qchisq(1-alpha,df=n-1)
        }
        int<-mean(LCL<nv^2)
      }else if(interval[1]=="right"){
        method<-"One-sided right interval of variance for chisq distributed one sample σ2"
        for(i in 1:m){
          x<-rchisq(n,nv)
          UCL[i]<-(n-1)*var(x)/qchisq(alpha,df=n-1)
        }
        int<-mean(UCL>nv^2)
      }else{
        interval="two.sided"
        method<-"Two-sided interval of variance for chisq distributed one sample σ2"
        for(i in 1:m){
          x<-rchisq(n,nv)
          UCL[i]<-(n-1)*var(x)/qchisq(alpha/2,df=n-1)
          LCL[i]<-(n-1)*var(x)/qchisq(1-alpha/2,df=n-1)
        }
        int<-mean(LCL<nv^2&UCL>nv^2)
      }
    }
  }
  dname <- deparse(substitute(data))
  title="Monte Carlo method to simulate the confidence level"
  rval<-list("estimate confidence level"=int, method=title,data.name=method)
  class(rval)<-c("htest","summaryDefault", "table")
  rval
}
