#' scr6_5
#' @description
#' The solution to the exercise 6_5 for Rizzo's Book.
#'
#' Suppose a 0.95 symmetric t-interval is applied to estimate a mean, but the sample data are non-normal. Then the probability that the confidence interval covers the mean is not necessarily equal to 0.95. Use aMonteCarlo experiment to estimate the coverage probability of the t-interval for random samples of χ2(2) data with sample size n = 20. Compare your t-interval results with the simulation results in Example 6.4. (The t-interval should be more robust to departures from normality than the interval for variance.)
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @details
#' Rizzo's Book P159
#'
#' The confidence level is the probability that the interval (U, V) covers the true value of the parameter θ. Evaluating the confidence level is therefore an integration problem.
#'
#' Monte Carlo estimate of the true confidence level. This type of simulation can be conveniently implemented by using the replicate function.
#'
#'
#' Two-sided t interval for one sample μ:
#'
#' UCL<-mean(x)+qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
#'
#' LCL<-mean(x)-qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
#'
#' One-sided inverval of variance for one sample σ2:
#'
#' UCL<-(n-1) * var(x) / qchisq(alpha, df = n-1)
#'
#' Related function:
#'
#' MCsimCL1()
#'
#' @param alpha significance level. Defaults to 0.05.
#' @param n sample size. Defaults to 20.
#' @param m number of replicates.  Defaults to 1000.
#'
#' @examples scr6_5(alpha=0.05,n=20,m=1000)
#' @export
#'
#'
#'
#'
scr6_5<-function(alpha=0.05,n=20,m=1000){
   #Two-sided t interval for one sample μ
   UCL<-numeric(m)
   LCL<-numeric(m)
   for(i in 1:m){
     x<-rchisq(n,2)
     UCL[i]<-mean(x)+qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
     LCL[i]<-mean(x)-qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
   }
  t_interval<-mean(LCL<2&UCL>2)
  #One-sided inverval of variance for one sample σ2
  UCL=replicate(1000,exp={
     x<-rchisq(n,2)
     (n-1)*var(x)/qchisq(alpha,df=n-1)
  })
  interval_for_variance<-mean(UCL>4)
  print(list("t interval"=t_interval,"interval for variance"=interval_for_variance))
}

