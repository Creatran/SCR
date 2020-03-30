#' scr6_2
#' @description
#' The solution to the exercise 6_2 for Rizzo's Book.
#' 
#' Plot the empirical power curve for the t-test in Example 6.9, changing the alternative hypothesis to H1 : μ ≠ 500, and keeping the significance level α = 0.05.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#' @param n random samples size. Defaults to 20
#' @param m number of response vectors to simulate. Defaults to 1000
#' @param mu0 null hypothesis. Defaults to 500
#'
#' @return plot
#' @export
#'
#' @examples scr6_2(n=20,m=1000, mu0=500)
#'
scr6_2<-function(n=20,m=1000, mu0=500){
  sigma=100
  mu=c(seq(450,650,10))
  M=length(mu)
  power=numeric(M)

  for(i in 1:M){
    mu1=mu[i]
    pvalues=replicate(m,expr={
      x=rnorm(n,mean=mu1,sd=sigma)
      ttest=t.test(x,alternatice="two.sided",mu=mu0)
      ttest$p.value
    }
    )
    power[i]<-mean(pvalues<=0.05)
  }
  plot(mu,power,main="Empirical Power Curve")
  abline(v=mu0, lty=1)
  abline(h=0.05,lty=1)
}
