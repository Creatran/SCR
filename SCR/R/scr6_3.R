#' scr6_3
#' @description
#' The solution to the exercise 6_3 for Rizzo's Book.
#' 
#' Plot the power curves for the t-test in Example 6.9 for sample sizes 10, 20, 30, 40, and 50, but omit the standard error bars. Plot the curves on the same graph, each in a different color or different line type, and include a legend. Comment on the relation between power and sample size.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.             
#'
#' @return plot
#' @export
#'
#' @examples scr6_3()
#'
scr6_3<-function(){
  size=seq(10,50,10)
  m=1000
  mu0=500
  sigma=100
  mu=c(seq(450,650,10))
  M=length(mu)
  power=matrix(0,length(size),M)
  j=1
  
  for(n in size){
    for(i in 1:M){
      mu1=mu[i]
      pvalues=replicate(m,expr={
        x=rnorm(n,mean=mu1,sd=sigma)
        ttest=t.test(x,alterbative="two.sided",mu=mu0)
        ttest$p.value
      })
      power[j,i]<-mean(pvalues<=0.05)
    }
    j=j+1
  }
  
  plot(mu,power[1,],col=1,lty=1,ylab="power",xlab="",main="Empirical Power Curve")
  for(i in 1:5){
    lines(mu,power[i,],col=i,lty=i)
  }
  abline(v=mu0,lty=1)
  abline(h=0.05,lty=1)
  legend("bottomright",paste("sample size",seq(10,50,10)),col=1:5,lty = 1:5,ncol=1,cex=0.5)
}


