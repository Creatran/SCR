#' scr6_8
#' @description
#' The solution to the exercise 6_8 for Rizzo's Book.
#'
#' Refer to Example 6.16. Repeat the simulation, but also compute the F test of equal variance, at significance level α = 0.055. Compare the power of the Count Five test and F test for small, medium, and large sample sizes. (Recall that the F test is not applicable for non-normal distributions.)
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @return matrix
#' @export
#'
#' @examples scr6_8()
#'
scr6_8<-function(){
  count5test<-function(x,y){
    X=x-mean(x)
    Y=y-mean(y)
    outx=sum(X>max(Y))+sum(X<min(Y))
    outy=sum(Y>max(X))+sum(Y<min(X))
    return(as.integer(max(c(outx,outy))>5))
  }
  sample_size<-c(20,500,2000)
  m=1000
  sigma1=1
  sigma2=1.5

  count5_result=numeric(m)
  ftest_result=numeric(m)

  power=numeric(3)
  f_test=numeric(3)

  index=1

  for(size in sample_size){
    for(i in 1:m){
      x=rnorm(size,0,sigma1)
      y=rnorm(size,0,sigma2)
      count5_result[i]=count5test(x,y)
      ftest_result[i]=var.test(x,y,alternative="two.sided")$p.value
    }
    power[index]=mean(count5_result)
    f_test[index]=mean(ftest_result>0.055)
    index=index+1
  }
  a<-rbind(sample_size,power,f_test)
  colnames(a)<-c(1,2,3)
  a
}

