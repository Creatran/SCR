#' scr6_1
#' @description
#' The solution to the exercise 6_1 for Rizzo's Book.
#' 
#' Estimate the MSE of the level k trimmed means for random samples of size 20 generated from a standard Cauchy distribution. (The target parameter θ is the center or median; the expected value does not exist.) Summarize the estimates of MSE in a table for k = 1, 2, . . . , 9.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#' @param n random samples size generated from a standard Cauchy distribution. Defaults to 20
#' @param k level k trimmed means
#' @param m number of response vectors to simulate. Defaults to 1000
#'
#' @return matrix 
#' @export
#'
#' @examples scr6_1(n=20,k=n/2-1,m=1000)
#'
scr6_1<-function(n=20,k=n/2-1,m=1000){
  mse=matrix(0,k,2)
  colnames(mse)=c("MSE","SE")
  rownames(mse)<-paste("k=", 1:k)

  trimmed.mse=function(n,m,k){
    tmean=numeric(m)
    for(i in 1:m){
      x=sort(rcauchy(n))
      tmean[i]=sum(x[(k+1):(n-k)])/(n-2*k)
    }

    mse.est=mean(tmean^2)
    se.mse=mean(tmean^2)
    se.mse=sqrt(mean((tmean-mean(tmean))^2))/sqrt(m)
    return(c(mse.est,se.mse))
  }
  for(k in 1:k){
    mse[k,]=trimmed.mse(n,m,k)
  }
  mse
}


