#' scr3_19
#'
#' @description
#' The solution to the exercise 3_19 for Rizzo's Book.
#'
#' Suppose that A and B each start with a stake of $10,and bet $1 on consecutive coin flips. The
#' game ends when either one of the players has all the money. Let Sn be the fortune of player A
#' at time n. Then {Sn,n \eqn{\ge} 0} is a symmetric random walk with absorbing barriers at 0 and 20.
#' Simulate a realization of the process {Sn,n \eqn{\ge} 0} and plot Sn vs the time index from time 0
#' until a barrier is reached.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param p the probability of one side of coin flips.
#' @param s_min the minimum dallors to stop the random walk.
#' @param s_max the maximum dallors to stop the random walk.
#' @param s_start the current dollors the player held.
#'
#' @return plot
#' @export
#'
#'
#' @examples scr3_19(p=0.5,s_min=0,s_max=20,s_start=10)
#'
scr3_19<-function(p=0.5,s_min=0,s_max=20,s_start=10){
    prosess<-function(s){
      u<-runif(1)
      n<-s[length(s)]
      if(u > p){
        n<-n+1
      }
      else{
        n<-n-1
      }
      if(n<s_min||n>s_max){
        return(c(s,n))
      }
      else{
        return(prosess(c(s,n)))
      }
    }
    x<-prosess(s_start)
    plot(1:length(x),x,type='l',main = "Random walk", xlab = "Index", ylab = "Dollars")
}
