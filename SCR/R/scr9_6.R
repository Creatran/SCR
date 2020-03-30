#' scr9_6
#' @description
#' The solution to the exercise 9_6 for Rizzo's Book.
#'
#' Rao [220, Sec. 5g] presented an example on genetic linkage of 197 animals in
#' four categories. The group sizes are (125, 18, 20, 34). Assume that the probabilities of the
#' corresponding multinomial distribution are ((2 + θ)/4, (1 - θ)/4, (1 - θ)/4, θ/4).
#' Estimate the posterior distribution of θ given the observed sample,
#' using one of the methods in this chapter.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the length of the chain. Defaults to be 10000.
#' @param burn a number indicating the burn-in time. Defaults to be 1000.
#'
#' @return `estimate theta hat` a number indicating the estimate theta.
#' @return method a character string indicating which method is used.
#' @return data.name a character string giving the names of the data
#' @return x a list of number containing the generated normal chain Xt.
#' @return reject_rate a number indicating the reject rate in this method.
#' @return plot showing the generated chain.
#' @export
#'
#' @examples scr9_6()
#'
#' @details The results are generated with the Metropolis-Hasting Method.
#' More details about this exercise could be found in the expanded function
#' for this Chapter by referring `MCMCsim9`
#'
#'
#'
scr9_6 <- function(m = 10000, burn = 1000){
  MCMCsim9(sample.method = "MHS")
}
