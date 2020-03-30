##############################################
## Markov Chain Monte Carlo Methods sampler ##
##############################################
#' MCMCsim9
#' @description
#' The The Metropolis-Hastings algorithms are a class of
#' Markov Chain Monte Carlo methods including the Metropolis-Hasting sampler,
#' the Gibbs sampler, the independence sampler, and the random walk.
#'
#' Corresponding to Rizzo's book exercise 9.6, this function could generated
#' Markov Chains with any of the Metropolis-Hasting Method, the independence
#' Metropolis-Hasting Method, and the Random Walk Method for this problem by
#' specifying the `sample.method` term in the function.
#'
#' @usage MCMCsim9(...)
#'
#' @exportMethod
#' MCMCsim9(sample,method = c("MHS, "RWS", "IS"), burn = 2000, m = 15000)
#' @references Maria L. Rizzo. (2016). Statistical computing with r.
#'
#' @param m a number indicating the length of the generated Markov Chain. Defaults to 15000.
#' @param burn a number indicating the burn-in time. Defaults to 1000.
#' @param samole.method a charcter string specifying the Metropolis-Hsating method used to
#'        generate the Markov Chain, must be one of "MHS"(default), "IS" or "RWS",
#'        representing Metropois-Hsting sampler, Independent Sampler,
#'        and Random Walk Sampler respectively.
#'
#' @return `estimate theta hat` a number indicating the estimate theta.
#' @return method a character string indicating which method is used.
#' @return data.name a character string giving the names of the data
#' @return x a list of number containing the generated normal chain Xt.
#' @return reject_rate a number indicating the reject rate in this method.
#'
#'
#' @examples MCMCsim9(sample.method = "IS", burn = 1000, m = 10000)
#' @examples MCMCsim9(sample.method = "RWS", burn = 0, m = 10000)
#' @examples MCMCsim9()
#' @export
#'
#' @author Tianran Zhang <trzhang@gmail.com>
#'
MCMCsim9 <- function(sample.method = "MHS", burn = 2000, m = 15000){
  set.seed(135)
  sizes = c(125, 18, 20, 34)

  prob.vector = function (theta) {
    return(c(2 + theta, (1-theta), (1-theta), theta) / 4)
  }

  prob <- function (p) {
    if (p < 0 || p >= 1) return(0)
    return(prod(prob.vector(p)^sizes))
  }
  # random walk metropolis.
  # Default using unif(-0.25, 0.25) as step.
  rw.sampler <- function(m, w = 0.25){
    x.rw = numeric(m)
    k.rw = 0
    u = runif(m) # for accept/reject step
    v = runif(m, -w, w) # proposal distribution
    x.rw[1] = w
    for (i in 2:m) {
      y <- x.rw[i - 1] + v[i]
      #print(c(u[i], x.rw[i - 1], prob(x.rw[i - 1]), prob(y)/prob(x.rw[i - 1]), y))
      if (u[i] <= prob(y)/prob(x.rw[i - 1]))
        x.rw[i] <- y
      else{
        x.rw[i] <- x.rw[i - 1]
        k.rw = k.rw + 1
      }
    }
    return(list("x" = x.rw, rj_rate = k.rw/m))
  }


  ru = function(p, min_p = -0.8, max_p = 0.8) {
    return(runif(1,  min_p - abs(p), max_p + abs(p)))
  }

  du = function(x, p, min_p = -0.8, max_p = 0.8) {
    return(dunif(x, min_p - abs(p), max_p + abs(p)))
  }

  # metropolis hastings sampler
  mh.sampler <- function(m){
    x.mh = numeric(m)
    k.mh = 0

    u = runif(m)
    x.mh[1] = ru(0)
    for(i in 2:m) {
      xt <- x.mh[i-1]
      y <- ru(xt)
      r <-  prob(y) * du(xt, y) /prob(xt)/du(y, xt)
      if (!is.na(r) & u[i] <= r)
        x.mh[i] = y
      else {
        x.mh[i] = xt
        k.mh = k.mh + 1
      }
    }
    return(list("x" = x.mh, rj_rate = k.mh/m))
  }


  # independence sampler.
  i.sampler <- function(m){
    x.i = numeric(m)
    k.i = 0
    u = runif(m)
    x.i[1] = u[1]

    for (i in 2:m){
      xt = x.i[i-1]
      y = ru(0)
      r = prob(y)/prob(xt) * du(xt, 0)/du(y, 0)
      if (u[i] <= r) {
        x.i[i] = y
      } else {
        x.i[i] = xt
        k.i = k.i + 1
      }
    }
    return(list("x" = x.i, rj_rate = k.i/m))
  }

  if (sample.method == "MHS") {
    ans = mh.sampler(m)
    sample.method = 'M-H Method'
  }
  if (sample.method == "RWMS") {
    ans = rw.sampler(m)
    sample.method = 'Random Walk M-H Method'
  }
  if (sample.method == "IS"){
    ans = i.sampler(m)
    sample.method = 'Independent Method'
  }


  x = ans$x[(burn + 1):m]
  hist(x, probability = TRUE, main = "Distribution of the sampler")
  plot((burn + 1):m, x, type='l', xlab = "Index", ylab = "x",
       main = paste("Marcov Chain with\n", sample.method))
  ans["theta.hat"] = mean(x)

  dname <- deparse(substitute(data))
  sample.method = 'Metropolis-Hastings Method'
  title="Markov Chain Monte Carlo Sampler"
  rval<-list("estimate theta hat"=ans$theta.hat,
             method=sample.method,
             data.name=title,
             'x' = x,
             "reject_rate" = ans$rj_rate)
  class(rval)<-c("htest", "summaryDefault", "table")
  rval
}
