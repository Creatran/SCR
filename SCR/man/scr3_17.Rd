% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scr3_17.R
\name{scr3_17}
\alias{scr3_17}
\title{scr3_17}
\usage{
scr3_17(n = 50, a = 2, b = 2, N = 100)
}
\arguments{
\item{n}{sample size}
}
\value{
list
}
\description{
The solution to the exercise 3_17 for Rizzo's Book.

Compare the performance of the Beta generator of Exercise 3.7, Example 3.8 and the R generator
rbeta. Fix the parameters a =2,b= 2 and time each generator on 1000 iterations with sample size
5000. (See Example 3.19.) Are the results different for different choices of a and b?
}
\examples{
scr3_17(n=50,a=2,b=2,N=100)
}
\references{
Maria L. Rizzo. (2016). Statistical computing with r.
}
