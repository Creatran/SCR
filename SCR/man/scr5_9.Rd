% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scr5_9.R
\name{scr5_9}
\alias{scr5_9}
\title{scr5_9}
\usage{
scr5_9(m = 1000)
}
\arguments{
\item{m}{a number indicating the iteration numbers.}
}
\value{
data.frame containing the variance of (X + X')/2 and (X1 + X2)/2,
and the variance reduction percent.
}
\description{
The solution to the exercise 5_9 for Rizzo's Book.

Implement a function to generate samples from a Rayleigh(σ) distribution,
using antithetic variables. What is the percent reduction in variance of (X+X')/2
compared with (X1 + X2)/2 for independent X1, X2 ?
}
\examples{
scr5_9()


}
\references{
Maria L. Rizzo. (2016). Statistical computing with r.
}
