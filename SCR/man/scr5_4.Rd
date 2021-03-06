% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scr5_4.R
\name{scr5_4}
\alias{scr5_4}
\title{scr5_4}
\usage{
scr5_4(m = 1000)
}
\arguments{
\item{m}{a number indicating the iteration number. Defaults to 1000.}
}
\value{
a data.frame containing the Monte Carlo estimate of Beta(3, 3) cdf for x = 0.1,
0.2, ..., 0.9, and the estimate with the values returned by pbeta function in R.
}
\description{
The solution to the Rizzo's book exercise 5_4.

Write a function to compute a Monte Carlo estimate of the Beta(3, 3) cdf,
and use the function to estimate F(x) for x = 0.1, 0.2, . . ., 0.9.
Compare the estimates with the values returned by the pbeta function in R.
}
\examples{
scr5_4(m = 5000)

}
\references{
Maria L. Rizzo. (2016). Statistical computing with r.
}
