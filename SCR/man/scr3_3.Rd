% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scr3_3.R
\name{scr3_3}
\alias{scr3_3}
\title{scr3_3}
\usage{
scr3_3(n = 1000, a = 2, b = 2)
}
\arguments{
\item{n}{Sample size}

\item{a}{The parameter of pareto.}

\item{b}{The parameter of pareto.}
}
\value{
plot
}
\description{
The solution to the exercise 3_3 for Rizzo's Book.

Derive the probability inverse transformation F1(U) and use the inverse transform method to
simulate a random sample from the Pareto(2, 2) distribution. Graph the density histogram of the 
sample with the Pareto(2, 2) density superimposed for comparison
}
\examples{
scr3_3(n=1000,a=2,b=2)

}
\references{
Maria L. Rizzo. (2016). Statistical computing with r.
}
