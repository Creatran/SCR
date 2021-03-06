% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scr9_4.R
\name{scr9_4}
\alias{scr9_4}
\title{scr9_4}
\usage{
scr9_4(m = 2000, sigma = c(0.05, 0.5, 2, 16), x0 = 10)
}
\arguments{
\item{m}{a number indicating the iteration numbers. Defaults to 2000}

\item{sigma}{a list of number(s) indicating different variances used for the proposal
distribution. Defaults to [0.05, 0.5, 2, 16]}

\item{x0}{a number indicating the first value set for the chain. Defaluts to 10}
}
\value{
data.frame containing the acceptance rates of each chain with different sigmas.

plot showing the chains generated from Random Walk Metropolis Method with different
sigmas.
}
\description{
The solution to the exercise 9_4 for Rizzo's Book.

Implement a random walk Metropolis sampler for generating the
standard Laplace distribution (see Exercise 3.2). For the increment,
simulate from a normal distribution.
Compare the chains generated when diﬀerent variances are used for
the proposal distribution. Also, compute the acceptance rates of each chain.
}
\details{
The plots show that random walk Metropolis sampler is very sensitive to the variance of
the proposal distribution. Only the third chain has a rejection rate in the range [0.15, 0.75].
In the ﬁrst plot with σ = 0.05, the acceptance rate tend to be large and almost every candidate
point is accepted. The increments are small and the chain is almost like a true random walk.
Chain 1 has not converged to the target in 2000 iterations.
The chain in the second plot generated with σ = 0.5 is converging very slowly and requires
a much longer burn-in period.
In the third plot (σ = 2) the chain is mixing well and converging to the target distribution
after a short burn-in period of about 500.
Finally, in the fourth plot, where σ = 16, the acceptance rate are smaller and most of the
candidate points are rejected. The fourth chain converges, but it is ineﬃcient.
}
\examples{
scr9_4()



}
\references{
Maria L. Rizzo. (2016). Statistical computing with r.
}
