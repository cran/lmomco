\name{pp}
\alias{pp}
\title{ Plotting-Position Formula }
\description{
The plotting positions of a data vector (\code{x}) are returned in ascending order. The plotting-position formula is

\deqn{pp_i = \frac{i-a}{n+1-2a} \mbox{,}}

where \eqn{pp_i} is the nonexceedance probability \eqn{F} of the \eqn{i}th ascending
data value. The parameter \eqn{a} specifies the plotting-position type, and \eqn{n} is the sample size (\code{length(x)}).
}
\usage{
pp(x, a=0, sort=TRUE)
}
\arguments{
  \item{x}{A vector of data values. The vector is used to get sample size through \code{length()};}
  \item{a}{A value for the plotting-position formula, default is \code{A=0}, which returns the Weibull plotting positions; and}
  \item{sort}{A logical whether the ranks of the data are sorted prior to \eqn{F} computation.}
}  
\value{
  An R \code{vector} is returned.
}
\note{
Various plotting positions have been suggested in the literature. Stedinger and others (1992, p. 18.25) comment that "all plotting positions give crude estimates of the unknown [non]exceedance probabilities associated with the largest (and smallest) events." The various plotting positions are summarized in the follow table.

\describe{
 \item{Weibull}{\eqn{a=0}, Unbiased exceedance probability for all distributions}
 \item{Median}{\eqn{a=0.3175}, Median exceedance probabilities for all distributions}
 \item{APL}{\eqn{\approx 0.35}, Often used with probability-weighted moments}
 \item{Blom}{\eqn{a=0.375}, Nearly unbiased quantiles for normal distribution}
 \item{Cunnane}{\eqn{a=0.40}, Approximately quantile unbiased}
 \item{Gringorten}{\eqn{a=0.44}, Optimized for Gumbel distribution}
 \item{Hazen}{\eqn{a=0.50}, A traditional choice}
}

The function uses the \code{rank()} function, which has specific settings to handle tied data. For implementation here, the \code{ties.method="first"} method to \code{rank()} is used.

}
\references{
Stedinger, J.R., Vogel, R.M., and Foufoula-Georgiou, E., 1992, Frequency
analysis of extreme events, in Handbook of Hydrology, chapter 18,
editor-in-chief D. A. Maidment: McGraw-Hill, New York.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{nonexceeds}}, \code{\link{pwm.pp}} }
\examples{
Q <- rnorm(20)
PP <- pp(Q)
plot(PP,sort(Q))

Q <- rweibull(30,1.4,scale=400)
WEI <- parwei(lmom.ub(Q))
PP <- pp(Q)
plot(PP,sort(Q))
lines(PP,quawei(PP,WEI))

# This plot looks similar, but when connecting lines are added
# the nature of the sorting is obvious.
plot(pp(Q,sort=FALSE), Q)
lines(pp(Q,sort=FALSE), Q, col=2)

}
\keyword{univar}
\keyword{distribution}
