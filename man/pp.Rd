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
pp(x,a=0)
}
\arguments{
  \item{x}{A vector of data values. The vector is used to get sample size through \code{length()}.}
  \item{a}{A value for the plotting-position formula. Default is \code{A=0}, which returns the Weibull plotting positions}
}  
\value{
  An R \code{vector} is returned.
}
\note{
Various plotting positions have been suggested in the literature. Stedinger and others (1992, p. 18.25) comment that "all plotting positions give crude estimates of the unknown [non]exceedance probabilities associated with the largest (and smallest) events." The various plotting positions are summarized in the follow table.

\tabular{ccl}{
 \hline Name \tab a \tab Motivation \cr \hline 
 Weibull \tab 0 \tab Unbiased exceedance probability for all distributions \cr
 Median \tab 0.3175 \tab Median exceedance probabilities for all distributions \cr
 APL \tab about 0.35 \tab Often used with probability-weighted moments \cr
 Blom \tab 0.375 \tab Nearly unbiased quantiles for normal distribution \cr
 Cunnane \tab 0.40 \tab Approximately quantile unbiased \cr
 Gringorten \tab 0.44 \tab Optimized for Gumbel distribution \cr
 Hazen \tab 0.50 \tab A traditional choice \cr
 \hline \tab \tab
}

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
}
\keyword{univar}
\keyword{distribution}
