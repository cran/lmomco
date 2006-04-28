\name{pp}
\alias{pp}
\title{ Plotting-Position Formula }
\description{
The plotting positions of a data vector (\code{x}) are returned in ascending order. The plotting-position formula is

\deqn{pp_i = \frac{i+A}{n+B} \mbox{,}}

where \eqn{pp_i} is the nonexceedance probability \eqn{F} of the \eqn{i}th ascending
data value. The parameters \eqn{A} and \eqn{B} together specify the plotting-position type, and \eqn{n} is the sample size (\code{length(x)})..
}
\usage{
pp(x,A=0,B=1)
}
\arguments{
  \item{x}{A vector of data values.}
  \item{A}{A value for the plotting-position formula.}
  \item{B}{Another value for the plotting-position formula.}
}  
\value{
  An R \code{vector} is returned.
}
\note{
The Weibull plotting-position formula is the default.
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
