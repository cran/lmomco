\name{pdftexp}
\alias{pdftexp}
\title{Probability Density Function of the Truncated Exponential Distribution}
\description{
This function computes the probability density
of the Truncated Exponential distribution given parameters (\eqn{\xi} and \eqn{\alpha}) of the
distribution computed by \code{\link{partexp}}. The probability density
function of the distribution is

\deqn{f(x) = \frac{\alpha^{-1}\mathrm{exp}(-t/\alpha)}{1 - \mathrm{exp}(-\xi/\alpha)}\mbox{,}}

where \eqn{f(x)} is the probability density for the quantile \eqn{x},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter, and \eqn{0 \le x \le \xi}.  The distribution has \eqn{0 < \tau_2 <= 1/2}, \eqn{\xi > 0}, and \eqn{1/\alpha \ne 0}.
}
\usage{
pdftexp(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{partexp}} or similar.}
}

\value{
  Probability density (\eqn{F}) for \eqn{x}.
}
\references{
Vogel, R.M., Hosking, J.R.M., Elphick, C.S., Roberts, D.L., and Reed, J.M., 2008, Goodness of fit of probability distributions for sightings as species approach extinction: Bulletin of Mathematial Biology, v. 71, no. 3, pp. 701--719.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdftexp}}, \code{\link{quatexp}}, \code{\link{partexp}}}
\examples{
  lmr <- vec2lmom(c(40,0.38), lscale=FALSE)
  expp <- partexp(lmr)
  x <- quatexp(.5,expp)
  pdftexp(x,expp)
}
\keyword{distribution}
