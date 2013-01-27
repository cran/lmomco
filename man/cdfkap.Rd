\name{cdfkap}
\alias{cdfkap}
\title{Cumulative Distribution Function of the Kappa Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Kappa distribution given parameters (\eqn{\xi}, \eqn{\alpha}, and \eqn{\kappa}, \eqn{h}) of the distribution computed
by \code{\link{parkap}}. The cumulative distribution function of the distribution is

\deqn{F(x) = \left(1-h\left(1-\frac{\kappa(x-\xi)}{\alpha}\right)^{1/\kappa}\right)^{1/h} \mbox{,}}

where \eqn{F(x)} is the nonexceedance probability for quantile \eqn{x},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter,
\eqn{\kappa} is a shape parameter, and \eqn{h} is another shape parameter.
}
\usage{
cdfkap(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parkap}} or similar.}
}

\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
}
\references{
Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105--124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{quakap}}, \code{\link{parkap}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78,21,32,231,23))
  cdfkap(50,parkap(lmr))
}
\keyword{distribution}
\keyword{cumulative distribution function}
