\name{quakap}
\alias{quakap}
\title{Quantile Function of the Kappa Distribution}
\description{
This function computes the quantiles of the Kappa distribution given
parameters (\eqn{\xi}, \eqn{\alpha}, \eqn{\kappa}, and \eqn{h}) of the distribution
computed by \code{\link{parkap}}.
The quantile function of the distribution is

\deqn{x(F) = \xi + \frac{\alpha}{\kappa}\left(1-{\left(\frac{1-F^h}{h}\right)}^\kappa\right)}

where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{f},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter,
\eqn{\kappa} is a shape parameter, and \eqn{h} is another shape parameter.
}
\usage{
quakap(f, para)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{parkap}} or similar.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\references{
Hosking, J.R.M., 1990, L-moments--Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105-124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis--An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfkap}}, \code{\link{parkap}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78,21,32,231,23))
  quakap(0.5,parkap(lmr))
}
\keyword{distribution}
