\name{lmomkap}
\alias{lmomkap}
\title{L-moments of the Kappa Distribution}
\description{
This function estimates the L-moments of the Kappa distribution given
the parameters (\eqn{\xi}, \eqn{\alpha}, \eqn{\kappa}, and \eqn{h})
from \code{\link{parkap}}. The L-moments in terms of the
parameters are complicated and are solved numerically.
}
\usage{
lmomkap(para)
}
\arguments{
  \item{para}{The parameters of the distribution.}
}
\value{
  An R \code{list} is returned.

  \item{L1}{Arithmetic mean.}
  \item{L2}{L-scale---analogous to standard deviation.}
  \item{LCV}{coefficient of L-variation---analogous to coe. of variation.}
  \item{TAU3}{The third L-moment ratio or L-skew---analogous to skew.}
  \item{TAU4}{The fourth L-moment ratio or L-kurtosis---analogous to kurtosis.}
  \item{TAU5}{The fifth L-moment ratio.}
  \item{L3}{The third L-moment.}
  \item{L4}{The fourth L-moment.}
  \item{L5}{The fifth L-moment.}
  \item{source}{An attribute identifying the computational 
source of the L-moments: \dQuote{lmomkap}.}
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
\seealso{\code{\link{parkap}}, \code{\link{quakap}}, \code{\link{cdfkap}} }
\examples{
lmr <- lmom.ub(c(123,34,4,654,37,78))
lmr
lmomkap(parkap(lmr))
}
\keyword{distribution}
