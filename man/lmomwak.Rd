\name{lmomwak}
\alias{lmomwak}
\title{L-moments of the Wakeby Distribution}
\description{
This function estimates the L-moments of the Wakeby distribution given
the parameters (\eqn{\xi}, \eqn{\alpha}, \eqn{\beta}, \eqn{\gamma},
and \eqn{\delta}) from \code{\link{parwak}}. The L-moments in terms of the
parameters are complicated and solved numerically.
}
\usage{
lmomwak(wakpara)
}
\arguments{
  \item{wakpara}{The parameters of the distribution.}
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
source of the L-moments: \dQuote{lmomwak--ordinary}.}
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
\seealso{\code{\link{parwak}}, \code{\link{quawak}}, \code{\link{cdfwak}} }
\examples{
lmr <- lmom.ub(c(123,34,4,654,37,78))
lmr
lmomwak(parwak(lmr))
}
\keyword{distribution}