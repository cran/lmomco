\name{lmompe3}
\alias{lmompe3}
\title{L-moments of the Pearson Type III Distribution}
\description{
This function estimates the L-moments of the Pearson Type III distribution
given the parameters (\eqn{\mu}, \eqn{\sigma}, and \eqn{\gamma}) from 
\code{\link{parpe3}}. The L-moments in terms of the parameters are
complicated and solved numerically.

For the implementation in the \pkg{lmomco} package, the three parameters are \eqn{\mu}, \eqn{\sigma}, and \eqn{\gamma} for the mean, standard deviation, and skew, respectively. Therefore, the Pearson Type III distribution is of considerable theoretical interest to this package because the parameters, which are estimated via the L-moments, are in fact the product moments. Although, these values fitted by the method of L-moments will not be numerically equal to the sample product moments.  Further details are provided in the examples section of the \code{\link{pmoms}} function documentation.
}
\usage{
lmompe3(para)
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
source of the L-moments: \dQuote{lmompe3}.}
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
\seealso{\code{\link{parpe3}}, \code{\link{quape3}}, \code{\link{cdfpe3}} }
\examples{
lmr <- lmom.ub(c(123,34,4,654,37,78))
lmr
lmompe3(parpe3(lmr))
}
\keyword{distribution}
