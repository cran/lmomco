\name{parpe3}
\alias{parpe3}
\title{Estimate the Parameters of the Pearson Type III Distribution}
\description{
This function estimates the parameters of the Pearson Type III distribution given
the L-moments of the data in an L-moment object such as that returned by
\code{lmom.ub}. The L-moments in terms of the parameters are
complicated and solved numerically.

For the implementation in the \pkg{lmomco} package, the three parameters are \eqn{\mu}, \eqn{\sigma}, and \eqn{\gamma} for the mean, standard deviation, and skew, respectively. Therefore, the Pearson Type III distribution is of considerable theoretical interest to this package because the parameters, which are estimated via the L-moments, are in fact the product moments. Although, these values fitted by the method of L-moments will not be numerically equal to the sample product moments. Further details are provided in the examples section of the \code{\link{pmoms}} function documentation.
}
\usage{
parpe3(lmom)
}
\arguments{
  \item{lmom}{A L-moment object created by \code{\link{lmom.ub}}
or \code{\link{pwm2lmom}}.}
}

\value{
  An R \code{list} is returned.

  \item{type}{The type of distribution: \code{pe3}.}
  \item{para}{The parameters of the distribution.}
  \item{source}{The source of the parameters: \dQuote{parpe3}.}
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
\seealso{\code{\link{lmom.ub}}, \code{\link{lmompe3}},
\code{\link{cdfpe3}}, \code{\link{quape3}}
}
\examples{
lmr <- lmom.ub(rnorm(20))
parpe3(lmr)
}
\keyword{distribution}
