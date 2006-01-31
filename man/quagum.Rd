\name{quagum}
\alias{quagum}
\title{Quantile Function of the Gumbel Distribution}
\description{
This function computes the quantiles of the Gumbel distribution given
parameters (\eqn{\xi} and \eqn{\alpha}) of the distribution computed by
\code{\link{pargum}}.
The quantile function of the distribution is

\deqn{x(F) = \xi - \alpha\log(-\log(F))}

where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F},
\eqn{\xi} is a location parameter, and \eqn{\alpha} is a scale parameter.
}
\usage{
quagum(f, para)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{pargum}} or similar.}
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
\seealso{\code{\link{cdfgum}}, \code{\link{pargum}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  quagum(0.5,pargum(lmr))
}
\keyword{distribution}
