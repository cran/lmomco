\name{cdfnor}
\alias{cdfnor}
\title{Cumulative Distribution Function of the Normal Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Normal distribution given parameters of the distribution computed
by \code{\link{parnor}}.  The cumulative distribution function of the distribution is

\deqn{F(x) = \Phi(x-\mu/\sigma) \mbox{,}}

where \eqn{F(x)} is the nonexceedance probability for quantile \eqn{x},
\eqn{\mu} is the arithmetic mean, and \eqn{\sigma} is the standard deviation, and
\eqn{\Phi} is the cumulative distribution function of the standard normal
distribution. The R-function \code{pnorm} is used.
}
\usage{
cdfnor(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parnor}} or similar.}
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
\seealso{\code{\link{quanor}}, \code{\link{parnor}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  cdfnor(50,parnor(lmr))
}
\keyword{distribution}
