\name{cdfgno}
\alias{cdfgno}
\title{Cumulative Distribution Function of the Generalized Normal Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Generalized Normal distribution given parameters (\eqn{\xi}, \eqn{\alpha},
and \eqn{\kappa}) of the distribution computed by \code{\link{pargno}}.
The cumulative distribution function of the distribution is

\deqn{F(x) = \Phi(y)}
where \eqn{\Phi} is the cumulative ditribution function of the
standard normal distribution and \eqn{y} is

\deqn{y = -\kappa^{-1} \log\left(1 - \frac{\kappa(x-\xi)}{\alpha}\right)}
for \eqn{\kappa \ne 0}

\deqn{y = (x-\xi)/\alpha} for \eqn{\kappa = 0}
where \eqn{F(x)} is the nonexceedance probability for quantile \eqn{x},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter, and
\eqn{\kappa} is a shape parameter.
}
\usage{
cdfgno(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{pargno}} or similar.}
}

\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
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
\seealso{\code{\link{quagno}}, \code{\link{pargno}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  cdfgno(50,pargno(lmr))
}
\keyword{distribution}
