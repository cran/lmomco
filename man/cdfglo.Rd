\name{cdfglo}
\alias{cdfglo}
\title{Cumulative Distribution Function of the Generalized Logistic Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Generalized Logistic distribution given parameters (\eqn{\xi}, \eqn{\alpha},
and \eqn{\kappa}) of the distribution computed by \code{\link{parglo}}. The cumulative
distribution function of the distribution is

\deqn{F(x) = 1/(1+e^{-y})}
where \eqn{y} is

\deqn{y = -\kappa^{-1} \log\left(1 - \frac{\kappa(x-\xi)}{\alpha}\right)}
for \eqn{\kappa \ne 0}

\deqn{y = (x-\xi)/\alpha} for \eqn{\kappa = 0}
where \eqn{F(x)} is the nonexceedance probability for quantile \eqn{x},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter, and
\eqn{\kappa} is a shape parameter.
}
\usage{
cdfglo(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parglo}} or similar.}
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
\seealso{\code{\link{quaglo}}, \code{\link{parglo}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  cdfglo(50,parglo(lmr))
}
\keyword{distribution}
