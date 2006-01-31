\name{quaglo}
\alias{quaglo}
\title{Quantile Function of the Generalized Logistic Distribution}
\description{
This function computes the quantiles of the Generalized Logistic distribution given
parameters (\eqn{\xi}, \eqn{\alpha}, and \eqn{\kappa}) of the distribution computed by
\code{\link{parglo}}. The quantile function of the distribution is

\deqn{x(F) = \xi + \frac{\alpha}{\kappa}\left(1-\left(\frac{1-F}{F}\right)^\kappa\right)}

for \eqn{\kappa \ne 0}

\deqn{x(F) = \xi - \alpha\log{\left(\frac{1-F}{F}\right)}}

for \eqn{\kappa = 0}

where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter, and
\eqn{\kappa} is a shape parameter.
}
\usage{
quaglo(f, para)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{parglo}} or similar.}
}
\value{
  Quantile value for for nonexceedance probability \eqn{F}.
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
\seealso{\code{\link{cdfglo}}, \code{\link{parglo}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  quaglo(0.5,parglo(lmr))
}
\keyword{distribution}
