\name{cdfgpa}
\alias{cdfgpa}
\title{Cumulative Distribution Function of the Generalized Pareto Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Generalized Pareto distribution given parameters (\eqn{\xi}, \eqn{\alpha}, and \eqn{\kappa})
of the distribution computed
by \code{\link{pargpa}}. The cumulative distribution function of the distribution is

\deqn{F(x) = 1 - e^{-y} \mbox{,}}
where \eqn{y} is

\deqn{y = -\kappa^{-1} \log\left(1 - \frac{\kappa(x-\xi)}{\alpha}\right)
\mbox{ for } \kappa \ne 0 \mbox{, and}}

\deqn{y = (x-\xi)/A \mbox{ for } \kappa = 0 \mbox{,}}
where \eqn{F(x)} is the nonexceedance probability for quantile \eqn{x},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter, and
\eqn{\kappa} is a shape parameter.
}
\usage{
cdfgpa(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{pargpa}} or similar.}
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
\seealso{\code{\link{quagpa}}, \code{\link{pargpa}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  cdfgpa(50,pargpa(lmr))
}
\keyword{distribution}