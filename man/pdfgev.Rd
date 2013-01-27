\name{pdfgev}
\alias{pdfgev}
\title{Probability Density Function of the Generalized Extreme Value Distribution}
\description{
This function computes the probability density
of the Generalized Extreme Value distribution given parameters (\eqn{\xi},
\eqn{\alpha}, and \eqn{\kappa}) of the distribution computed by \code{\link{pargev}}. The
probability density function of the distribution is

\deqn{f(x) = \alpha^{-1} e^{-(1-\kappa)y - e^{-y}} \mbox{,}}

\deqn{y = -\kappa^{-1} \log\left(1 - \frac{\kappa(x-\xi)}{\alpha}\right) \mbox{ for } \kappa \ne 0 \mbox{, and}}

\deqn{y = (x-\xi)/\alpha \mbox{ for } \kappa = 0 \mbox{,} }
where \eqn{f(x)} is the probability density for quantile \eqn{x},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter, and
\eqn{\kappa} is a shape parameter.
}
\usage{
pdfgev(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{pargev}} or similar.}
}

\value{
  Probability density (\eqn{f}) for \eqn{x}.
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
\seealso{\code{\link{pdfgev}}, \code{\link{quagev}}, \code{\link{pargev}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  gev <- pargev(lmr)
  x <- quagev(0.5,gev)
  pdfgev(x,gev)
}
\keyword{distribution}
\keyword{probability density function}
