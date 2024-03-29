\name{pdfglo}
\alias{pdfglo}
\title{Probability Density Function of the Generalized Logistic Distribution}
\description{
This function computes the probability density of the Generalized Logistic distribution given parameters (\eqn{\xi}, \eqn{\alpha}, and \eqn{\kappa})  computed by \code{\link{parglo}}. The probability density function is
\deqn{f(x) = \frac{\alpha^{-1} \exp(-(1-\kappa)Y)}{[1+\exp(-Y)]^2} \mbox{,}}
where \eqn{Y} is
\deqn{Y = -\kappa^{-1} \log\left(1 - \frac{\kappa(x-\xi)}{\alpha}\right)
\mbox{,}}
for \eqn{\kappa \ne 0}, and
\deqn{Y = (x-\xi)/\alpha\mbox{,}}
for \eqn{\kappa = 0}, and where \eqn{f(x)} is the probability density for quantile \eqn{x}, \eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter, and \eqn{\kappa} is a shape parameter.
}
\usage{
pdfglo(x, para)
}
\arguments{
  \item{x}{A real value vector.}
  \item{para}{The parameters from \code{\link{parglo}} or \code{\link{vec2par}}.}
}

\value{
  Probability density (\eqn{f}) for \eqn{x}.
}
\references{
Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, v. 52, pp. 105--124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{W.H. Asquith}
\seealso{\code{\link{cdfglo}}, \code{\link{quaglo}}, \code{\link{lmomglo}}, \code{\link{parglo}}}
\examples{
  lmr <- lmoms(c(123,34,4,654,37,78))
  glo <- parglo(lmr)
  x <- quaglo(0.5,glo)
  pdfglo(x,glo)
}
\keyword{distribution}
\keyword{probability density function}
\keyword{Distribution: Generalized Logistic}
