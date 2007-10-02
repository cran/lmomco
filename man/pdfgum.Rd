\name{pdfgum}
\alias{pdfgum}
\title{Probability Density Function of the Gumbel Distribution}
\description{
This function computes the probability density
of the Gumbel distribution given parameters (\eqn{\xi} and \eqn{\alpha}) of the distribution computed
by \code{\link{pargum}}. The probability density function of the distribution is

\deqn{f(x) = \alpha^{-1} e^{\left(-\frac{(x-\xi)}{\alpha}\right)}
              e^{-e^{\left(-\frac{(x-\xi)}{\alpha}\right)}} \mbox{,}}

where \eqn{f(x)} is the nonexceedance probability for quantile \eqn{x},
\eqn{\xi} is a location parameter, and \eqn{\alpha} is a scale parameter.
}
\usage{
pdfgum(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{pargum}} or similar.}
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
\seealso{\code{\link{cdfgum}}, \code{\link{quagum}}, \code{\link{pargum}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  gum <- pargum(lmr)
  x <- quagum(0.5,gum)
  pdfgum(x,gum)
}
\keyword{distribution}
