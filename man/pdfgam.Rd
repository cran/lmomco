\name{pdfgam}
\alias{pdfgam}
\title{Probability Density Function of the Gamma Distribution}
\description{
This function computes the probability density function
of the Gamma distribution given parameters (\eqn{\alpha}, shape, and \eqn{\beta}, scale)  computed by \code{\link{pargam}}.  The probability density function has no explicit form, but is expressed as an integral
\deqn{f(x) = \frac{1}{\beta^\alpha\,\Gamma(\alpha)}\, x^{\alpha - 1}\, \mathrm{exp}(-x/\beta) \mbox{,}}
where \eqn{f(x)} is the probability density for the quantile \eqn{x}, \eqn{\alpha} is a shape parameter, and \eqn{\beta} is a scale parameter.
}
\usage{
pdfgam(x, para)
}
\arguments{
  \item{x}{A real value vector.}
  \item{para}{The parameters from \code{\link{pargam}} or \code{\link{vec2par}}.}
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
\author{ W.H. Asquith}
\seealso{\code{\link{cdfgam}}, \code{\link{quagam}}, \code{\link{lmomgam}}, \code{\link{pargam}}}
\examples{
  lmr <- lmoms(c(123,34,4,654,37,78))
  gam <- pargam(lmr)
  x <- quagam(0.5,gam)
  pdfgam(x,gam)
}
\keyword{distribution}
\keyword{probability density function}
\keyword{Distribution: Gamma}
