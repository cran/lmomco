\name{pdfnor}
\alias{pdfnor}
\title{Probability Density Function of the Normal Distribution}
\description{
This function computes the probability density function
of the Normal distribution given parameters of the distribution computed
by \code{\link{parnor}}.  The probability density function of the distribution is

\deqn{f(x) = \frac{1}{\sigma \sqrt{2\pi}} e^{-(x-\mu)^2/(2\sigma^2)} \mbox{,}}

where \eqn{f(x)} is the probability density for quantile \eqn{x},
\eqn{\mu} is the arithmetic mean, and \eqn{\sigma} is the standard deviation, and
\eqn{\Phi} is the cumulative distribution function of the standard normal
distribution. The R-function \code{pnorm} is used.
}
\usage{
pdfnor(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parnor}} or similar.}
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
\seealso{\code{\link{cdfnor}}, \code{\link{quanor}}, \code{\link{parnor}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  pdfnor(50,parnor(lmr))
}
\keyword{distribution}
\keyword{probability density function}
