\name{pdfray}
\alias{pdfray}
\title{Probability Density Function of the Rayleigh Distribution}
\description{
This function computes the probability density
of the Rayleigh distribution given parameters (\eqn{\xi} and \eqn{\alpha}) of the distribution computed
by \code{\link{parray}}. The probability density function of the distribution is

\deqn{f(x) =  \frac{x - \xi}{\alpha^2}e^{-(x - \xi)^2/(2\alpha^2)}\mbox{,}}

where \eqn{f(x)} is the nonexceedance probability for quantile \eqn{x},
\eqn{\xi} is a location parameter, and \eqn{\alpha} is a scale parameter.
}
\usage{
pdfray(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parray}} or similar.}
}

\value{
  Probability density (\eqn{f}) for \eqn{x}.
}
\references{
Hosking, J.R.M., 1986, The theory of probability weighted moments:
Research Report RC12210, IBM Research Division, Yorkton Heights, N.Y.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfray}}, \code{\link{quaray}}, \code{\link{parray}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  ray <- parray(lmr)
  x <- quaray(0.5,ray)
  pdfray(x,ray)
}
\keyword{distribution}
