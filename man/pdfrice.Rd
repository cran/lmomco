\name{pdfrice}
\alias{pdfrice}
\title{Probability Density Function of the Rice Distribution}
\description{
This function computes the probability density
of the Rice distribution given parameters (\eqn{\alpha} and \eqn{\nu}) of the distribution computed by \code{\link{parrice}}. The probability density function of the distribution is


where \eqn{f(x)} is the nonexceedance probability for quantile \eqn{x},
\eqn{\alpha} is a parameter and \eqn{\nu} is a parameter.
}
\usage{
pdfrice(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parrice}} or similar.}
}

\value{
  Probability density (\eqn{f}) for \eqn{x}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfrice}}, \code{\link{quarice}}, \code{\link{parrice}}}
\examples{
#  lmr <- lmom.ub(c(123,34,4,654,37,78))
#  rice <- parrice(lmr)
#  x <- quarice(0.5,ray)
#  pdfrice(x,ray)
}
\keyword{distribution}
