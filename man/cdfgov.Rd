\name{cdfgov}
\alias{cdfgov}
\title{Cumulative Distribution Function of the Govindarajulu Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Govindarajulu distribution given parameters (\eqn{\xi}, \eqn{\alpha},
and \eqn{\beta}) of the distribution computed by \code{\link{pargov}}. The cumulative
distribution function requires numerical methods.
}
\usage{
cdfgov(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{pargov}} or similar.}
}

\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
}
\references{
Gilchrist, W.G., 2000, Statistical modelling with quantile functions: Chapman and Hall/CRC, Boca Raton.

Nair, N.U., Sankaran, P.G., Balakrishnan, N., 2013, Quantile-based reliability analysis: Springer, New York.
}
\author{ W.H. Asquith}
\seealso{\code{\link{pdfgov}}, \code{\link{quagov}}, \code{\link{pargov}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  cdfgov(50,pargov(lmr))
}
\keyword{distribution}
\keyword{cumulative distribution function}
