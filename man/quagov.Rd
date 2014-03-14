\name{quagov}
\alias{quagov}
\title{Quantile Function of the Govindarajulu Distribution}
\description{
This function computes the quantiles of the Govindarajulu distribution given
parameters (\eqn{\xi}, \eqn{\alpha}, and \eqn{\beta}) of the distribution computed by
\code{\link{pargov}}. The quantile function of the distribution is

\deqn{x(F) = \xi + \alpha[(\beta+1)F^\beta - \beta F^{\beta+1}] \mbox{,}}

where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F}, \eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter, and \eqn{\beta} is a shape parameter.
}
\usage{
quagov(f, para, paracheck=TRUE)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{pargov}} or similar.}
  \item{paracheck}{A logical controlling whether the parameters and checked for validity. Overriding of this check might be extremely important and needed for use of the distribution quantile function in the context of TL-moments with nonzero trimming.}
}
\value{
  Quantile value for for nonexceedance probability \eqn{F}.
}
\references{
Gilchrist, W.G., 2000, Statistical modelling with quantile functions: Chapman and Hall/CRC, Boca Raton.

Nair, N.U., Sankaran, P.G., Balakrishnan, N., 2013, Quantile-based reliability analysis: Springer, New York.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfgov}}, , \code{\link{pdfgov}}, \code{\link{pargov}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  quagov(0.5,pargov(lmr))
}
\keyword{distribution}
\keyword{quantile function}
