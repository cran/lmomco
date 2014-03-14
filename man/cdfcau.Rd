\name{cdfcau}
\alias{cdfcau}
\title{Cumulative Distribution Function of the Cauchy Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Cauchy distribution given parameters (\eqn{\xi} and \eqn{\alpha}) of the
distribution provided by \code{\link{parcau}} or \code{\link{vec2par}}.  The cumulative distribution
function of the distribution is

\deqn{F(x) = \frac{\arctan(\frac{x - \xi}{\alpha})}{\pi}+0.5 \mbox{,}}

where \eqn{F(x)} is the nonexceedance probability for quantile \eqn{x},
\eqn{\xi} is a location parameter and \eqn{\alpha} is a scale parameter.
}
\usage{
cdfcau(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parcau}} or \code{\link{vec2par}}.}
}

\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
}
\references{
Elamir, E.A.H., and Seheult, A.H., 2003, Trimmed L-moments: Computational Statistics
and Data Analysis, vol. 43, pp. 299--314.

Gilchrist, W.G., 2000, Statistical modeling with quantile functions:
Chapman and Hall/CRC, Boca Raton, FL.
}
\author{ W.H. Asquith}
\seealso{\code{\link{quacau}}, \code{\link{parcau}}, \code{\link{vec2par}}}
\examples{
  para <- c(12,12)
  cdfcau(50,vec2par(para,type='cau'))
}
\keyword{distribution}
\keyword{cumulative distribution function}
