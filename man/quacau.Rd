\name{quacau}
\alias{quacau}
\title{Quantile Function of the Cauchy Distribution}
\description{
This function computes the quantiles of the Cauchy distribution given
parameters (\eqn{\xi} and \eqn{\alpha}) of the distribution provided by
\code{\link{vec2par}}. The quantile function of the distribution is

\deqn{x(F) = \xi + \alpha \times atan(\pi*(F-0.5))}

where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F},
\eqn{\xi} is a location parameter and \eqn{\alpha} is a scale parameter.
R supports the quantile function of the Cauchy distribution through
\code{qcauchy}. This function does not use \code{qcauchy} because
\code{qcauchy} does not return \code{Inf} for \eqn{F = 1} although it
returns \code{-Inf} for \eqn{F = 0}.
}
\usage{
quacau(f, para)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{vec2par}} or similar.}
}
\value{
  Quantile value for for nonexceedance probability \eqn{F}.
}
\references{
Gilchirst, W.G., 2000, Statistical modeling with quantile functions: 
Chapman and Hall/CRC, Boca Raton, FL.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfcau}}, \code{\link{vec2par}}}
\examples{
  para <- c(12,12)
  quacau(.5,vec2par(para,type='cau'))
}
\keyword{distribution}
