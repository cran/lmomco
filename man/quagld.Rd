\name{quagld}
\alias{quagld}
\title{Quantile Function of the Generalized Lambda Distribution}
\description{
This function computes the quantiles of the Generalized Lambda distribution given
parameters (\eqn{\xi}, \eqn{\alpha}, \eqn{\kappa}, and \eqn{h})
of the distribution computed by \code{\link{vec2par}}.
The quantile function of the distribution is

\deqn{x(F) = \xi + \alpha(F^{\kappa} - (1-F)^{h}) \mbox{,}}{
      x(F) = x + a*(F^k-(1-F)^h)}

where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale
parameter, and \eqn{\kappa}, and \eqn{h} are
shape parameters. Note that in this parameterization the scale term is shown
in the numerator and not the denominator. This is done in this package as
part of the parallel nature between distributions. The scale parameter is often
shown have same units and the location parameter.
}
\usage{
quagld(f, gldpara, paracheck=TRUE)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{gldpara}{The parameters from \code{\link{vec2par}} or similar.}
  \item{paracheck}{A logical controlling whether the parameters and checked for validity. Overriding of this check might be extremely important and needed for use of the distribution quantile function in the context of TL-moments with nonzero trimming.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\references{
Karian, Z.A., and Dudewicz, E.J., 2000, Fitting statistical distributions---The generalized lambda distribution and generalized bootstrap methods:
CRC Press, Boca Raton, FL, 438 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfgld}}, \code{\link{pargld}}, \code{\link{lmomgld}}, \code{\link{lmomTLgld}}, \cr
\code{\link{pargld}}, \code{\link{parTLgld}}
}
\examples{
  para <- vec2par(c(123,34,4,3),type="gld")
  quagld(0.5,para, paracheck=FALSE)
}
\keyword{distribution}
