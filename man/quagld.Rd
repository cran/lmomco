\name{quagld}
\alias{quagld}
\title{Quantile Function of the Generalized Lambda Distribution}
\description{
This function computes the quantiles of the Generalized Lambda distribution given
parameters (\eqn{\Lambda_1}, \eqn{\Lambda_2}, \eqn{\Lambda_3}, and \eqn{\Lambda_4})
of the distribution computed by \code{\link{vec2par}}.
The quantile function of the distribution is

\deqn{x(F) = \Lambda_1 + \Lambda_2(F^{\Lambda_3} - (1-F)^{\Lambda_4})}{
      x(F) = Lambda_1 + Lambda_2*(F^Lambda_3-(1-F)^Lambda_4)}

where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F},
\eqn{\Lambda_1} is a location parameter, \eqn{\Lambda_2} is a scale
parameter, and \eqn{\Lambda_3}, and \eqn{\Lambda_4} are
shape parameters. Note that in this parameterization the scale term is shown
in the numerator and not the denominator. This is done in this package as 
part of the parallel nature between distributions. The scale parameter is often
shown have same units and the location parameter.
}
\usage{
quagld(f, gldpara, paracheck)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{gldpara}{The parameters from \code{\link{vec2par}} or similar.}
  \item{paracheck}{A logical switch as to whether the validity of the parameters should be checked. Default is \code{paracheck=TRUE}. This switch is made so that the root solution needed for \code{\link{cdfgld}} exhibits an extreme speed increase because of the repeated calls to \code{quagld}.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\references{
Karian, Z.A., and Dudewicz, E.J., 2000, Fitting statistical distributions--The generalized lambda distribution and generalized bootstrap methods: 
CRC Press, Boca Raton, FL, 438 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfgld}}, \code{\link{pargld}}, \code{\link{lmomgld}}, \code{\link{lmomTLgld}}, \cr
\code{\link{pargld}}, \code{\link{parTLgld}}
}
\examples{
  para <- vec2par(c(123,34,4,3),type="gld")
  quagld(0.5,para)
}
\keyword{distribution}
