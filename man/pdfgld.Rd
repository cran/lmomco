\name{pdfgld}
\alias{pdfgld}
\title{Probability Density Function of the Generalized Lambda Distribution}
\description{
This function computes the probability density function of the Generalized Lambda distribution given parameters (\eqn{\xi}, \eqn{\alpha}, \eqn{\kappa}, and \eqn{h}) of the distribution computed by \code{\link{pargld}} or similar. The probability density function is

\deqn{f(x) = {[(\kappa(F^{\kappa-1}) + h(1-F)^{h-1})\times\alpha]}^{-1} \mbox{at\ } x = Q(F) }

where \eqn{f(x)} is the probability density function at \eqn{x}, \eqn{Q(F)} is the quantile function for nonexceedance probability \eqn{F}.
}
\usage{
pdfgld(x, gldpara, paracheck)
}
\arguments{
  \item{x}{A real value.}
  \item{gldpara}{The parameters from \code{\link{pargld}} or similar.}
  \item{paracheck}{A logical switch as to whether the validity of the parameters should be checked. Default is \code{paracheck=TRUE}. This switch is made so that the root solution needed for \code{\link{cdfgld}} exhibits an extreme speed increase because of the repeated calls to \code{quagld}.}
}
\value{
  Probability density (\eqn{f}) for \eqn{x}.
}
\references{
Karian, Z.A., and Dudewicz, E.J., 2000, Fitting statistical distributions---The generalized lambda distribution and generalized bootstrap methods: CRC Press, Boca Raton, FL, 438 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfgld}}, \code{\link{quagld}}, \code{\link{lmomgld}}, \code{\link{pargld}}}
\examples{
# Using Karian and Dudewicz, 2000, p. 10
gld <- vec2par(c(0.0305,1/1.3673,0.004581,0.01020),type='gld')
quagld(0.25,gld) # which equals about 0.028013 as reported by K&D
pdfgld(0.028013,gld) # which equals about 43.04 as reported by K&D
F <- seq(.001,.999,by=.001)
x <- quagld(F,gld)
#plot(x,pdfgld(x,gld),type='l',xlim=c(0,.1))
}
\keyword{distribution}
