\name{cdfgld}
\alias{cdfgld}
\title{Cumulative Distribution Function of the Generalized Lambda Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability of the Generalized Lambda distribution given parameters (\eqn{\xi}, \eqn{\alpha}, \eqn{\kappa}, and \eqn{h}) of the distribution computed by \code{\link{pargld}} or similar. The cumulative distribution function of the distribution has no explicit form. The R function \code{uniroot} is used to root the quantile function \code{\link{quagld}} to compute the nonexceedance probability.
}
\usage{
cdfgld(x, gldpara)
}
\arguments{
  \item{x}{A real value.}
  \item{gldpara}{The parameters from \code{\link{pargld}} or similar.}
}

\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
}
\references{
Karian, Z.A., and Dudewicz, E.J., 2000, Fitting statistical distributions---The generalized lambda distribution and generalized bootstrap methods: 
CRC Press, Boca Raton, FL, 438 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{quagld}}, \code{\link{lmomgld}}, \code{\link{pargld}}}
\examples{
  P <- vec2par(c(123,340,0.4,0.654),type='gld')
  cdfgld(300,P)
}
\keyword{distribution}
