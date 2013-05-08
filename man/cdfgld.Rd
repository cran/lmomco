\name{cdfgld}
\alias{cdfgld}
\title{Cumulative Distribution Function of the Generalized Lambda Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability of the Generalized Lambda distribution given parameters (\eqn{\xi}, \eqn{\alpha}, \eqn{\kappa}, and \eqn{h}) of the distribution computed by \code{\link{pargld}} or similar. The cumulative distribution function of the distribution has no explicit form. The R function \code{uniroot} is used to root the quantile function \code{\link{quagld}} to compute the nonexceedance probability. The function returns 0 or 1 if the \code{x} argument is at or beyond the limits of the distribution as specified by the parameters.
}
\usage{
cdfgld(x, gldpara, paracheck)
}
\arguments{
  \item{x}{A real value.}
  \item{gldpara}{The parameters from \code{\link{pargld}} or similar.}
  \item{paracheck}{A logical switch as to whether the validity of the parameters should be checked. Default is \code{paracheck=TRUE}. This switch is made so that the root solution needed for \code{\link{cdfgld}} exhibits an extreme speed increase because of the repeated calls to \code{quagld}.}
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

\dontrun{
  P <- vec2par(c(123,340,0.4,0.654),type='gld')
  cdfgld(300,P, paracheck=FALSE)
}
}
\keyword{distribution}
