\name{cdfrice}
\alias{cdfrice}
\title{Cumulative Distribution Function of the Rice Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Rice distribution given parameters (\eqn{\alpha} and \eqn{\nu}) of the distribution computed
by \code{\link{parrice}}. The cumulative distribution function of the distribution is

where \eqn{F(x)} is the nonexceedance probability for quantile \eqn{x}, \eqn{\alpha} is a parameter, and \eqn{\nu} is a parameter.
}
\usage{
cdfrice(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parrice}} or similar.}
}
\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{quarice}}, \code{\link{parrice}}}
\examples{
#  lmr <- lmom.ub(c(123,34,4,654,37,78))
#  cdfrice(50,parrice(lmr))
}
\keyword{distribution}
