\name{quarice}
\alias{quarice}
\title{Quantile Function of the Rice Distribution}
\description{
This function computes the quantiles of the Rice distribution given
parameters (\eqn{\alpha} and \eqn{\nu}) of the distribution computed by
\code{\link{parrice}}.
The quantile function of the distribution is

where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F},
\eqn{\alpha} is a parameter, and \eqn{\nu} is a parameter.
}
\usage{
quarice(f, para, xmax=NULL)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{parrice}} or similar.}
  \item{xmax}{The maximum x value used for integeration.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfrice}}, \code{\link{parrice}}}
\examples{
#  lmr <- lmom.ub(c(123,34,4,654,37,78))
#  quarice(0.5,parrice(lmr))
}
\keyword{distribution}
