\name{quasla}
\alias{quasla}
\title{Quantile Function of the Slash Distribution}
\description{
This function computes the quantiles of the Slash distribution given
parameters (\eqn{\xi} and \eqn{\alpha}) of the distribution provided by
\code{\link{vec2par}}. The quantile function of the distribution \eqn{x(F; \xi, \alpha)} for nonexceedance probability \eqn{F} and where
\eqn{\xi} is a location parameter and \eqn{\alpha} is a scale parameter requires numerical optimization of the cumulative distribution function.
}
\usage{
quasla(f, para, paracheck=TRUE)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{parsla}} or \code{\link{vec2par}}.}
  \item{paracheck}{A logical controlling whether the parameters and checked for validity. Overriding of this check might be extremely important and needed for use of the distribution quantile function in the context of TL-moments with nonzero trimming.}
}
\value{
  Quantile value for for nonexceedance probability \eqn{F}.
}
\references{
Rogers, W.H., and Tukey, J.W., 1972, Understanding some long-tailed symmetrical distributions: Statistica Neerlandica, v. 26, no. 3, pp. 211-226.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfsla}}, \code{\link{parsla}}, \code{\link{vec2par}}}
\examples{
  para <- c(12,1.2)
  quasla(.55,vec2par(para,type='sla'))
}
\keyword{distribution}
\keyword{quantile function}
