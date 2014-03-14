\name{cdfsla}
\alias{cdfsla}
\title{Cumulative Distribution Function of the Slash Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Slash distribution given parameters (\eqn{\xi} and \eqn{\alpha}) of the
distribution provided by \code{\link{parsla}} or \code{\link{vec2par}}.  The cumulative distribution function of the distribution is

\deqn{F(x) = \Phi(y) - [\phi(0) - \phi(y)]/y \mbox{,}}

for \eqn{y \ne 0} and

\deqn{F(x) = 1/2 \mbox{,}}

for \eqn{y = 0} and where \eqn{f(x)} is the probability density for quantile \eqn{x}, \eqn{y = (x - \xi)/\alpha}, \eqn{\xi} is a location parameter, and \eqn{\alpha} is a scale parameter. The function \eqn{\Phi(y)} is the cumulative distribution function of the standard normal distribution function, and \eqn{\phi(y)} is the probability density function of the standard normal distribution.
}
\usage{
cdfsla(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parsla}} or \code{\link{vec2par}}.}
}

\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
}
\references{
Rogers, W.H., and Tukey, J.W., 1972, Understanding some long-tailed symmetrical distributions: Statistica Neerlandica, v. 26, no. 3, pp. 211-226.
}
\author{ W.H. Asquith}
\seealso{\code{\link{quasla}}, \code{\link{parsla}}, \code{\link{vec2par}}}
\examples{
  para <- c(12,1.2)
  cdfsla(50,vec2par(para,type='sla'))
}
\keyword{distribution}
\keyword{cumulative distribution function}
