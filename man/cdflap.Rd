\name{cdflap}
\alias{cdflap}
\title{Cumulative Distribution Function of the Laplace Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability of the Laplace distribution given parameters (\eqn{\xi} and \eqn{\alpha}) of the distribution computed by \code{\link{parlap}}. The cumulative distribution function of the distribution is

\deqn{F(x) = \frac{1}{2} e^{(x-\xi)/\alpha} \mbox{ for } x \le \xi \mbox{, and}}

\deqn{F(x) = 1 - \frac{1}{2} e^{-(x-\xi)/\alpha}  \mbox{ for } x > \xi \mbox{,}}

where \eqn{F(x)} is the nonexceedance probability for quantile \eqn{x},
\eqn{\xi} is a location parameter and \eqn{\alpha} is a scale parameter.
}
\usage{
cdflap(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parlap}} or similar.}
}

\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
}
\references{
Hosking, J.R.M., 1986, The theory of probability weighted moments: IBM Research Report RC12210, T.J. Watson Research Center, Yorktown Heights, New York.
}
\author{ W.H. Asquith}
\seealso{\code{\link{qualap}}, \code{\link{parlap}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  cdflap(50,parlap(lmr))
}
\keyword{distribution}
\keyword{cumulative distribution function}
