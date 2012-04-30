\name{pdflap}
\alias{pdflap}
\title{Probability Density Function of the Laplace Distribution}
\description{
This function computes the probability density of the Laplace distribution given parameters (\eqn{\xi} and \eqn{\alpha}) of the distribution computed by \code{\link{parlap}}. The probability density function of the distribution is

\deqn{f(x) = \frac{1}{2}\alpha^{-1} e^{-|x - \xi|/\alpha} \mbox{.}}
}
\usage{
pdflap(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parlap}} or similar.}
}

\value{
  Probability density (\eqn{f}) for \eqn{x}.
}
\references{
Hosking, J.R.M., 1986, The theory of probability weighted moments: IBM Research Report RC12210, T.J. Watson Research Center, Yorktown Heights, New York.
}
\author{ W.H. Asquith}
\seealso{\code{\link{pdflap}}, \code{\link{qualap}}, \code{\link{parlap}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  lap <- parlap(lmr)
  x <- qualap(0.5,lap)
  pdflap(x,lap)
}
\keyword{distribution}
