\name{pdfln3}
\alias{pdfln3}
\title{Probability Density Function of the 3-Parameter Log-Normal Distribution}
\description{
This function computes the probability density
of the Log-Normal3 distribution given parameters (\eqn{\zeta}, \eqn{\mu},
and \eqn{\sigma}) of the distribution computed by \code{\link{parln3}}.
The probability density function function of the distribution is

\deqn{f(x) = \frac{exp{\kappa y - y^2/2}}{\alpha \sqrt{2\pi}} \mbox{,} }
where \eqn{\Phi} is the cumulative ditribution function of the
standard normal distribution and \eqn{y} is
}
\usage{
pdfln3(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parln3}} or similar.}
}

\value{
  Probability density (\eqn{f}) for \eqn{x}.
}
\references{
NEED
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfln3}}, \code{\link{qualn3}}, \code{\link{parln3}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  ln3 <- parln3(lmr)
  x <- qualn3(0.5,ln3)
  pdfgno(x,ln3)
}
\keyword{distribution}
