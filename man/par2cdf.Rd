\name{par2cdf}
\alias{par2cdf}
\title{Cumulative Distribution Function of the Distributions}
\description{
This function acts as a front end of dispatcher to the distribution-specific cumulative
distribution functions.
}
\usage{
par2cdf(x,para,...)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{lmom2par}} or similar.}
  \item{...}{The additional arguments are passed to the cumulative distribution function such as \code{paracheck=FALSE} for the generalized Lambda distribution (\code{\link{cdfgld}}).}
}
\value{
  Nonexceedance probability (\eqn{0 \le F \le 1}) for \code{x}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{par2qua}}, \code{\link{lmom2par}}}
\examples{
lmr       <- lmom.ub(rnorm(20))
para      <- parnor(lmr)
nonexceed <- par2cdf(0,para)
}
\keyword{distribution}
