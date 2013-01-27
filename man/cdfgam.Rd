\name{cdfgam}
\alias{cdfgam}
\title{Cumulative Distribution Function of the Gamma Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Gamma distribution given parameters (\eqn{\alpha} and \eqn{\beta}) of the
distribution computed by \code{\link{pargam}}.  The cumulative distribution
function of the distribution has no explicit form, but is expressed as an integral.

\deqn{F(x) = \frac{\beta^{-\alpha}}{\Gamma(\alpha)}\int_0^x t^{\alpha - 1}
e^{-t/\beta} \mbox{d}F \mbox{,}}

where \eqn{F(x)} is the nonexceedance probability for the quantile \eqn{x}. The parameters have the following interpretation in the \pkg{R} syntax; \eqn{\alpha} is a shape parameter and \eqn{\beta} is a scale parameter.
}
\usage{
cdfgam(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{pargam}} or similar.}
}
\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
}
\references{
Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105--124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{quagam}}, \code{\link{pargam}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  cdfgam(50,pargam(lmr))

  # A manual demonstration of a gamma parent
  G  <- vec2par(c(0.6333,1.579),type='gam') # the parent
  F1 <- 0.25         # nonexceedance probability
  x  <- quagam(F1,G) # the lower quartile (F=0.25)
  a  <- 0.6333       # gamma parameter
  b  <- 1.579        # gamma parameter
  # compute the integral
  xf <- function(t,A,B) { t^(A-1)*exp(-t/B) }
  Q  <- integrate(xf,0,x,A=a,B=b)
  # finish the math
  F2 <- Q$val*b^(-a)/gamma(a)
  # check the result
  if(abs(F1-F2) < 1e-8) print("yes")
}
\keyword{distribution}
\keyword{cumulative distribution function}
